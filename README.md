# bitfire

Bitfire is a fixed-interleave loader system for C64 with a depacker, a basic framework and an image-writing tool.

# the techniques behind it

## disk layout

The sectors on a foppy disk have 256 bytes of payload. Files consist of a chain of sectors on disk that are linked together. The first two bytes of a sector's payload are used to either point to the next track/sector or give the size of the last sector being used.
This means, that the de-facto payload per sector is 254 bytes, an awful number to calculate with easily. Also, the last sector of a file leaves quite a few bytes unused, as files always start on a new sector at position 0 within the sector.

When assuming a fixed interleave for the blocks the file is located on, and by knowing the filsizes beforehand, in fact each single byte on a disk can be used for files, and files can be loaded in chunks of 256 bytes, which simplifies a few things in a loader.
So this means, with an interleave of 4, the sectors 0, 4, 8, 12, 16, 20 are used, on wrap around, 1, 5, 9, ... and so on. When the filesize is known beforehand, the size of the last sector is also known.
Now it is possible to glue each file after another without any gaps in between. This means, however, that a new dir-layout is needed, as the native layout can not compensate for such design changes. For that purpose, 2 sectors on track 18 are spent with 63 entries each, while the native directory still exists for dir-layout, standard files and the bootloader.

The bitfire dir entries only contain the load-address and the filesize and are accessed by number, which is more than sufficient for a demo. Most of the time these will load sequentially from disk. Random file-access is nevertheless still possible.
By walking through the file list and adding up filesizes, the right track/sector and offset within a sector can be determined. Each dir-sector has a track, sector and block index it starts at. That is why only 63 entries are located on it, as that information is also stored in the dir-sector. The final remaining byte is used to indicate the disk-side.

The layout of a dir-sector:

pos | content
--- | -------
$00 | load-address lowbyte
$01 | load-address hibyte
$02 | filesize lowbyte
$03 | filesize hibyte
... | ... repeated 63 times ...
$fc | track where first file starts
$fd | index to sector first file starts
$fe | position in sector first file starts
$fe | disk-side info

## GCR-loop

The data on disk is GCR-enocded, so every nibble of a byte is represented by a quintuple on disk. To make the decoding fast enough, tables are used to look up data-portions for low- and high-nibbles that are finally merged together to form the original bytes. This happens so fast that the checksumming of a sector is also done on the fly. The tables also have further advantages, as bits can be swapped, so they are better suited to serial transfer later on. However, debugging or sending variable values over the serial bus are more complicated then, as they need to be scrambled/descrambled beforehand to the needed bit order.

So 8 nibbles are represented like this on disc and stored in 8 quintuples on disc:

```
11111222 22333334 44445555 56666677 77788888
```

Things are shifted and masked together, so that the read values represent the following orders:

```
11111000
02200222
00333330
44444000
00005555
05666660        note: the last bit of 5 is added with the 6th nibble
0700dd77	note: bit 0, 2 can be formed to one partition, this are the remaining bits 1, 3, 4
7d788888	note: the resulting table is big, but this saves masking and decodes 7 bits at once!
```

As one can see, the quintuples can be partitioned in some way, but in the gcr encoding choosen this is very restricted. Bit 2 even reflects the same bit in the gcr and raw data, bit 0 can also be clipped of and added again with ora, adc or eor. So a quintuple can be partitoned like this:
```
43210 -> 4321.  +  ....0
43210 -> 43.10  +  ..2..
43210 -> 43.1.  +  ..2.0
```

Sadly, no partitioning is possible in those ways (only if you change the mapping for the gcr-codes):
```
43210 -> 432..  +  ...10
43210 -> 43...  +  ..210
43210 -> 4....  +  .3210
```

The tables can be arranged in a way (with offsets) that many of them fit together into one area to save space. One table is located in the zeropage, the gaps are used for variables and lists for the loader. Another table is located @ $0600 where the gaps are filled with code. The 0700dd77-table is located @ $0200, as it is very small, it can be easily interleaved into the code. If using a more strict mask on the seventh table or also other tables, the size could be reduced even more, but al this costs cycles in the gcr-loop, resulting in a less tolerant behaviour.
Read bytes are directly stored on the stack via PHA, as this only needs 3 cycles for a written byte. This however makes it impossible to use the stack for most of the code, so most of all JSR calls need to be avoided as they would destroy data on the stack.

## salvador / zx0

The original [zx0 packer](https://github.com/einar-saukas/ZX0) is done by Einar Saukas (thanks for the many mails and discussions and exchange of ideas!) and yields a really good pack-ratio and depacking speed!
Meanwhile also [salvador](https://github.com/emmanuel-marty/salvador) emerged that features very fast compression speeds compared to zx0 and yields results that are nearly as good, while being compatible with zx0, regarding the output format. I added a recoder named dali under packer/dali that wraps salvador and outputs compressed files in a bitfire compatible format, as i changed a few things on the encoding to speed up things and shrink the depacker-code. This wrapper also does all the c64-specific things you are used to from bitfire, like handling load-addresses, relocation, cutting and a sfx. It substitutes the modified zx0 that also comes along with bitfire so far. The new depacker is not compatible with the modified zx0 anymore, so dali is the new way to go.

The most notable changes in the encoding are the drop of the xor 0xff on the LSB of the offset, so offsets are now subtracted and not added. I also changed the order of MSB and LSB bits, sent via the interlaced elias gamma encoding. Lengths greater than 8 bit happen very rarely, and thus an extra 8 bit loop is done first and only extended to 16 bit if necessary. This speeds up bitfetching in 6502 quite a bit. To save upon the setup of .lz_bits i changed direction of the bitbuffer, it is now shifted right.

In-place depacking was also added, so that files can be directly loaded and depacked within the memory range the unpacked data would land in anyway. So no more safety-margins, deltas, overlap or however you name it, needs to be taken care of, except, if you depack out of another location. To achieve that, the end-marker is clamped off, and encoding is interupted as soon as a match or literal would overwrite the still packed data. From there on the data is output as plain unencoded literal. It is there in memory, where it belongs anyway.

Besides the adopted version there's also a compatible version for the original zx0 format used by the original zx0 and salvador. It can be found [here](https://github.com/bboxy/bitfire/tree/master/packer/zx0/6502) for either versions, v1 and v2.

## future work

The tables needed for decoding could be shrunk in several ways. One way to do so is more precise masking and not adding any bits we don't care about. By swapping nibbles (3 and 4), the 44444000_lo table could be replaced by the 11111000_hi table. The eor-checksum for the native d64-format would then fail, however. The tables 05666660_lo and 7d788888_lo could be used in the same way if an additional bit would be added or the 7d788888 table is shifted one to the left. So far this means extra code to achieve smaller tables, and this has not been implemented but there's quite some potential left.
