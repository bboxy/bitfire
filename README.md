# bitfire

Bitfire is a fixed interleave loadersystem for c64 with depacker, a basic framework and an image writing tool.

# the technics behind

## disc layout

The sectors on a foppy disc have 256 bytes of payload. Files consist of a chain of sectors on disc that are linked together, the first two bytes of a sectors payload are used to either point to the next track/sector or give the size of the last sector being used.
This means, that the de facto payload per sector is 254 bytes, an awful number to calculate with easy. Also, the last sector of a file leaves quite a few bytes unused, as files always start on a new sector at position 0 within the sector.

When assuming a fixed interleave for the blocks the file is located on and by knowing the filsizes beforehand, in fact each single byte on a disc can be used for files, and files can be loaded in chunks of 256 bytes, what simplifies a few things in a loader.
So this means, with an interleave of 4, the sectors 0, 4, 8, 12, 16, 20 are used, on wrap around, 1, 5, 9, ... and so on. When the filesize is known beforehand, the size of the last sector is also known.
Now it is possible to glue each file after another without any gaps in between. This means however that a new dir-layout is needed, as the native layout can not compensate such design changes. For that purpose 2 sectors on track 18 are spent with 63 entries each, while the native directory still exists for dir-layout, standard files and the bootloader.

The bitfire dir entries only contain the load-address and the filesize and are accessed by number, what is more than sufficient for a demo, that most of the time will load sequentially from disc. Random file-access is nevertheless still possible by that.
By walking through the file list and adding up filesizes, the right track/sector and offset within a sector can be determined. Each dir-sector has a track, sector and block index it starts at. That is, why only 63 entries are located on it, as that information is also stored on the dir-sector. The final remaining byte is used to indicate the disc-side.

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
$fe | disc-side info

## GCR-loop

The data on disc is GCR-enocded, so every nibble of a byte is represented by a quintuple on disc. To make the decoding fast enough, tables are used to look up data-portions for low- and high-nibbles that are finally merged together to the originating bytes. That happens that fast now, that also the checksumming of a sector is done on the fly. The tables also have further advantages, as bits can be swapped, so they suit better for the serial transfer later on. However debugging or sending variable values over the serial bus are more complicated then, as they need to be scrambled/descrambled beforehand to the needed bitorder.

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
7d788888	note: resulting table is big, but this saves masking and decodes 7 bits at once!
```

As one can see, the quintuples can be partitioned in some way, but in the gcr encoding choosen this is very restricted. Bit 2 even reflects the same bit in the gcr and raw data, bit 0 can also be clipped of and added again with ora, adc or eor. So a quintuple can be partitoned like this:
```
43210 -> 4321.  +  ....0
43210 -> 43.10  +  ..2..
43210 -> 43.1.  +  ..2.0
```

Sadly, there's no partitioning possible in those ways (only if you change the mapping for the gcr-codes):
```
43210 -> 432..  +  ...10
43210 -> 43...  +  ..210
43210 -> 4....  +  .3210
```

The tables can be arranged in a away (with offsets) that many of them fit together into one area to save space. One table is located in the zeropage, the gaps are used for variables and lists for the loader. Another table is located @ $0600 where the gaps are filled with code. The 0700dd77-table is located @ $0200, as it is very small, it can be easily interleaved into the code. If using a more strict mask on the seventh table or also other tables, the size could be reduced even more, but al this costs cycles in the gcr-loop, resulting in a less tolerant behaviour.
Read bytes are directly stored on the stack via PHA, as this only needs 3 cycles for a written byte. This however makes it impossible to use the stack on most of the code, so most of all JSR calls need to be avoided as they would destroy data on the stack.

## zx0 packer / salvador

The original [zx0 packer](https://github.com/einar-saukas/ZX0) is done by Einar Saukas (thanks for the many mails and discussions and exchange of ideas!) and yields a really good pack-ratio and depacking speed! There has not been a 6502 port of the depacker yet, but now there is :-D I made a few adoptions to the encoding to be able to save a bit on code and cycles on the depacker side. I also added all the features that were available with bitnax. It now handles c64-files very comfortably.
Most notably changes in the encoding are the drop of the xor 0xff on the LSB of the offset, so offsets are now subtracted and not added. Also i changed the order of MSB and LSB bits, sent via the interlaced elias gamma encoding. Lengths greater than 8 bit happen very rarely, and thus an extra 8 bit loop is done first and only extended to 16 bit if necessary. This speeds up bitfetching in 6502 quite a bit.
Also inplace depacking was added, so that files can be directly loaded and depacked within the memory range the unpacked data would land anyway. So no more safety-margins. deltas, overlap or however you name it has to be taken care of, except, if you depack out of another location. To achieve that, the end-marker is clamped off, and encoding is interupted as soon as a match or literal would overwrite the still packed data, from there on the data is output as plain unencoded literal. It is there in memory, where it belongs anyway.

Besides the adopted version there's also a compatible version for the original zx0 format, it can be found [here](https://github.com/bboxy/bitfire/tree/master/bitfire/zx0/6502) including the original zx0.

Meanwhile also [salvador](https://github.com/emmanuel-marty/salvador) emerged that features very fast compression speeds compared to zx0 and yields nearly as good results, while being compatible with zx0, regarding the poutput-format. I added a recoder under packer/recoder that wraps either salvador or the original zx0 and outputs compressed files in a bitfire compatible format. This wrapper also does all the c64-specific things, like handling load-addresses, relocation, cutting and a sfx. It substitutes the modified zx0 that also comes along with bitfire.

## future work

The tables needed for decoding could be shrunk in several ways. One mean to do so is more precise masking and not adding dont't care bits. By swapping nibbles (3 and 4), the 44444000_lo table could be replaced by the 11111000_hi table. The eor-checksum for the native d64-format would then however fail. The tables 05666660_lo and 7d788888_lo could be used in the same way if an additional bit would be added or the 7d788888 table is shifted one to the left. So far this means extra code to achieve smaller tables, and this it is not implemented. But there's quite some potential left.
