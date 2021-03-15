# bitfire

Bitfire is a fixed interleave loadersystem for c64 with depacker, a basic framework and an image writing tool.

# the technics behind

## disc layout

The sectors on a foppy disc have 256 bytes of payload. Files consist of a chain of sectors on disc that are linked together, The first two bytes of a sectors payload are used to either point to the next track/sector or give the size of the last sector being used.
This means, that the realy data payload per sector is 254 bytes, an awful number to calculate with easy. Also, the last sector leaves quite a few bytes unused.
When assuming a fixed interleave for the blocks the file is spread overm and by knowing the filsizes beforehand, in fact each single byte on a disc can be used for files, and files can be loaded in full 256 bytes large chunks, what simplifies a few things in a loader.
So this means, with an interleave of 4, the sectors 0, 4, 8, 12, 16, 29 are used, on wrapp around, 1, 5, 9, ... and so on. When the filesize is known beforehand, the size of the last sector is also known.                                                                    Now it is possible to glue each file after another without any gaps in between. This means however a new dir-layout. For that purpose 2 sectors on track 18 are spent with 63 entries each.
The dir entries only contain the load-address and the filesize and are accessed by number, what is more than sufficient for a demo, that most of the time will load sequentially from disc.
By walking through the file list and adding up filesizes, the right track/sector and offset within a sector can be determined. Each dir-sector has a track, sector and bock index it starts at. That is, why only 63 entries are located on it. The final remaining byte is used
 to indicate the disc-side.

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
$fe | position in sector first fle starts
$fe | disc-side info

## GCR-loop

The data on disc is GCR-enocded, so every nibble of a byte is represented by a quintuple on disc. To make teh decoding fast enough, tables are used to look up data-portions for low- and high-nibbles that are finally merged together to the originating bytes. that happens t
hat fast now, that also the checksumming of a sector is done on the fly. The tables also have further advantages, as bits can be swapped, so they suit better for the serial transfer later on. However debugging or sending variblae values over the serial bus, are more complicated then, as they need to be scrambled/descrambled beforehand.

So 8 nibbles are represented like this on disc and stored in 8 quintuples on disc

`11111222 22333334 44445555 56666677 77788888`

Things are shifted and masked together, so that the read values represent teh following orders:

`11111000`

`02200222`

`00333330`

`44444000`

`00005555`

`05666660        note: the last bit portion of 5 is added with the 6th nibble`

`77700077`

`00088888`

The tables can be arranged ina away (with offsets) that many of them fit together into one area to save space.
Read bytes are directly stored on the stack wie PHA, as tis only needs 3 cycles for a written byte. This however makes it impossible to use the stack on most of the code, as JSR calls would destryo data on the stack.

## zx0 packer

The original [zx0 packer](https://github.com/einar-saukas/ZX0) is done by Einar Saukas (thanks for the many mails and discussions and exchange of ideas!) and yields a really good pack-ratio. There has not been a 6502 port of the depacker yet, but now there is :-D I made a few adoptions to the encoding to be able to save a bit on code and cycles on the depacker side. I also added all the features that were available with bitnax. It now handles c64-files very comfortably.
Most notably changes are the drop of the xor 0xff on the LSB of the offset, so offsets are now subtracted and not added. Also i changes teh order of MSB and LSB bits, sent via the interlaced elias gamma encoding. Lengths greater than 8 bit happen very rarely, and thus an extra 8 bit loop is done first and only extended to 16 bit if necessary. This speeds up bitfetching in 6502.
Also inplace depacking was added, so that files can be directly loaded and depacked within the memory range the unpacked data would land anyway. So no more safety-margins. deltas, overlap or however you name it has to be taken care of, except, if you depack out of another location. To achieve that, the end-marker is clamped off, and encoding is interupted as soon as a match or literal would overwrite the still packed data, from there on the data is output as plain unencoded literal. It is there in memory, where it belongs anyway.

Besides the adopted version there's also a compatible version for the original zx0, it can be found [here][https://github.com/bboxy/bitfire/tree/master/bitfire/zx0/6502] including the original zx0.
