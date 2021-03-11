# bitfire
Bitfire is a fixed interleave loadersystem for c64 with depacker, a basic framework and an image writing tool.

# the technics behind



Supported drives: all 1541 compatible
Protocoll: 2bit ATN, with second drives on bus silenced


#72 cycle loop
gcr on the fly + checksum
explain the tables
#zx0 bei Einar Saukas, link to his site
changes: no eor #$ff on offset
swap MSB/LSB to little endian on interlaced elias gamma
explain interlaced elias gamma?

inplace depacking also explain?
