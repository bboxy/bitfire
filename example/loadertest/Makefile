BITNAX_OPTS = -v --bitfire
D64WRITE_OPTS = -v
ZX0 = ../packer/zx0/zx0
D64WRITE = ../d64write/d64write

all: example

example: example.asm Makefile ../loader/installer files/pic1.bin.lz files/pic2.bin.lz
	acme -f cbm -o $@ $<
	$(ZX0) -f --sfx 0x0800 -o $@.prg $@
	$(D64WRITE) $(D64WRITE_OPTS) -c side1.d64 -h performers -i \<3 --side 1 \
			--boot $@.prg \
			-b files/pic1.bin.lz \
			-b files/pic2.bin.lz \
			-b files/pic1.bin \
			-b files/pic2.bin

	rm $@ $@.prg
	x64 side1.d64

files/pic1.bin.lz: files/pic1.bin
	$(ZX0) -f -o $@ $<

files/pic2.bin.lz: files/pic2.bin
	$(ZX0) -f -o $@ $<

clean:
	-rm side1.d64 example example.prg
	-rm files/*.lz
