BITNAX_OPTS = -v --bitfire
D64WRITE_OPTS = -v

all: installer

installer: checksum.asm Makefile
	acme -f cbm -o $@ $<
	../packer/zx0/zx0 -f --sfx 0x0800 -o $@.prg $@
	cd files; rm *.lz; for i in *; do ../../packer/zx0/zx0 -f -o $$i.lz $$i; done
	../d64write/d64write $(D64WRITE_OPTS) -c side1.d64 -h oxyron -i \<3 --side 1 \
			--boot $@.prg \
			-b files/pic1.bin.lz \
			-b files/pic2.bin.lz \
			-b files/pic1.bin \
			-b files/pic2.bin

	rm $@ $@.prg
	x64 side1.d64

clean:
	-rm side1.d64 side2.d64 installer installer.prg
	-rm files/*.lz
