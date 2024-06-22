BITNAX_OPTS = -v --bitfire
D64WRITE_OPTS = -v
PACKER = ../../packer/dali/dali
PACKER_OPTS =
D64WRITE = ../../d64write/d64write

all: example

testfile.lz: ../../benchmark/files/b
	$(PACKER) -o $@ $<

example: example.asm Makefile ../../loader/installer testfile.lz
	acme -f cbm -o $@ $<
	$(PACKER) $(PACKER_OPTS) --sfx 0x0800 -o $@.prg $@

clean:
	-rm testfile.lz example example.prg
