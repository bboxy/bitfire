D64WRITE = ../d64write/d64write
D64WRITE_OPTS = -v
TIMESTAMP = $(shell date +%Y%m%d-%H%M%S)

ACME = acme
ACME_OPTS = -f cbm

ZX0 = ../packer/zx0/zx0
ZX0_OPTS = -f

ZX0PLAY = ../packer/zx0/zx0
ZX0PLAY_OPTS = -f

REENCODE = ../packer/recoder/dzx0 -x
#REENCODE_OPTS = '../packer/zx0/6502/zx0-original-encoding/src/zx0 -f'
REENCODE_OPTS = '../packer/salvador/salvador'

all: benchmark

benchmark-lz:
	rm files/*.lz; for i in files/*; do $(ZX0PLAY) $(ZX0PLAY_OPTS) -o $$i.lz $$i; done
benchmark-reencode:
	rm files/*.lz; for i in files/*; do $(REENCODE) $(REENCODE_OPTS) -o $$i.lz $$i; done

benchmark: checksum.asm Makefile ../loader/installer
	-rm side*.d64
	-rm installer*.prg
	$(MAKE) -C ../
	$(ACME) $(ACME_OPTS) -o $@ $<
	#exomizer sfx 0x0900 -o $@.prg $@
	$(ZX0) $(ZX0_OPTS) --sfx 0x0900 -o $@.prg $@
	cp benchmark.prg installer$(TIMESTAMP).prg
	$(D64WRITE) $(D64WRITE_OPTS) -c side$(TIMESTAMP).d64 --free 1337 -h oxyron -i \<3 --side 1 \
			--boot $@.prg \
			-b files/a.lz \
			-b files/b.lz \
			-b files/c.lz \
			-b files/d.lz \
			-b files/e.lz \
			-b files/f.lz \
			-b files/g.lz \
			-b files/h.lz \
			-b files/i.lz \
			-b files/j.lz \
			-b files/k.lz \
			-b files/l.lz \
			-b files/m.lz \
			-b files/n.lz \
			-b files/o.lz \
			-b files/p.lz \
			-b files/q.lz \
			-b files/r.lz \
			-b files/b.lz \
			-b files/b.lz \
			-b files/b.lz \
			-b files/e.lz

	x64 side$(TIMESTAMP).d64

clean:
	-rm side*.d64 insstaller*.prg benchmark benchmark.prg
