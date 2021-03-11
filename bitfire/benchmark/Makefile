D64WRITE = ../d64write
D64WRITE_OPTS = -v

ZX0 = ../zx0/zx0
ZX0_OPTS = -f

all: installer

lz:
	make -C ../zx0/
	rm files/*.lz; for i in files/*; do $(ZX0) $(ZX0_OPTS) -o $$i.lz $$i; done

installer: checksum.asm Makefile ../ ../../
	make -C ../
	acme -f cbm -o $@ $<
	$(ZX0) $(ZX0_OPTS) --sfx 0x0800 -o $@.prg $@
	#../../orig_mapping/bitnax -v --sfx 0x0800 -o $@.prg $@
	$(D64WRITE) $(D64WRITE_OPTS) -c side1.d64 -h oxyron -i \<3 --side 1 \
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
			-b files/r.lz

	x64 side1.d64
	rm $@ $@.lz

clean:
	-rm side1.d64 side2.d64 installer installer.prg
	#-rm files/*.lz
	make -C ../ clean
