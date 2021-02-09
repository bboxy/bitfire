BITNAX_OPTS = -v --bitfire
D64WRITE_OPTS = -v

all: installer

installer: checksum.asm Makefile ../ ../../
	make -C ../
	acme -f cbm -o $@ $<
	../bitnax --sfx 0x0800 -o $@.prg $@
	cd files; rm *.lz; for i in *; do ../../bitnax $(BITNAX_OPTS) -o $$i.lz $$i; done
	../d64write $(D64WRITE_OPTS) -c side1.d64 -h oxyron -i \<3 --side 1 -a 12 dirart \
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
			-s installer.prg 2

#	../d64write -c side2.d64 -h oxyron -i \<3 --side 2 -a 12 dirart \
#			--boot $@.lz \
#			-b files/d \
#			-b files/e \
#			-b files/f \
#			-b files/g \
#			-b files/h \
#			-b files/i \
#			-b files/j \
#			-b files/k \
#			-b files/l \
#			-b files/m \
#			-b files/n \
#			-b files/o \
#			-b files/p \
#			-b files/q \
#			-b files/r
	#~/repo/vice-emu-code/vice/src/x64 side1.d64
	x64 side1.d64
	rm $@ $@.lz

clean:
	-rm side1.d64 side2.d64 installer installer.prg
	-rm files/*.lz
	make -C ../ clean
