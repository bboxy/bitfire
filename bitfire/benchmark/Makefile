BITNAX_OPTS = --bitfire --checksum #--overlap

all: cl13

cl13: checksum.asm
	make -C ../
	acme -f cbm -o $@ $<
	../bitnax --sfx 0x0800 -o $@.lz $@
	cd files; rm *.lz; for i in *; do ../../bitnax $(BITNAX_OPTS) -o $$i.lz $$i; done
	../d64write -c side1.d64 -h oxyron -i \<3 --side 1 -a 12 dirart \
			--boot $@.lz \
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
	../d64write -c side2.d64 -h oxyron -i \<3 --side 2 -a 12 dirart \
			--boot $@.lz \
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
	#~/repo/vice-emu-code/vice/src/x64 side1.d64
	x64 side1.d64
	rm $@ $@.lz

clean:
	-rm side1.d64 side2.d64 cl13 cl13.lz
	-rm files/*.lz
	make -C ../ clean
