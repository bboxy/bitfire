ACME = acme
ACMEOPT = -v1 -f cbm -DRELEASE=1 -Dlink_exit=$(link_exit)

.PHONY: all

all: bootstrap1 bootstrap2

bootstrap1: boot.asm ../*.inc
	$(ACME) $(ACMEOPT) -DSIDE=1 -o $@ $<

bootstrap2: boot.asm ../*.inc
	$(ACME) $(ACMEOPT) -DSIDE=2 -o $@ $<

clean:
	-rm bootstrap*
