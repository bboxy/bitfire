link_exit ?= 0
release ?= 0
ACME ?= acme
ACME_OPTS ?= -v1 -f cbm -Drelease=$(release) -Dlink_exit=$(link_exit)

.PHONY: all

all: bootstrap1 bootstrap2

bootstrap1: boot.asm ../*.inc
	$(ACME) $(ACME_OPTS) -DSIDE=1 -o $@ $<

bootstrap2: boot.asm ../*.inc
	$(ACME) $(ACME_OPTS) -DSIDE=2 -o $@ $<

clean:
	-rm bootstrap*
