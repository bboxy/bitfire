ACME = acme
ACMEOPT = -f cbm
CFLAGS = -Os -Wall
LDLIBS =
CC = gcc
ZX0 = ../zx0/src/zx0

all: dzx0

dzx0: depack.asm dzx0.asm testfile.zx0
	$(ACME) $(ACMEOPT) -o $@ $<
vice:
	x64 -autostartprgmode 1 -moncommands dzx0

clean:
	@-rm dzx0
