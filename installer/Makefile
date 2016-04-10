C6510 = c6510
C6510OPT = -p ../../../include
ACME = acme
ACMEOPT= -v1 -f cbm

.PHONY: all

all: installer

installer: installer.asm ../bitfire/installer ../bitfire/loader_*.inc
	$(ACME) $(ACMEOPT) -DRELEASE=1 -o $@ $<

clean:
	-rm installer
