ACME ?= acme
ACME_OPTS ?= -f cbm -DRELEASE=$(RELEASE)

.PHONY: all

all: installer

installer: installer.asm ../bitfire/installer ../bitfire/loader_*.inc
	$(ACME) $(ACME_OPTS) -o $@ $<

clean:
	-rm installer
