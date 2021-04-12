RELEASE ?= 0
ACME ?= acme
ACME_OPTS ?= -f cbm -DRELEASE=$(RELEASE)

.PHONY: all

all: installer

installer: installer.asm ../loader/installer ../loader/loader_*.inc
	$(ACME) $(ACME_OPTS) -o $@ $<

clean:
	-rm installer
