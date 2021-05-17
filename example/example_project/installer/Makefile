release ?= 0
link_exit ?= 256
ACME ?= acme
ACME_OPTS ?= -f cbm -Drelease=$(release)

.PHONY: all

all: installer

installer: installer.asm ../loader/installer ../loader/loader_*.inc
	$(ACME) $(ACME_OPTS) -Dlink_exit=$(link_exit) -o $@ $<

clean:
	-rm installer
