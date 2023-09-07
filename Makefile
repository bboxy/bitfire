MAKE_OPTS ?= --no-print-directory --quiet
ACME ?= acme

LDLIBS =
CC ?= cc

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: loader dali d64write crtwrite

#zx0-play: FORCE
#	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C packer/$@

zx0: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C packer/$@

bitnax: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C packer/$@

dali: FORCE
	@git submodule update --init --recursive
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C packer/$@

d64write: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C $@

crtwrite: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C $@

loader: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C $@

macros: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C $@

benchmark-lz: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C benchmark/ $@

benchmark-dali: FORCE
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C benchmark/ $@

benchmark: FORCE zx0 dali d64write loader
	@$(MAKE) $(MAKE_OPTS) ACME=$(ACME) -C benchmark/ $@

#link: FORCE zx0 bitnax d64write loader
#	@$(MAKE) $(MAKE_OPTS) -C link/

clean:
	@$(MAKE) $(MAKE_OPTS) -C packer/zx0/ clean
	@$(MAKE) $(MAKE_OPTS) -C packer/dali/ clean
	@$(MAKE) $(MAKE_OPTS) -C d64write/ clean
	@$(MAKE) $(MAKE_OPTS) -C crtwrite/ clean
	@$(MAKE) $(MAKE_OPTS) -C loader/ clean
	@$(MAKE) $(MAKE_OPTS) -C benchmark/ clean
	@$(MAKE) $(MAKE_OPTS) -C example/loadertest/ clean
	#@$(MAKE) $(MAKE_OPTS) -C macros/ clean
#	@$(MAKE) $(MAKE_OPTS) -C link/ clean

FORCE:
	@true
