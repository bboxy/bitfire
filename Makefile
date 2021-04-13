MAKE_OPTS ?= --no-print-directory --quiet

LDLIBS =
CC ?= cc

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: zx0 d64write loader

zx0: FORCE
	@$(MAKE) $(MAKE_OPTS) -C packer/zx0/

bitnax: FORCE
	@$(MAKE) $(MAKE_OPTS) -C packer/bitnax/

d64write: FORCE
	@$(MAKE) $(MAKE_OPTS) -C d64write/

loader: FORCE
	@$(MAKE) $(MAKE_OPTS) -C loader/

benchmark-lz: FORCE
	@$(MAKE) $(MAKE_OPTS) -C benchmark/ benchmark-lz

benchmark: FORCE zx0 bitnax d64write loader
	@$(MAKE) $(MAKE_OPTS) -C benchmark/ benchmark

#link: FORCE zx0 bitnax d64write loader
#	@$(MAKE) $(MAKE_OPTS) -C link/

clean:
	@$(MAKE) $(MAKE_OPTS) -C packer/zx0/ clean
	@$(MAKE) $(MAKE_OPTS) -C packer/bitnax/ clean
	#@$(MAKE) $(MAKE_OPTS) -C d64write/ clean
	#@$(MAKE) $(MAKE_OPTS) -C loader/ clean
	#@$(MAKE) $(MAKE_OPTS) -C benchmark/ clean
#	@$(MAKE) $(MAKE_OPTS) -C link/ clean

FORCE:
