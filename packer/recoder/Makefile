ACME ?= acme
ACME_OPTS ?= -f cbm
zx0_CFLAGS ?= $(CFLAGS) -Ofast -Wall
CC ?= cc

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: dzx0 depack.prg

SOURCE:= dzx0.c
HEADER:=

dzx0: $(SOURCE) $(HEADER)
	@echo "zx0-reencoder..."
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $(SOURCE)

depack.prg: depack.asm bitfire.asm testfile.bitfire
	$(ACME) $(ACME_OPTS) -o $@ $<
vice:
	x64 -autostartprgmode 1 -moncommands dzx0

clean:
	$(VR)-rm zx0 dzx0 sfx.h depack.prg
