CC ?= cc
zx0_CFLAGS ?= $(CFLAGS) -Ofast -Wall
ACME ?= acme
ACMEOPT = -f cbm

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: zx0 dzx0

zx0_SRC:= zx0.c compress.c optimize.c memory.c
zx0_HDR:= zx0.h sfx.h

zx0: $(zx0_SRC) $(zx0_HDR)
	@echo "Buidling zx0..."
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $(zx0_SRC)

dzx0: dzx0.c
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $^

sfx.h: sfx.asm
	$(VR)echo '#ifndef SFX_H' > $@
	$(VR)echo '#define SFX_H' >> $@
	$(VR)$(ACME) $(ACMEOPT) --vicelabels $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'ZX0' $(basename $@).lst | sed -E 's/al C:([0-9a-f]+)[[:space:]]\.(.*)/#define \2 0x\1/' >> $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/\n/g' -e 's/.\{50\}/&\n/g' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)echo '#endif /* SFX_H */' >> $@
	$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)rm zx0 dzx0 sfx.h
