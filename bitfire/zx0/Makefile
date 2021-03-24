CC = gcc
CFLAGS  = -Ofast -Wall
ACME = acme
ACMEOPT = -f cbm

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: zx0 dzx0

zx0: zx0.c compress.c optimize.c memory.c zx0.h sfx.h
	@echo "Buidling zx0..."
	$(VR)$(CC) $(CFLAGS) -o $@ $^

dzx0: dzx0.c
	$(VR)$(CC) $(CFLAGS) -o $@ $^

sfx.h: sfx.asm
	$(VR)echo '#ifndef SFX_H' > $@
	$(VR)echo '#define SFX_H' >> $@
	$(VR)$(ACME) $(ACMEOPT) --vicelabels $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'ZX0' $(basename $@).lst | sed -r 's/al C:([0-9a-f]+)\s\.(.*)/#define \2 0x\1/' >> $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/\n/g' | sed -e 's/.\{50\}/&\n/g' | sed -e 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)echo '#endif /* SFX_H */' >> $@
	$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)rm zx0 dzx0 sfx.h
