ACME ?= acme
ACMEOPT = -f cbm
CFLAGS = -Ofast -Wall
LDLIBS =
CC ?= cc

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

SOURCE:= bitnax.c
HEADER:= sfx.h

all: bitnax

bitnax: $(SOURCE) $(HEADER)
	@echo "Buidling bitnax..."
	$(VR)$(CC) $(CFLAGS) -o $@ $(SOURCE)

sfx.h: sfx.asm
	$(VR)echo '#ifndef SFX_H' > $@
	$(VR)echo '#define SFX_H' >> $@
	$(VR)$(ACME) $(ACMEOPT) --vicelabels $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'BITNAX' $(basename $@).lst | sed -E 's/al C:([0-9a-f]+)[[:space:]]\.(.*)/#define \2 0x\1/' >> $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/\n/g' -e 's/.\{50\}/&\n/g' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)echo '#endif /* SFX_H */' >> $@
	$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)rm bitnax sfx.h
