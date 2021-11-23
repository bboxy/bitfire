ACME ?= acme
ACME_OPTS ?= -f cbm
bitnax_CFLAGS = $(CFLAGS) -Ofast -Wall
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
	@echo "Building bitnax..."
	$(VR)$(CC) $(bitnax_CFLAGS) -o $@ $(SOURCE)

sfx.h: sfx.asm
	$(VR)$(ACME) $(ACME_OPTS) -l $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'BITNAX' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/\s*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/\n/g' -e 's/.\{50\}/&\n/g' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	#$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)-rm bitnax sfx.h
