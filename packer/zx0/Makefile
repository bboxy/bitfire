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

SOURCE:= zx0.c compress.c optimize.c
HEADER:= zx0.h sfx.h

all: zx0 dzx0

zx0: $(SOURCE) $(HEADER)
	@echo "Building zx0..."
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $(SOURCE)

dzx0: dzx0.c
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $^

sfx.h: sfx.asm
	@#$(VR)$(ACME) $(ACME_OPTS) --labeldump sau.lst --vicelabels $(basename $@).lst -o $(basename $@) $<
	@#$(VR)grep 'ZX0' $(basename $@).lst | sed -E 's/al C:([0-9a-f]+)[[:space:]]\.(.*)/#define \2 0x\1/' >> $@
	$(VR)$(ACME) $(ACME_OPTS) --labeldump $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'ZX0' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)-rm zx0 dzx0 sfx.h
