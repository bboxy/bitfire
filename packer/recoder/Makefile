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

all: recode

recode: recode.c sfx.h
	@echo "zx0-reencoder..."
	$(VR)$(CC) $(zx0_CFLAGS) -o $@ $^

depack.prg: depack.asm bitfire.asm testfile.bitfire
	$(ACME) $(ACME_OPTS) -o $@ $<

sfx.h: sfx.asm
	@#$(VR)$(ACME) $(ACME_OPTS) -l sau.lst --vicelabels $(basename $@).lst -o $(basename $@) $<
	@#$(VR)grep 'ZX0' $(basename $@).lst | sed -E 's/al C:([0-9a-f]+)[[:space:]]\.(.*)/#define \2 0x\1/' >> $@
	$(VR)$(ACME) $(ACME_OPTS) -l $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'ZX0' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst sfx

clean:
	$(VR)-rm recode sfx.h depack.prg
