ACME ?= acme
ACME_OPTS ?= -f cbm
SALVADOR_PATH = salvador
SALVADOR_FILES += $(SALVADOR_PATH)/src/shrink.c
SALVADOR_FILES += $(SALVADOR_PATH)/src/matchfinder.c
SALVADOR_FILES += $(SALVADOR_PATH)/src/expand.c
SALVADOR_FILES += $(SALVADOR_PATH)/src/libdivsufsort/lib/*.c
SALVADOR_INCLUDES = $(SALVADOR_PATH)/src/libdivsufsort/include
DALI_CFLAGS = -O3 -g -Wall -fomit-frame-pointer -I$(SALVADOR_INCLUDES)
DALI_CC = gcc

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: dali

dali: dali.c sfx_small.h sfx_fast.h $(SALVADOR_FILES)
	@echo "Building dali..."
	$(VR)$(DALI_CC) $(DALI_CFLAGS) -o $@ $^
	$(VR)strip $@

depack.prg: depack.asm dzx0.asm dzx0_dali.asm ../../benchmark/files/a.lz
	$(ACME) $(ACME_OPTS) -o $@ $<

sfx_small.h: sfx.asm
	@echo "Compiling sfx code for dali..."
	$(VR)$(ACME) $(ACME_OPTS) -l $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'DALI' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher_small[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst $(basename $@)

sfx_fast.h: sfx.asm
	@echo "Compiling sfx code for dali..."
	$(VR)$(ACME) $(ACME_OPTS) -DSFX_FAST=1 -l $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'DALI' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst $(basename $@)

clean:
	$(VR)-rm dali sfx_small.h sfx_fast.h depack.prg
