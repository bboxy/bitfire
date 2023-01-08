ACME ?= acme
ACME_OPTS ?= -f cbm
SALVADOR_PATH = salvador/src
SALVADOR_LIB_PATH = $(SALVADOR_PATH)/libdivsufsort/lib
DALI_INCLUDES = $(SALVADOR_PATH)/libdivsufsort/include
DALI_CFLAGS = $(CFLAGS) -O3 -g -Wall -fomit-frame-pointer -I$(DALI_INCLUDES)
ifdef win
CC = x86_64-w64-mingw32-gcc
STRIP = x86_64-w64-mingw32-strip
else
CC ?= gcc
STRIP ?= strip
endif
#DALI_OBJS += $(SALVADOR_PATH)/expand.o
#DALI_OBJS += $(SALVADOR_PATH)/matchfinder.o
#DALI_OBJS += $(SALVADOR_PATH)/shrink.o
#DALI_OBJS += $(SALVADOR_LIB_PATH)/divsufsort.o
#DALI_OBJS += $(SALVADOR_LIB_PATH)/divsufsort_utils.o
#DALI_OBJS += $(SALVADOR_LIB_PATH)/sssort.o
#DALI_OBJS += $(SALVADOR_LIB_PATH)/trsort.o

SALVADOR_SRC += $(SALVADOR_PATH)/expand.c
SALVADOR_SRC += $(SALVADOR_PATH)/matchfinder.c
SALVADOR_SRC += $(SALVADOR_PATH)/shrink.c
SALVADOR_SRC += $(SALVADOR_LIB_PATH)/divsufsort.c
SALVADOR_SRC += $(SALVADOR_LIB_PATH)/divsufsort_utils.c
SALVADOR_SRC += $(SALVADOR_LIB_PATH)/sssort.c
SALVADOR_SRC += $(SALVADOR_LIB_PATH)/trsort.c

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: dali

#%.o: %.c
#	$(VR)$(CC) $(DALI_CFLAGS) -c $< -o $@
#
#dali.o: dali.c sfx_small.h sfx_fast.h
#	@echo "Building dali..."
#	$(VR)$(CC) $(DALI_CFLAGS) -c $< -o $@
#
#dali: dali.o $(DALI_OBJS)

dali: dali.c $(SALVADOR_SRC) sfx_small.h sfx_fast.h
	@echo "Building dali..."
	$(VR)$(CC) dali.c $(SALVADOR_SRC) $(DALI_CFLAGS) -o $@
ifdef win
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif

testfile.lz: ../../benchmark/files/a
	./dali --no-inplace -o $@ $<

example.prg: example.asm dali dzx0_dali.asm testfile.lz
	$(VR)$(ACME) $(ACME_OPTS) -o $@ $<

sfx_small.h: sfx.asm
	@echo "Creating sfx_small.h..."
	$(VR)$(ACME) $(ACME_OPTS) --labeldump $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'DALI' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher_small[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst $(basename $@)

sfx_fast.h: sfx.asm
	@echo "Creating sfx_fast.h..."
	$(VR)$(ACME) $(ACME_OPTS) -DSFX_FAST=1 --labeldump $(basename $@).lst -o $(basename $@) $<
	$(VR)grep 'DALI' $(basename $@).lst | sed -e 's/[[:space:]]*;[[:space:]]*.*//g' -e 's/[[:space:]]*//g' -e 's/\=\$$/ 0x/g' -e 's/^/#define /' > $@
	$(VR)echo 'static const char decruncher[] = {' >> $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst $(basename $@)

clean:
	$(VR)-rm dali dali.exe *.o sfx_small.h sfx_fast.h example.prg testfile.lz
	$(VR)-rm $(SALVADOR_PATH)/*.o
	$(VR)-rm $(SALVADOR_LIB_PATH)/*.o
	$(VR)-make -C salvador clean
