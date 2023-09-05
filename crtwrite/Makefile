CRTWRITE_CFLAGS = $(CFLAGS) -Ofast -Wall
CRTWRITE_LIBS = $(CLIBS) -lpng
ACME ?= acme
ACME_OPTS ?= -f cbm

ifdef win
CC = x86_64-w64-mingw32-gcc
STRIP = x86_64-w64-mingw32-strip
else ifdef win32
CC = x86_64-w64-mingw32-gcc-win32
STRIP = x86_64-w64-mingw32-strip
else
CC ?= gcc
STRIP ?= strip
endif

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

SOURCE:= crtwrite.c
HEADER:= *.h

all: crtwrite

crtwrite: $(SOURCE) $(HEADER) bootstrap.h
	@echo "Building crtwrite..."
	$(VR)$(CC) $(CRTWRITE_CFLAGS) -o $@ $(SOURCE) $(CRTWRITE_LIBS)
ifdef win
	$(VR)$(STRIP) $@.exe
else ifdef win32
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif
bootstrap.h: bootstrap.asm
	$(VR)$(ACME) $(ACME_OPTS) --labeldump $(basename $@).lst -o $(basename $@) $<
	$(VR)echo 'static const char bootstrap[] = {' > $@
	@#create a hexdump, add a marker (+) where lines are truncated (each 50 chars = 8 bytes per line), substitute marker (+) with newline (use tr here, as bsd-sed fails on \n), add identation to each line
	$(VR)hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/+/g' -e 's/.\{50\}/&+/g' | tr -s '+' '\n' | sed 's/^/&    /g' >> $@
	$(VR)echo '};' >> $@
	$(VR)rm $(basename $@).lst $(basename $@)

clean:
	$(VR)-rm crtwrite bootstrap.h
