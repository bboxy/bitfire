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

crtwrite: $(SOURCE) $(HEADER) bootstrap
	@echo "Building crtwrite..."
	$(VR)$(CC) $(CRTWRITE_CFLAGS) -o $@ $(SOURCE) $(CRTWRITE_LIBS)
ifdef win
	$(VR)$(STRIP) $@.exe
else ifdef win32
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif

bootstrap: bootstrap.asm
	$(VR)$(ACME) $(ACME_OPTS) -o $@ $<

clean:
	$(VR)-rm crtwrite bootstrap
