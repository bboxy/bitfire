CRTWRITE_CFLAGS = $(CFLAGS) -Ofast -Wall
CRTWRITE_LIBS = $(CLIBS) -lpng
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

crtwrite: $(SOURCE) $(HEADER)
	@echo "Building crtwrite..."
	$(VR)$(CC) $(CRTWRITE_CFLAGS) -o $@ $(SOURCE) $(CRTWRITE_LIBS)
ifdef win
	$(VR)$(STRIP) $@.exe
else ifdef win32
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif

clean:
	$(VR)-rm crtwrite
