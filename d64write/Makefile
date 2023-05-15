D64WRITE_CFLAGS = $(CFLAGS) -Ofast -Wall
D64WRITE_LIBS = $(CLIBS) -lpng
ifdef win
CC = x86_64-w64-mingw32-gcc
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

SOURCE:= d64write.c
HEADER:= *.h

all: d64write

d64write: $(SOURCE) $(HEADER)
	@echo "Building d64write..."
	$(VR)$(CC) $(D64WRITE_CFLAGS) -o $@ $(SOURCE) $(D64WRITE_LIBS)
ifdef win
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif

clean:
	$(VR)-rm d64write
