d64write_CFLAGS = $(CFLAGS) -Ofast -Wall
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
	$(VR)$(CC) $(d64write_CFLAGS) -o $@ $(SOURCE)
ifdef win
	$(VR)$(STRIP) $@.exe
else
	$(VR)$(STRIP) $@
endif

clean:
	$(VR)-rm d64write
