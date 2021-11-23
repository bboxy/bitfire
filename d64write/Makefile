d64write_CFLAGS = $(CFLAGS) -Ofast -Wall
CC ?= cc

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

clean:
	$(VR)rm d64write
