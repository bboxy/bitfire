CFLAGS = -Ofast -Wall
LDLIBS =
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
	@echo "Buidling d64write..."
	$(VR)$(CC) $(CFLAGS) -o $@ $(SOURCE)

clean:
	$(VR)rm d64write
