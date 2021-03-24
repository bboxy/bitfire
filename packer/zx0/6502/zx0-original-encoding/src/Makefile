CC = gcc
CFLAGS  = -Ofast -Wall
RM = rm
EXTENSION =

all: zx0 dzx0

zx0: zx0.c optimize.c compress.c memory.c zx0.h
	$(CC) $(CFLAGS) -o zx0$(EXTENSION) zx0.c optimize.c compress.c memory.c

dzx0: dzx0.c
	$(CC) $(CFLAGS) -o dzx0$(EXTENSION) dzx0.c

clean:
	$(RM) *.obj dzx0 zx0
