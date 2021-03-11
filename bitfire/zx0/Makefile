CC = gcc
CFLAGS  = -Ofast -Wall
ACME = acme
ACMEOPT = -f cbm

all: zx0 dzx0

zx0: zx0.c compress.c optimize.c memory.c zx0.h sfx.h
	@echo "Buidling zx0..."
	@$(CC) $(CFLAGS) -o $@ $^

dzx0: dzx0.c
	@$(CC) $(CFLAGS) -o $@ $^

sfx.h: sfx.asm
	@echo '#ifndef SFX_H' > $@
	@echo '#define SFX_H' >> $@
	@$(ACME) $(ACMEOPT) --vicelabels $(basename $@).lst -o $(basename $@) $<
	@grep 'ZX0' $(basename $@).lst | sed -r 's/al C:([0-9a-f]+)\s\.(.*)/#define \2 0x\1/' >> $@
	@echo 'static const char decruncher[] = {' >> $@
	@hexdump -ve '1/1 "0x%.2x,"' $(basename $@) | sed -e 's/,$$/\n/g' | sed -e 's/.\{50\}/&\n/g' | sed -e 's/^/&    /g' >> $@
	@echo '};' >> $@
	@echo '#endif /* SFX_H */' >> $@
	@rm $(basename $@).lst sfx

clean:
	@rm zx0 dzx0 sfx.h
