ACME = acme
ACMEOPT = -f cbm
CFLAGS = -Os -Wall
LDLIBS =
CC = gcc

all: zx0 d64write bitfire gcr_tables bitnax

zx0: FORCE
	@make --no-print-directory -C $@

d64write: d64write.c *.h
	@echo "Buidling d64write..."
	@$(CC) $< -o $@ $(CFLAGS)

bitnax: bitnax.c sfx.h
	@echo "Buidling bitnax..."
	@${CC} $< -o $@ ${CFLAGS}

gcr_tables: gcr_tables.c
	@echo "Buidling gcr table tool..."
	@${CC} $< -o $@ ${CFLAGS}

resident: resident.asm config.inc Makefile
	@echo -n "Buidling resident part ->"
	@$(ACME) $(ACMEOPT) -o resident -l resident_labels.txt resident.asm
	@printf " Size: $$%04x\n" $$((`wc -c < resident` - 2))
	@echo "Creating loader_acme.inc..."
	@#fetch all global labels for resident part
	@grep "bitfire_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" > loader_acme.inc
	@grep "link_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme.inc
	@#fetch all config labels
	@grep "BITFIRE_*" resident_labels.txt | sed "s/\s*;\s*.*//g" | tr -d "\t" | sed 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' | sed "s/\= / \= /" >> loader_acme.inc
	@echo "Creating loader_c6510.inc..."
	@#convert to c6510 format (add const, wipe out underscores and add prefix with .)
	@echo "if def BITFIRE.ISINCLUDED == 0" > loader_c6510.inc
	@cat loader_acme.inc | sed "s/\(.*\)/const \\1/" >> loader_c6510.inc
	@echo "endif" >> loader_c6510.inc
	@echo ".importonce" > loader_kickass.inc
	@#cat loader_acme.inc | grep "bitfire" | sed "s/bitfire/\.var bitfire/" >> loader_kickass.inc
	@#cat loader_acme.inc | grep "BITFIRE" | sed "s/BITFIRE/\.var BITFIRE/" >> loader_kickass.inc
	@echo "Creating loader_kickass.inc..."
	@cat loader_acme.inc | sed "s/\(.*\)/\.var \\1/" >> loader_kickass.inc
	@#echo "}" >> loader_kickass.inc
	@rm resident_labels.txt

installer: resident drivecode.asm installer.asm config.inc detect.asm Makefile
	@echo "Buidling installer..."
	@$(ACME) $(ACMEOPT) -o installer installer.asm

bitfire: installer

clean:
	@-rm bitnax d64write installer loader_acme.inc loader_c6510.inc loader_kickass.inc resident gcr_tables
	@make --no-print-directory -C zx0/ clean

FORCE:
