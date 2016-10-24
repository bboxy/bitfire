ACME = acme
ACMEOPT = -f cbm
CFLAGS = -Os -Wall
LDLIBS =
CC = gcc

all: bitnax d64write installer

d64write: d64write.c *.h
	@echo "Buidling d64write..."
	@$(CC) $< -o $@ $(CFLAGS)

bitnax: lz.c
	@echo "Buidling bitnax..."
	@${CC} $< -o $@ ${CFLAGS}

installer: installer.asm drivecode.asm request_disc.asm resident.asm detect.asm config.inc Makefile resident_minimal_zp.asm
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
	@echo "Buidling installer..."
	@$(ACME) $(ACMEOPT) -o installer installer.asm
	@rm resident_labels.txt

clean:
	@-rm bitnax d64write installer loader_acme.inc loader_c6510.inc loader_kickass.inc resident
