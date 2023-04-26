ACME ?= acme
ACME_OPTS ?= -f cbm
loader_CFLAGS ?= $(CFLAGS) -Os -Wall
CC ?= cc
LOADER_CONFIG ?= '../config.inc'

V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: bitfire gcr_tables

gcr_tables: gcr_tables.c
	@echo "Building gcr table tool..."
	$(VR)$(CC) $< -o $@ $(loader_CFLAGS)

resident: resident.asm ../config.inc constants.inc Makefile
	@echo "Building resident part..."
	$(VR)$(ACME) $(ACME_OPTS) -I ../../ -I ../ -o resident --labeldump resident_labels.txt resident.asm
	@printf "Resident size: $$%04x\n" $$((`wc -c < resident` - 2))
	@echo "Creating loader_acme.inc..."
	@#fetch all global labels for resident part
	$(VR)grep "bitfire_*" resident_labels.txt | sed "s/[[:space:]]*;[[:space:]]*.*//g" | tr -d "\t" | sed -e 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' -e "s/\= / \= /" > loader_acme.inc
	$(VR)grep "link_*" resident_labels.txt | sed "s/[[:space:]]*;[[:space:]]*.*//g" | tr -d "\t" | sed -e 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' -e "s/\= / \= /" >> loader_acme.inc
	@#fetch all config labels
	$(VR)grep "BITFIRE_*" resident_labels.txt | sed "s/[[:space:]]*;[[:space:]]*.*//g" | tr -d "\t" | sed -e 's/\$$\([0-9a-f]\{3\}$$\)/\$$0\1/' -e "s/\= / \= /" >> loader_acme.inc
	@echo "Creating loader_c6510.inc..."
	@#convert to c6510 format (add const, wipe out underscores and add prefix with .)
	$(VR)echo "if def BITFIRE.ISINCLUDED == 0" > loader_c6510.inc
	$(VR)cat loader_acme.inc | sed "s/\(.*\)/const \\1/" >> loader_c6510.inc
	$(VR)echo "endif" >> loader_c6510.inc
	$(VR)echo ".importonce" > loader_kickass.inc
	@#cat loader_acme.inc | grep "bitfire" | sed "s/bitfire/\.var bitfire/" >> loader_kickass.inc
	@#cat loader_acme.inc | grep "BITFIRE" | sed "s/BITFIRE/\.var BITFIRE/" >> loader_kickass.inc
	@echo "Creating loader_kickass.inc..."
	$(VR)cat loader_acme.inc | sed "s/\(.*\)/\.var \\1/" >> loader_kickass.inc
	@#echo "}" >> loader_kickass.inc
	$(VR)rm resident_labels.txt

installer: resident drivecode.asm installer.asm constants.inc ../config.inc detect.asm Makefile
	@echo "Buidling installer..."
	@$(ACME) $(ACME_OPTS) -I ../../ -I ../ -o installer installer.asm

bitfire: installer

clean:
	$(VR)-rm installer loader_acme.inc loader_c6510.inc loader_kickass.inc resident gcr_tables
