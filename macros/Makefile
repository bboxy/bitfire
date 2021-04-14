V ?= 0
ifeq ($(V),1)
VR:=
else
VR:=@
endif

all: macros

macros: link_macros_acme.inc
	@echo "Generating macros for other assemblers..."
	$(VR)./convert.sh

clean:
	$(VR)-rm link_macros_64tass.inc link_macros_kickass.inc link_macros_dreamass.inc link_macros_ca65.inc link_macros_dasm.inc
