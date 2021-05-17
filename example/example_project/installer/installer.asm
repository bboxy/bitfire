;linking helper macros
!src "../macros/link_macros_acme.inc"

;loader labels
!src "../loader/loader_acme.inc"

!ifndef link_exit {
link_exit	= $0100
}

		* = bitfire_install_
		!bin "../loader/installer",,2

		* = $0900
.init
		;install loader
		jsr bitfire_install_

		;reset stack shits
		sei
		lda #$35
		sta $01
		ldx #$ff
		txs

		;load stage 2, either loaded by bootloader or after turn disk
		jsr link_load_next_raw

		;XXX TODO here it would also be possible to laod a custom link_resident part per side, but would require includes per side and resident part
		jmp link_exit
