!src "../loader/loader_acme.inc"
!src "../macros/link_macros_acme.inc"

		* = $1000
!bin "../loader/installer",,2
		* = $0800
		lda #$0b
		sta $d020
		sta $d021
		ldx #$00
-
		lda #$7b
		sta $0400,x
		sta $0500,x
		sta $0600,x
		sta $0700,x
		dex
		bne -

		lda #$3b
		sta $d011
		lda #$18
		sta $d018
		lda #$08
		sta $d016

		jsr bitfire_install_

		sei

		lda #$35
		sta $01
.loop
		lda #$00
		jsr link_load_comp
		jsr link_load_next_comp
		lda #$02
		jsr link_load_raw
		jsr link_load_next_raw
		jmp .loop
