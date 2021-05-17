!src "../../loader/loader_acme.inc"
!src "../../macros/link_macros_acme.inc"

		* = $1000
!bin "../../loader/installer",,2
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
		jsr .start_timer
		lda #$00
		jsr bitfire_loadcomp_
		ldx #$00
		jsr .stop_timer
		jsr .start_timer
		lda #$01
		jsr bitfire_loadcomp_
		ldx #$04
		jsr .stop_timer
		jsr .start_timer
		lda #$02
		jsr bitfire_loadraw_
		ldx #$08
		jsr .stop_timer
		jsr .start_timer
		lda #$03
		jsr bitfire_loadraw_
		ldx #$0c
		jsr .stop_timer
		jmp .loop
.start_timer
		lda #$00
		sta $dc0e
		lda #$40
		sta $dc0f
		lda #$ff
		sta $dc04
		sta $dc05
		sta $dc06
		sta $dc07
		lda #$41
		sta $dc0f
		lda #$01
		sta $dc0e
		rts
.stop_timer
		lda #$00
		sta $dc0e
		lda #$40
		sta $dc0f

		lda $dc04
		eor #$ff
		sta $0f03,x
		lda $dc05
		eor #$ff
		sta $0f02,x
		lda $dc06
		eor #$ff
		sta $0f01,x
		lda $dc07
		eor #$ff
		sta $0f00,x
		rts
