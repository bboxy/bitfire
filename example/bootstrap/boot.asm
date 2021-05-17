!src "../loader/loader_acme.inc"
!src "../macros/link_macros_acme.inc"
!src "../config.inc"

		* = $0100

!if SIDE = 1 {
		;load music
		jsr link_load_next_comp
		lda #$00
		;reset frame counter
		sta link_frame_count + 0
		sta link_frame_count + 1
		;load tune1
		jsr link_load_next_comp
		lda #$00
		ldy #$12
		ldx #$0a
		jsr link_music_init_side1
		;add if music entry changes on current side
		lda #>link_music_play_side1
		sta link_music_addr + 1
} else {
		;load music
		lda #$02		;file 1 is turndisk, file 0 is bootstrap
		jsr link_load_comp
		lda #$00
		;reset frame counter
		sta link_frame_count + 0
		sta link_frame_count + 1
		tax
		tay
		jsr link_music_init_side2
		;add if music entry changes on current side
		lda #>link_music_play_side2
		sta link_music_addr + 1
}

		sei
		lda #$35
		sta $01

	!if CONFIG_FRAMEWORK_MUSIC_NMI = 1 {
		+start_music_nmi

		;better cease all other IRQs
		lda #$7f
		sta $dc0d
		lda $dc0d

		lda #$00
		sta $d019
		sta $d01a

                dec $d019
	} else {
		ldx #<link_player
		lda #>link_player
		stx $fffe
		sta $ffff
		lda #$7f
		sta $dc0d
		lda $dc0d
		lda #$01
		sta $d019
		sta $d01a
		lda #$ff
		sta $d012
		lda $d011
		and #$7f
		sta $d011
	}
                cli

!if SIDE = 1 {
                jsr link_load_next_comp
} else {
		lda #$10
		bit $d011
		beq +
		lda #$01
		sta $d020
		sta $d021
		lda #$0b
		sta $d011
+
		;load first part
                jsr link_load_next_comp
}

		;start part at the same point in time
		jmp link_exit
