		.macro link_decomp_under_io
		.local l
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
		.endmacro

		.macro link_load_next_raw_decomp
		.local l
			jsr link_load_next_raw
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
		.endmacro

		;load next file as a raw file (do not decomp on the fly) and jump to .addr
		.macro link_load_next_raw_jmp arg
		.local l
			lda #>(arg-1)
			pha
			lda #<(arg-1)
			pha
			jmp link_load_next_raw
		.endmacro

		;load next file and decomp on the fly, then jump to arg
		.macro link_load_next_comp_jmp arg
		.local l
			lda #>(arg-1)
			pha
			lda #<(arg-1)
			pha
			jmp link_load_next_comp
		.endmacro

		;decomp previously loaded file and jump to arg
		.macro link_decomp_jmp arg
		.local l
			lda #>(arg-1)
			pha
			lda #<(arg-1)
			pha
			jmp link_decomp
		.endmacro

		.macro link_load_next_double
		.local l
			jsr link_load_next_comp
			jsr link_load_next_raw
			dec $01
			jsr link_decomp
			inc $01
		.endmacro

		;link irq hook back to base irq
		.macro link_player_irq
		.local l
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		.endmacro

		.macro reset_drive
		.local l
			lda #BITFIRE_RESET
			jsr bitfire_send_byte_
		.endmacro

		;request next disk side and reset filenum_counter
		.macro request_disk arg
		.local l
			lda #arg + $f0
			;simply do a bogus loadraw, this will call ld_pblock until eof is raised, nothing is loaded as block_ready never happens :-)
			jsr bitfire_loadraw_
			;jsr bitfire_send_byte_
			;pha
			;pla
			;pha
			;pla
			;+wait_floppy_idle
		.endmacro

		;wait for given frame counter value
		.macro link_wait_syncpoint arg
		.local l
l
			lda link_syncpoint
			cmp #arg
			bcc l
		.endmacro

		.macro setup_sync arg
		.local l
			lda #$00
			sta link_frame_count + 0	;avoid that the counter wraps worstcase, as we can't write atomic
			lda #>($8000-arg)
			sta link_frame_count + 1
			lda #<($8000-arg)
			sta link_frame_count + 0	;now setup counter
		.endmacro

		.macro sync
		.local l
l
			lda link_frame_count + 1	;Counter expired when $8000 is reached
			bpl l
		.endmacro

		.macro bus_lock
		.local l
			lda #$37			;set ATN
			sta $dd02
		.endmacro

		.macro bus_unlock
		.local l
			lda $dd00
			and #$03
			sta $dd00
			lda #$3f
			sta $dd02
		.endmacro

		.macro set_depack_pointers arg
		.local l
			lda #<arg
			sta bitfire_load_addr_lo
			lda #>arg
			sta bitfire_load_addr_hi
		.endmacro

		.macro link_music_update
		.local l
			inc link_frame_count + 0
			bne l
			inc link_frame_count + 1
l
			jmp (link_music_addr)
		.endmacro

		.macro to_nmi
		.local l
			lda #$ad
			sta link_player + 7
			lda #$0d
			sta link_player + 8
			lda #$dd
			sta link_player + 9
		.endmacro

		.macro to_irq
		.local l
			lda #$ce
			sta link_player + 7
			lda #$19
			sta link_player + 8
			lda #$d0
			sta link_player + 9
		.endmacro

		.macro switch_to_nmi
		.local l
			lda #$fe
l
			cmp $d012
			bne l
			sei
			+to_nmi
			+start_music_nmi
			jsr link_music_play
		.endmacro

		.macro switch_to_irq
		.local l
			lda #$fe
l
			cmp $d012
			bne l
			+stop_music_nmi
			+to_irq
			ldx #<link_player
			lda #>link_player

			stx $fffe
			sta $ffff
			lda #$ff
			sta $d012
			jsr link_music_play
		.endmacro

		.macro start_music_nmi
		.local l
			;lda #$20
			;sta link_music_play_jmp
			ldx #<link_player
			lda #>link_player

			stx $fffa
			sta $fffb
			lda #$00
			sta $dd0e
			lda $dd0d
			lda #$c7
			sta $dd04
			lda #$4c
			sta $dd05
			lda #$81
			sta $dd0d

			lda #$ff
l
			cmp $d012
			bne l

			lda #$11
			sta $dd0e
		.endmacro

		.macro stop_music_nmi
		.local l
			lda #$7f
			sta $dd0d
			lda $dd0d
		.endmacro

		.macro restart_music_nmi
		.local l
			lda $dd0d
			lda #$81
			sta $dd0d
		.endmacro

		.macro set_music_addr arg
		.local l
			lda #<arg
			sta link_music_addr + 0
			lda #>arg
			sta link_music_addr + 1
		.endmacro

;		!macro gaps_setup {
;			ldx #$09
;l
;			lda CONFIG_RESIDENT_ADDR + $000,x
;			pha
;			lda CONFIG_RESIDENT_ADDR + $100,x
;			pha
;			dex
;			bpl l
;		}
;
;		!macro gaps_restore {
;			ldx #$00
;l
;			pla
;			sta CONFIG_RESIDENT_ADDR + $000,x
;			pla
;			sta CONFIG_RESIDENT_ADDR + $100,x
;			inx
;			cpx #$0a
;			bne l
;		}
