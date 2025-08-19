		mac link_decomp_under_io
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
		endm

		mac link_load_next_raw_decomp
			jsr link_load_next_raw
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
		endm

		;load next file as a raw file (do not decomp on the fly) and jump to .addr
		mac link_load_next_raw_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_raw
		endm

		;load next file and decomp on the fly, then jump to .arg
		mac link_load_next_comp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_comp
		endm

		;decomp previously loaded file and jump to .arg
		mac link_decomp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_decomp
		endm

		mac link_load_next_double
			jsr link_load_next_comp
			jsr link_load_next_raw
			dec $01
			jsr link_decomp
			inc $01
		endm

		;link irq hook back to base irq
		mac link_player_irq
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		endm

		mac reset_drive
			lda #BITFIRE_RESET
			jsr bitfire_send_byte_
		endm

		;request next disk side and reset filenum_counter
		mac request_disk
			lda #{1} + $ef
			;simply do a bogus loadraw, this will call ld_pblock until eof is raised, nothing is loaded as block_ready never happens :-)
			jsr bitfire_loadraw_
		endm

		mac setup_sync
			lda #$00
			sta link_frame_count + 0	;avoid that the counter wraps worstcase, as we can't write atomic
			lda #>($8000-{1})
			sta link_frame_count + 1
			lda #<($8000-{1})
			sta link_frame_count + 0	;now setup counter
		endm

		mac sync
.l
			lda link_frame_count + 1	;Counter expired when $8000 is reached
			bpl .l
		endm

		mac bus_lock
			lda #$37			;set ATN
			sta $dd02
		endm

		mac bus_unlock
			lda $dd00
			and #$03
			sta $dd00
			lda #$3f
			sta $dd02
		endm

		mac set_depack_pointers
			lda #<{1}
			sta bitfire_load_addr_lo
			lda #>{1}
			sta bitfire_load_addr_hi
		endm

		mac link_music_update
			inc link_frame_count + 0
			bne .l
			inc link_frame_count + 1
.l
			jmp (link_music_addr)
		endm

		mac to_nmi
			lda #$ad
			sta link_player + 7
			lda #$0d
			sta link_player + 8
			lda #$dd
			sta link_player + 9
		endm

		mac to_irq
			lda #$ce
			sta link_player + 7
			lda #$19
			sta link_player + 8
			lda #$d0
			sta link_player + 9
		endm

		mac switch_to_nmi
			lda #$fe
.l
			cmp $d012
			bne .l
			sei
			to_nmi
			start_music_nmi
			jsr link_music_play
		endm

		mac switch_to_irq
			lda #$fe
.l
			cmp $d012
			bne .l
			stop_music_nmi
			to_irq
			ldx #<link_player
			lda #>link_player

			stx $fffe
			sta $ffff
			lda #$ff
			sta $d012
			jsr link_music_play
		endm

		mac start_music_nmi
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
.l
			cmp $d012
			bne .l

			lda #$11
			sta $dd0e
		endm

		mac stop_music_nmi
			lda #$7f
			sta $dd0d
			lda $dd0d
		endm

		mac restart_music_nmi
			lda $dd0d
			lda #$81
			sta $dd0d
		endm

		mac set_music_addr
			lda #<{1}
			sta link_music_addr + 0
			lda #>{1}
			sta link_music_addr + 1
		endm

		mac wait_space
			lda #$7f
			sta $dc00
			lda #$10
.l

			bit $dc01
			bne .l
		endm

		mac wait_shift_lock
			lda #$fd
			sta $dc00
.l
			lda $dc01
			bpl .l
		endm

		mac crt_request_disk
			setup_sync .arg
.l
			lda #$7f			;space pressed?
			sta $dc00
			lda $dc01
			and #$10
			beq .e				;yes, exit
			lda link_frame_count + 1	;check counter
			bpl .g
			sta .f + 1
.g
			lda #$fd			;shift lock pressed?
			sta $dc00
			lda $dc01
.f
			and #$00
			bpl .l				;shift lock pressed or not expired
.e
		endm

		mac skip_file
			lda #BITFIRE_SKIP_FILE
			jsr bitfire_loadraw_
		endm

;		!macro gaps_setup {
;			ldx #$09
;.l
;			lda CONFIG_RESIDENT_ADDR + $000,x
;			pha
;			lda CONFIG_RESIDENT_ADDR + $100,x
;			pha
;			dex
;			bpl .l
;		}
;
;		!macro gaps_restore {
;			ldx #$00
;.l
;			pla
;			sta CONFIG_RESIDENT_ADDR + $000,x
;			pla
;			sta CONFIG_RESIDENT_ADDR + $100,x
;			inx
;			cpx #$0a
;			bne .l
;		}
