		ifnconst link_dasm_inc
		link_dasm_inc = 1
		mac link_load_next_raw_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_raw
		endm

		mac link_load_next_comp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_comp
		endm

		mac link_decomp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_decomp
		endm

		mac link_load_next_double_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_double
		endm

		mac link_load_next_raw_decomp_jmp
			lda #>({1}-1)
			pha
			lda #<({1}-1)
			pha
			jmp link_load_next_raw_decomp
		endm

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

		mac request_disk
			lda #{1} + $f0
			jsr bitfire_send_byte_
			lda #$3f
			sta $dd02
		endm

		mac wait_syncpoint
.sau
			lda link_syncpoint
			cmp #{1}
			bcc .sau
		endm

		mac wait_frame_count
.sau			lda link_frame_count+1
			cmp #>{1}
			bcc .sau
.sau2
			lda link_frame_count+0
			cmp #<{1}
			bcc .sau2
		endm

		endif

		mac bus_lock
			lda #$c7
			sta $dd02
		endm

		mac bus_unlock
			lda #({1} & 3) | $c0
			sta $dd00
			lda #$3f
			sta $dd02
		endm

		mac start_music_nmi
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
			cmp $d012
			bne *-3

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
