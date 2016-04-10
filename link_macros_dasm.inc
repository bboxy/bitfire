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
.sau
			lda link_frame_count+0
			cmp #<{1}
			lda link_frame_count+1
			sbc #>{1}
			bcc .sau
		endm

		endif

