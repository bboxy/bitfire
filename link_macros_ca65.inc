		.macro link_load_next_raw_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_raw
		.endmacro

		.macro link_load_next_comp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_comp
		.endmacro

		.macro link_decomp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_decomp
		.endmacro

		.macro link_load_next_double_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_double
		.endmacro

		.macro link_load_next_raw_decomp_jmp addr
			lda #>(addr-1)
			pha
			lda #<(addr-1)
			pha
			jmp link_load_next_raw_decomp
		.endmacro

		.macro link_player_irq
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		.endmacro

		.macro request_disk num
			lda #num + $f0
			jsr bitfire_send_byte_
			lda #$3f
			sta $dd02
		.endmacro

		.macro link_wait_syncpoint part
                .local l
l
			lda link_syncpoint
			cmp #part
			bcc l
		.endmacro

		.macro wait_frame_count cnt
                .local l
l
			lda link_frame_count+0
			cmp #<cnt
			lda link_frame_count+1
			sbc #>cnt
			bcc l
		.endmacro

		.macro bus_lock
			lda #$37
			sta $dd02
		.endmacro

		.macro bus_unlock bank
                .local l
			ldx #(bank & 3)
			stx $dd00
			lda #$3f
			sta $dd02
			stx $dd00

			ldy #$00
l
			dey
			bne l
		.endmacro
