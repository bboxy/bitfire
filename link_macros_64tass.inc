
link_load_next_raw_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_raw
		.endm

link_load_next_comp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_comp
		.endm

link_decomp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_decomp
		.endm

link_load_next_double_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_double
		.endm

link_load_next_raw_decomp_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_load_next_raw_decomp
		.endm

link_player_irq .macro
		sei
		lda #<link_player
		sta $fffe
		lda #>link_player
		sta $ffff
		lda #$ff
		sta $d012
		cli
		.endm

request_disk	.macro
		lda #(\1) + $f0
		jsr bitfire_send_byte_
		lda #$3f
		sta $dd02
		.endm

link_wait_syncpoint .macro
l1		lda link_syncpoint
		cmp #\1
		bcc l1
		.endm

wait_frame_count .macro
l2		lda link_frame_count+0
		cmp #<(\1)
		lda link_frame_count+1
		sbc #>(\1)
		bcc l2
		.endm

bus_lock	.macro
		lda #$03
		sta $dd02
		.endm

bus_unlock	.bus_unlock
		lda #(\1 & 3) | $c0
		sta $dd00
		lda #$3f
		sta $dd02
		.endm
