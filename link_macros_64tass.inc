
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

link_decomp_under_io_jmp .macro
		lda #>(\1)-1
		pha
		lda #<(\1)-1
		pha
		jmp link_decomp_under_io
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

clear_frame_count .macro
		lda #$00
		sta link_frame_count + 0
		sta link_frame_count + 1
		.endm

wait_frame_count .macro
l2		lda link_frame_count+0
		cmp #<(\1)
		lda link_frame_count+1
		sbc #>(\1)
		bcc l2
		.endm

bus_lock	.macro
		lda #$c7
		sta $dd02
		.endm

bus_unlock	.macro
		lda #(\1 & 3) | $c0
		sta $dd00
		lda #$3f
		sta $dd02
		.endm

set_depack_pointers .macro
		lda #<(\1)
		sta bitfire_lz_sector_ptr1
		sta bitfire_lz_sector_ptr2
		lda #>(\1)
		sta bitfire_lz_sector_ptr1 + 1
		sta bitfire_lz_sector_ptr2 + 1
		.endm

start_music_nmi .macro
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
l1		cmp $d012
		bne l1

		lda #$11
		sta $dd0e
		.endm

stop_music_nmi .macro
		lda #$7f
		sta $dd0d
		lda $dd0d
		.endm

restart_music_nmi .macro
		lda $dd0d
		lda #$81
		sta $dd0d
		.endm

