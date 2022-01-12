link_decomp_under_io .macro
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
.endm

link_load_next_raw_decomp .macro
			jsr link_load_next_raw
			dec $01                         ;bank out IO
			jsr link_decomp                 ;depack
			inc $01
.endm

		;load next file as a raw file (do not decomp on the fly) and jump to .addr
link_load_next_raw_jmp .macro
			lda #>(\1-1)
			pha
			lda #<(\1-1)
			pha
			jmp link_load_next_raw
.endm

		;load next file and decomp on the fly, then jump to .arg
link_load_next_comp_jmp .macro
			lda #>(\1-1)
			pha
			lda #<(\1-1)
			pha
			jmp link_load_next_comp
.endm

		;decomp previously loaded file and jump to .arg
link_decomp_jmp .macro
			lda #>(\1-1)
			pha
			lda #<(\1-1)
			pha
			jmp link_decomp
.endm

link_load_next_double .macro
			jsr link_load_next_comp
			jsr link_load_next_raw
			dec $01
			jsr link_decomp
			inc $01
.endm

		;link irq hook back to base irq
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

reset_drive .macro
			lda #BITFIRE_RESET
			jsr bitfire_send_byte_
.endm

wait_floppy_busy .macro
			;wait until floppy is busy
l
			bit $dd00
			bmi l
.endm

wait_floppy_idle .macro
			;wait until floppy is idle again
l
			bit $dd00
			bpl l
.endm

		;request next disk side and reset filenum_counter
request_disk .macro
			lda #\1 + $f0
			jsr bitfire_send_byte_
			pha
			pla
			pha
			pla
			;+wait_floppy_busy
			;lda #$3f
			;sta $dd02
			wait_floppy_idle
.endm

		;wait for given frame counter value
link_wait_syncpoint .macro
l
			lda link_syncpoint
			cmp #\1
			bcc l
.endm

setup_sync .macro
			lda #$00
			sta link_frame_count + 0	;avoid that the counter wraps worstcase, as we can't write atomic
			lda #<($8000 - .arg)
			sta link_frame_count + 0	;now setup counter
			lda #>($8000 - .arg)
			sta link_frame_count + 1
.endm

sync .macro
l
			lda link_frame_count + 1	;Counter expired when $8000 is reached
			bpl l
.endm

bus_lock .macro
			lda #$37			;set ATN and CLK
			sta $dd02
.endm

bus_unlock .macro
			ldx #(\1 & 3) + $c0
			stx $dd00

			lda #$3f			;release CLK
			sta $dd02

			pha
			pla
			pha
			pla
			pha
			pla
.endm

set_depack_pointers .macro
			lda #<\1
			sta bitfire_load_addr_lo
			lda #>\1
			sta bitfire_load_addr_hi
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
l
			cmp $d012
			bne l

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