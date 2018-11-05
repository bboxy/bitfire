#ifndef link_dream_inc
		link_dream_inc = 1

		#macro link_load_next_raw_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_raw
		}

		#macro link_load_next_comp_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_comp
		}

		#macro link_decomp_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_decomp
		}

		#macro link_decomp_under_io_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_decomp_under_io
		}

		#macro link_load_next_double_jmp(addr)
		{
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_double
		}

		#macro link_load_next_raw_decomp_jmp(addr) {
			lda #>({addr}-1)
			pha
			lda #<({addr}-1)
			pha
			jmp link_load_next_raw_decomp
                }

		#macro link_player_irq
		{
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		}

		#macro request_disk(num) {
			lda #{num} + $f0
			jsr bitfire_send_byte_
			lda #$3f
			sta $dd02
		}

		#macro wait_syncpoint(part)
		{
.(
sau:
			lda link_syncpoint
			cmp #part
			bcc sau
.)
		}

		#macro clear_frame_count
		{
			lda #$00
			sta link_frame_count + 0
			sta link_frame_count + 1
		}

		#macro wait_frame_count(cnt)
		{
.(
sau1:
			lda link_frame_count+0
			cmp #<{cnt}
			bcc sau1
sau2:
			lda link_frame_count+1
			cmp #>{cnt}
			bcc sau2
.)
		}

		#macro bus_lock
		{
			lda #$c7
			sta $dd02
		}

		#macro bus_unlock(bank)
		{
			lda #({bank} & 3) | $c0
			sta $dd00
			lda #$3f
			sta $dd02
		}
		#macro start_music_nmi()
		{
.(
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
l:
			cmp $d012
			bne l

			lda #$11
			sta $dd0e
.)
		}

#endif
