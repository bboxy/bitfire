;if def link_c6510_inc == 0
;const link_c6510_inc = 1

		include "word.s"
		include "irq.s"

		macro m_link_load_next_raw_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_raw
		}

		macro m_link_load_next_comp_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_comp
		}

		macro m_link_decomp_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_decomp
		}

		macro m_link_load_next_double_jmp n:16
		{
			lda #(n-1)>>8
			pha
			lda #(n-1)&255
			pha
			jmp link_load_next_double
		}

                macro m_link_load_next_raw_decomp_jmp n:16
		{
                        lda #(n-1)>>8
                        pha
                        lda #(n-1)&256
                        pha
                        jmp link_load_next_raw_decomp
                }


		macro m_link_player_irq
		{
			sei
			lda #link_player&255
			sta $fffe
			lda #link_player>>8
			sta $ffff
			lda #$ff
			sta $d012
			cli
		}

		macro m_request_disk n:8
		{
			lda #n + $f0
			jsr bitfire.sendbyte
			lda #$3f
			sta $dd02
		}

                macro m_wait_syncpoint part:16
		{
.w:
			lda link_syncpoint
			cmp #part
			bcc .w
                }

		macro m_wait_frame_count cnt:16
		{
.w:
			lda link_frame_count+0
			cmp #(cnt & 255)
			lda link_frame_count+1
			sbc #(cnt >> 8)
			bcc .w
		}

		macro bus_lock {
			lda #$03
			sta $dd02
		}

		macro bus_unlock bank:8 {
			lda #(bank & 3) | $c0
			sta $dd00
		}

;endif
