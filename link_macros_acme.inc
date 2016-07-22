		;load next file as a raw file (do not decomp on the fly) and jump to .addr
		!macro link_load_next_raw_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_raw
		}

		;load next file and decomp on the fly, then jump to .addr
		!macro link_load_next_comp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_comp
		}

		;decomp previously loaded file and jump to .addr
		!macro link_decomp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_decomp
		}

		;load next file and decompress on the fly, then load next file raw, disable IO and decompress it, then enable IO again -> a full file that was split into two parts is loaded, with a portion going under IO
		!macro link_load_next_double_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_double
		}

		;load file raw and decomp
		!macro link_load_next_raw_decomp_jmp .addr {
			lda #>(.addr-1)
			pha
			lda #<(.addr-1)
			pha
			jmp link_load_next_raw_decomp
		}

		;link irq hook back to base irq
		!macro link_player_irq {
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		}

		;request next disk side and reset filenum_counter
		!macro request_disk .num {
			lda #.num + $f0
			jsr bitfire_send_byte_
			lda #$3f
			sta $dd02
		}

		;XXX request disc

		;wait for given frame counter value
		!macro link_wait_syncpoint .part {
-
			lda link_syncpoint
			cmp #.part
			bcc -
		}

		!macro wait_frame_count .cnt {
-
			lda link_frame_count+0
			cmp #<.cnt
			lda link_frame_count+1
			sbc #>.cnt
			bcc -
		}

		!macro bus_lock {
			lda #$c7
			sta $dd02
		}

		!macro bus_unlock .bank {
			lda #(.bank & 3) | $c0
			sta $dd00
			lda #$3f
			sta $dd02
		}

		!macro set_depack_pointers .addr {
			lda #<.addr
			sta bitfire_lz_sector_ptr1
			sta bitfire_lz_sector_ptr2
			sta bitfire_lz_sector_ptr3
			lda #>.addr
			sta bitfire_lz_sector_ptr1 + 1
			sta bitfire_lz_sector_ptr2 + 1
			sta bitfire_lz_sector_ptr3 + 1
		}
