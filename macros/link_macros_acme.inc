		;load next file as a raw file (do not decomp on the fly) and jump to .addr
		!macro link_load_next_raw_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_load_next_raw
		}

		;load next file and decomp on the fly, then jump to .arg
		!macro link_load_next_comp_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_load_next_comp
		}

		;decomp previously loaded file and jump to .arg
		!macro link_decomp_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_decomp
		}

		;load next file and decompress on the fly, then load next file raw, disable IO and decompress it, then enable IO again -> a full file that was split into two parts is loaded, with a portion going under IO
		!macro link_load_next_double_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_load_next_double
		}

		;load file raw and decomp
		!macro link_load_next_raw_decomp_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_load_next_raw_decomp
		}

		!macro link_decomp_under_io_jmp .arg {
			lda #>(.arg-1)
			pha
			lda #<(.arg-1)
			pha
			jmp link_decomp_under_io
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

		!macro reset_drive {
			lda #BITFIRE_RESET_DRIVE
			jsr bitfire_send_byte_
		}

		!macro wait_floppy_busy {
			;wait until floppy is busy
.l
			bit $dd00
			bmi .l
		}

		!macro wait_floppy_idle {
			;wait until floppy is idle again
.l
			bit $dd00
			bpl .l
		}

		;request next disk side and reset filenum_counter
		!macro request_disk .arg {
			lda #.arg + $f0
			jsr bitfire_send_byte_
			+wait_floppy_busy
			;lda #$3f
			;sta $dd02
			+wait_floppy_idle
		}

		;wait for given frame counter value
		!macro link_wait_syncpoint .arg {
.l
			lda link_syncpoint
			cmp #.arg
			bcc .l
		}

		!macro setup_sync .arg {
			lda #$00
			sta link_frame_count + 0	;avoid that the counter wraps worstcase, as we can't write atomic
			lda #<($8000 - .arg)
			sta link_frame_count + 0	;now setup counter
			lda #>($8000 - .arg)
			sta link_frame_count + 1
		}

		!macro sync {
.l
			lda link_frame_count + 1	;Counter expired when $8000 is reached
			bpl .l
		}

		!macro bus_lock {
			;XXX TODO ATTENTION /!\ better don't touch ever again
			lda #$17			;set ATN
			sta $dd02
		}

		!macro bus_unlock .arg {
			;XXX TODO ATTENTION /!\ better don't touch ever again
			;this looks ugly, sorry for this, but it is yet the only way it works on THCM's sx64, the trailing stx $dd00/sta $dd00 is needed, either it is hardware vodoo or timing taht is fixed by it :-(
			ldx #(.arg & 3)
			stx $dd00
			lda $dd00
			lda #$1f			;release ATN
			sta $dd02
			stx $dd00
			lda $dd00
			lda #$3f			;release DATA
			sta $dd02
			stx $dd00
			lda $dd00
			;give some time for things to settle, no idea how much time is needed, leaving this out makes sx64 also fail
			ldx #$07
.l
			dex
			bne .l
		}

		!macro set_depack_pointers .arg {
			lda #<.arg
			sta bitfire_load_addr_lo
			lda #>.arg
			sta bitfire_load_addr_hi
		}

		!macro start_music_nmi {
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
		}

		!macro stop_music_nmi {
			lda #$7f
			sta $dd0d
			lda $dd0d
		}

		!macro restart_music_nmi {
			lda $dd0d
			lda #$81
			sta $dd0d
		}
