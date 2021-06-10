;
; (c) Copyright 2021 by Tobias Bindhammer. All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * The name of its author may not be used to endorse or promote products
;       derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;

!convtab pet
!cpu 6510
!src "music.inc"
!src "config.inc"
!src "constants.inc"

.CHECK_EVEN		= 1
!if CONFIG_LOADER = 1 {
;loader zp-addresses
.filenum		= CONFIG_ZP_ADDR + 0
.barrier		= .filenum
	!if CONFIG_DECOMP = 0 {
bitfire_load_addr_lo	= .filenum			;in case of no loadcompd, store the hi- and lobyte of loadaddress separatedly
bitfire_load_addr_hi	= .filenum + 1
	} else {
bitfire_load_addr_lo	= .lz_src + 0
bitfire_load_addr_hi	= .lz_src + 1
	}
bitfire_errors		= CONFIG_ZP_ADDR + 1
}

!if CONFIG_DECOMP = 1 {
.lz_bits		= CONFIG_ZP_ADDR + 2
.lz_dst			= CONFIG_ZP_ADDR + 3
.lz_src			= CONFIG_ZP_ADDR + 5
.lz_len_hi		= CONFIG_ZP_ADDR + 7
}

bitfire_install_	= CONFIG_INSTALLER_ADDR	;define that label here, as we only aggregate labels from this file into loader_*.inc

			* = CONFIG_RESIDENT_ADDR

!if CONFIG_FRAMEWORK = 1 & CONFIG_FRAMEWORK_FRAMECOUNTER = 1 {
link_frame_count
			!word 0
}

!if CONFIG_NMI_GAPS = 1 {
!align 255,2
.lz_gap1
			nop
			nop
			nop
			nop
			nop
			nop
}

!if CONFIG_AUTODETECT = 1 {
link_chip_types
link_sid_type			;%00000001		;bit set = new, bit cleared = old
link_cia1_type			;%00000010
link_cia2_type			;%00000100
			!byte $00
}
!if CONFIG_FRAMEWORK = 1 {

	!if CONFIG_FRAMEWORK_BASEIRQ = 1 {
link_player
			pha
			tya
			pha
			txa
			pha
			inc $01				;should be save with $01 == $34/$35, except when music is @ >= $e000
	!if CONFIG_FRAMEWORK_MUSIC_NMI = 1 {
			lda $dd0d
	} else {
			dec $d019
	}
			jsr link_music_play
			dec $01

			pla
			tax
			pla
			tay
			pla
			rti
	}

link_music_play
	!if CONFIG_FRAMEWORK_FRAMECOUNTER = 1 {
			inc link_frame_count + 0
			bne +
			inc link_frame_count + 1
+
link_music_addr = * + 1
			jmp link_music_play_side1
	}
			;this is the music play hook for all parts that they should call instead of for e.g. jsr $1003, it has a variable music location to be called
			;and advances the frame counter if needed

			;those calls could be a macro, but they are handy to be jumped to so loading happens while having all mem free, and code is entered afterwards
;	!if CONFIG_DECOMP = 1 {
;;			;expect $01 to be $35
;		!if CONFIG_LOADER = 1 {
;
;			;XXX TODO not used much, throw out
;link_load_next_double
;			;loads a splitted file, first part up to $d000 second part under IO
;			jsr link_load_next_comp
;link_load_next_raw_decomp
;			jsr link_load_next_raw
;		}
;link_decomp_under_io
;			dec $01				;bank out IO
;			jsr link_decomp			;depack
;			inc $01				;bank in again
;			rts
;	}
}

!if CONFIG_LOADER = 1 {
			;XXX we do not wait for the floppy to be idle, as we waste enough time with depacking or the fallthrough on load_raw to have an idle floppy
bitfire_send_byte_
			ldx #$37
			stx $dd02

			sec
			ror
			sta .filenum
			ldx #$3f
			txa
.ld_loop
			and #$2f
			bcs +
			eor #$10
+
			eor #$20
			sta $dd02 - $3f,x
			pha				;/!\ ATTENTION needed more than ever with spin down and turn disc, do never remove again
			pla
			lsr <(.filenum - $3f),x		;fetch next bit from filenumber and waste cycles
			bne .ld_loop
-
			bit $dd00			;/!\ ATTENTION wait for drive to become busy, also needed, do not remove, do not try again to save cycles/bytes here :-(
			bmi -
			stx $dd02			;restore $dd02
			rts

	!if CONFIG_FRAMEWORK = 1 {
link_load_next_raw
			lda #BITFIRE_LOAD_NEXT
link_load_raw
	}

bitfire_loadraw_
			jsr bitfire_send_byte_		;easy, open...
-
.ld_load_raw
			jsr .ld_pblock			;fetch all blocks until eof
			bcc -
.ld_pend
			rts				;XXX TODO can be omitted, maybe as we would skip blockloading on eof?
							;just run into ld_pblock code again that will then jump to .ld_pend and rts
.ld_pblock
			lda $dd00			;bit 6 is always set if not ready or idle/EOF so no problem with just an ASL
			asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
			bmi .ld_pend			;block ready?
.ld_pblock_
			ldx #$60			;set rts
			jsr .bitfire_ack_		;start data transfer (6 bits of payload possible on first byte, as first two bits are used to signal block ready + no eof). Also sets an rts in receive loop
			php				;preserve flag
			;extract errors here:
			asr #$7f
			lsr
			adc bitfire_errors
			sta bitfire_errors

	!if CONFIG_DECOMP = 1 {				;decompressor only needs to be setup if there
			jsr .ld_get_byte		;fetch barrier
			sta .barrier
	}
.bitfire_load_block
			jsr .ld_get_byte		;fetch blockaddr hi
			sta .ld_store + 2		;where to place the block?
;.sau			sta $1000
;			inc .sau + 1
;			ldx .sau + 1
;			stx $0fff
			tay				;preserve value in Y
			jsr .ld_get_byte
			sta .ld_store + 1
							;lo/hi-1 in a/y for later use
			plp
			bmi .ld_skip_stax		;#$fc -> first block, fetch load-address
			iny				;increment, as last .ld_get_byte call decremented y by 1
			sta <bitfire_load_addr_lo
			sty <bitfire_load_addr_hi
.ld_skip_stax
			jsr .ld_get_byte		;fetch blocklen

			tay
			ldx #$99			;sta $xxxx,y
.bitfire_ack_
			stx .ld_store
.ld_get_byte
			ldx #$8e			;opcode for stx	-> repair any rts being set (also accidently) by y-index-check
			top				;top XXX TODO
.ld_en_exit
			ldx #$60
			stx .ld_gend			;XXX TODO would be nice if we could do that with ld_store in same time, but happens at different timeslots :-(
			bpl +				;do bpl first and waste another 2 cycles on loop entry, so that floppy manages to switch from preamble to send_data
bitfire_ntsc5
;.ld_gloop
			bmi .ld_gentry
.ld_gloop
			ldx #$3f
bitfire_ntsc0		ora $dd00 - $3f,x
;bitfire_ntsc0		ora $dd00
			stx $dd02
			lsr				;%xxxxx111
			lsr				;%xxxxxx11 1
			dey
			beq .ld_en_exit
+
			ldx #$37
bitfire_ntsc1		ora $dd00
			stx $dd02
			ror				;c = 1
			ror				;c = 1 a = %11xxxxxx
			ldx #$3f
			sax .ld_nibble + 1
bitfire_ntsc2		and $dd00
			stx $dd02

.ld_nibble		ora #$00
.ld_store		sta $b00b,y
.ld_gentry
			lax <CONFIG_LAX_ADDR
bitfire_ntsc3		adc $dd00			;a is anything between 38 and 3b after add (37 + 00..03 + carry), so bit 3 and 4 is always set, bits 6 and 7 are given by floppy
							;%xx111xxx
.ld_gend
			stx $dd02			;carry is cleared now, we can exit here and do our rts with .ld_gend
			lsr				;%xxx111xx
			lsr				;%xxxx111x
bitfire_ntsc4		bpl .ld_gloop			;BRA, a is anything between 0e and 3e

!if >* != >.ld_gloop { !error "getloop code crosses page!" }
;XXX TODO in fact the branch can also take 4 cycles if needed, ora $dd00 - $3f,x wastes one cycle anyway

}

;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

!if CONFIG_DECOMP = 1 {
bitfire_decomp_
link_decomp
	!if CONFIG_LOADER = 1 {
			lda #(.lz_start_over - .lz_skip_poll) - 2
			ldx #$60
			bne .loadcomp_entry
		!if CONFIG_FRAMEWORK = 1 {
link_load_next_comp
			lda #BITFIRE_LOAD_NEXT
link_load_comp
		}
bitfire_loadcomp_
			jsr bitfire_send_byte_		;returns now with x = $3f
			lda #(.lz_poll - .lz_skip_poll) - 2
			ldx #$08
.loadcomp_entry
			sta .lz_skip_poll + 1
			stx .lz_skip_fetch

			jsr .lz_next_page_		;shuffle in data first until first block is present, returns with Y = 0, X = 0
	}
							;copy over end_pos and lz_dst from stream
			ldy #$00			;needs to be set in any case, also plain decomp enters here
			jsr .lz_get_byte		;y will stay 0
			sta <.lz_dst + 1
			jsr .lz_get_byte		;y will stay 0
			sta <.lz_dst + 0

			sty .lz_offset_lo + 1		;initialize offset with $0000
			sty .lz_offset_hi + 1
			sty <.lz_len_hi			;reset len - XXX TODO could also be cleared upon installer, as the depacker leaves that value clean again

			lda #$40			;start with an empty lz_bits, first asl <.lz_bits leads to literal this way and bits are refilled upon next shift
			sta <.lz_bits
			bne .lz_start_over		;start with a literal

			;------------------
			;SELDOM STUFF
			;------------------
.lz_l_page
			dec <.lz_len_hi
			bcs .lz_cp_lit

			;------------------
			;GET BYTE FROM STREAM
			;------------------
.lz_get_byte
			lda (.lz_src),y
			inc <.lz_src + 0
			beq .lz_next_page
			rts

	!if CONFIG_NMI_GAPS = 1 {
			!ifdef .lz_gap2 {
				!warn .lz_gap2 - *, " bytes left until gap2"
			}
!align 255,2
.lz_gap2
!if .lz_gap2 - .lz_gap1 > $0100 {
		!error "code on first page too big, second gap does not fit!"
}
			nop
			nop
			nop
	}
			;------------------
			;POINTER HANDLING LITERAL COPY
			;------------------
.lz_dst_inc
			inc <.lz_dst + 1
			bcs .lz_dst_inc_
.lz_src_inc
	!if CONFIG_LOADER = 1 {
			jsr .lz_next_page		;sets X = 0, so all sane
	} else {
			inc <.lz_src + 1
	}
			bcs .lz_src_inc_

			;------------------
			;POLLING
			;------------------
.lz_poll
	!if CONFIG_LOADER = 1 {
			bit $dd00
			bvs .lz_start_over
			jsr .ld_pblock_			;yes, fetch another block, call is disabled for plain decomp
	}
			;------------------
			;LITERAL
			;------------------
.lz_start_over
			lda #$01			;we fall through this check on entry and start with literal
			asl <.lz_bits
			bcs .lz_match			;after each match check for another match or literal?
.lz_literal
			jsr .lz_length
			tax
			beq .lz_l_page			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it returns on c = 1, what is always true after jsr .lz_length
.lz_cp_lit
			lda (.lz_src),y			;Need to copy this way, or wie copy from area that is blocked by barrier
			sta (.lz_dst),y

			inc <.lz_src + 0
			beq .lz_src_inc
.lz_src_inc_
			inc <.lz_dst + 0
			beq .lz_dst_inc
.lz_dst_inc_
			dex
			bne .lz_cp_lit

			lda <.lz_len_hi			;more pages to copy?
			bne .lz_l_page			;happens very seldom

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			rol				;A = 0, C = 1 -> A = 1
			asl <.lz_bits
			;rol
			;bne .lz_match
			;else A = 0
			;but only for lowbyte?!
			bcs .lz_match			;either match with new offset or old offset

			;------------------
			;DO MATCH
			;------------------
.lz_repeat
			jsr .lz_length
			sbc #$01
			sec				;XXX TODO in fact we could save on the sbc #$01 as the sec and adc later on corrects that again, but y would turn out one too less
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
							;XXX TODO save on eor #$ff and do sbc lz_dst + 0?
			eor #$ff			;restore A
.lz_match_len2						;entry from new_offset handling
			adc <.lz_dst + 0
			sta <.lz_dst + 0
			tax				;remember for later end check, cheaper this way
			bcs .lz_clc			;/!\ branch happens very seldom, if so, clear carry
			dec <.lz_dst + 1		;subtract one more in this case
.lz_clc_back
.lz_offset_lo		sbc #$00			;carry is cleared, subtract (offset + 1) in fact we could use sbx here, but would not respect carry, but a and x are same, but need x later anyway for other purpose
			sta .lz_msrcr + 0
			lda <.lz_dst + 1
.lz_offset_hi		sbc #$00
			sta .lz_msrcr + 1
			;				;XXX TODO would have dst + 0 and + 1 in X and A here, of any use?
.lz_cp_match
			;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (.lz_dst),y
			iny
			bne .lz_cp_match
			inc <.lz_dst + 1

			lda <.lz_len_hi			;check for more loop runs
			bne .lz_m_page			;do more page runs? Yes? Fall through
.lz_check_poll
			cpx <.lz_src + 0		;check for end condition when depacking inplace, .lz_dst + 0 still in X
!if .CHECK_EVEN = 1 {
.lz_skip_poll		bne .lz_start_over		;-> can be changed to .lz_poll, depending on decomp/loadcomp
}
			lda <.lz_dst + 1
			sbc <.lz_src + 1
!if .CHECK_EVEN = 1 {
			bne .lz_start_over
} else {
.lz_skip_poll		bcc .lz_start_over
}
			;jmp .ld_load_raw		;but should be able to skip fetch, so does not work this way
			;top				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway
							;as last block is forced, we would always wait for last block to be loaded if we enter this loop, no matter how :-)

			;------------------
			;NEXT PAGE IN STREAM
			;------------------
.lz_next_page
			inc <.lz_src + 1
.lz_next_page_
	!if CONFIG_LOADER = 1 {
.lz_skip_fetch
			php				;save carry
			pha				;and A
			txa
			pha
.lz_fetch_sector					;entry of loop
			jsr .ld_pblock			;fetch another block
			bcs .lz_fetch_eof		;eof? yes, finish, only needed if files reach up to $ffxx -> barrier will be 0 then and upcoming check will always hit in -> this would suck
							;XXX TODO send a high enough barrier on last block being sent
			lda <.lz_src + 1		;get current depack position
			cmp <.barrier			;next pending block/barrier reached? If barrier == 0 this test will always loop on first call or until first-block with load-address arrives, no matter what .bitfire_lz_sector_ptr has as value \o/
							;on first successful .ld_pblock they will be set with valid values and things will be checked against correct barrier
			bcs .lz_fetch_sector		;already reached, loop
.lz_fetch_eof						;not reached, go on depacking
			;Y = 0				;XXX TODO could be used to return somewhat dirty from a jsr situation, this would pull two bytes from stack and return
			pla
			tax
			pla
			plp
	}
			rts

			;------------------
			;FETCH A NEW OFFSET
			;------------------
-							;lz_length as inline
			asl <.lz_bits			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_match
			asl <.lz_bits
			bcc -

			bne +
			jsr .lz_refill_bits
+
			sbc #$01			;XXX TODO can be omitted if just endposition is checked, but 0 does not exist as value?
			bcc .lz_eof			;underflow. must have been 0

			lsr
			sta .lz_offset_hi + 1		;hibyte of offset

			lda (.lz_src),y			;fetch another byte directly
			ror
			sta .lz_offset_lo + 1

			inc <.lz_src + 0		;postponed, so no need to save A on next_page call
			bne +
	!if CONFIG_LOADER = 1 {
			jsr .lz_next_page		;preserves carry, all sane
	} else {
			inc <.lz_src + 1
	}
+
			lda #$01
			ldy #$fe
			bcs .lz_match_len2		;length = 1 ^ $ff, do it the very short way :-)
-
			asl <.lz_bits			;fetch first payload bit
							;XXX TODO we could check bit 7 before further asl?
			rol				;can also moved to front and executed once on start
			asl <.lz_bits
			bcc -
			bne .lz_match_big
			ldy #$00			;only now y = 0 is needed
			jsr .lz_refill_bits		;fetch remaining bits
			bcs .lz_match_big

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_m_page
			dec <.lz_len_hi
			inc .lz_msrcr + 1		;XXX TODO only needed if more pages follow
			bne .lz_cp_match

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax
			lda (.lz_src),y
			rol
			sta <.lz_bits
			inc <.lz_src + 0 		;postponed, so no need to save A on next_page call
			bne +				;XXX TODO if we would prefer beq, 0,2% saving
	!if CONFIG_LOADER = 1 {
			jsr .lz_next_page		;preserves carry and A, clears X, Y, all sane
	} else {
			inc <.lz_src + 1
	}
+
			txa
			bcs .lz_lend

			;lda #$00
			;slo <.lz_bits
.lz_get_loop
			asl <.lz_bits			;fetch payload bit
.lz_length_16_
			rol				;can also moved to front and executed once on start
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.lz_length
			asl <.lz_bits

			bcc .lz_get_loop
			beq .lz_refill_bits
.lz_lend
.lz_eof
			rts
.lz_length_16						;happens very rarely
			pha				;save LSB
			tya				;was lda #$01, but A = 0 + rol makes this also start with MSB = 1
			jsr .lz_length_16_		;get up to 7 more bits
			sta <.lz_len_hi			;save MSB
			pla				;restore LSB
			rts
}

bitfire_resident_size = * - CONFIG_RESIDENT_ADDR

;set end_address for inplace to real end of file if not inplaced? so nothing can go wrong? or set it to $ffff? also sane

;XXX TODO
;decide upon 2 bits with bit <.lz_bits? bmi + bvs + bvc? bpl/bmi decides if repeat or not, bvs = length 2/check for new bits and redecide, other lengths do not need to check, this can alos be used on other occasions?
;do a jmp ($00xx) to determine branch?
