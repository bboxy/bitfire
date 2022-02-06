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

LZ_BITS_LEFT		= 0

lz_bits			= CONFIG_ZP_ADDR + 0		;1 byte
lz_dst			= CONFIG_ZP_ADDR + 1		;2 byte
lz_src			= CONFIG_ZP_ADDR + 3		;2 byte
lz_len_hi		= CONFIG_ZP_ADDR + 5		;1 byte
preamble		= CONFIG_ZP_ADDR + 6		;5 bytes
barrier			= preamble + 3
filenum			= barrier

bitfire_load_addr_lo	= lz_src + 0
bitfire_load_addr_hi	= lz_src + 1

!macro get_lz_bit {
        !if LZ_BITS_LEFT = 1 {
			asl <lz_bits
        } else {
			lsr <lz_bits
        }
}

!macro set_lz_bit_marker {
        !if LZ_BITS_LEFT = 1 {
        	        rol
        } else {
	                ror
        }
}

!macro init_lz_bits {
	!if LZ_BITS_LEFT = 1 {
			lda #$40
			sta <lz_bits			;start with an empty lz_bits, first +get_lz_bit leads to literal this way and bits are refilled upon next shift
	} else {
			stx <lz_bits
	}
}

!macro inc_src_ptr {
			inc <lz_src + 1
}

bitfire_install_	= CONFIG_INSTALLER_ADDR	;define that label here, as we only aggregate labels from this file into loader_*.inc

			* = CONFIG_RESIDENT_ADDR
.loader_start
bitfire_loadraw_
bitfire_send_byte_
			sec
			ror
			sta <filenum
			lda #$3f
.ld_loop
			eor #$20
			tax				;$1f/$3f
			bcc +
			sbx #$10			;$0f/$2f
+
			jsr .ld_set_dd02		;waste lots of cycles upon write, so bits do not arrive to fast @floppy
			lsr <filenum
			bne .ld_loop
			tax				;x is always $3f after 8 rounds (8 times eor #$20)
-
			bit $dd00			;/!\ ATTENTION wait for drive to become busy, also needed, do not remove, do not try again to save cycles/bytes here :-(
			bmi -
.ld_set_dd02
			stx $dd02			;restore $dd02
.ld_pend
-
.ld_load_raw
			jsr .ld_pblock			;fetch all blocks until eof
			bcc -
			;rts				;XXX TODO can be omitted, maybe as we would skip blockloading on eof?
							;just run into ld_pblock code again that will then jump to .ld_pend and rts
.ld_pblock
			lda $dd00			;bit 6 is always set if not ready or idle/EOF so no problem with just an ASL
			asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
			bmi .ld_pend			;block ready?
.ld_pblock_
			ldy #$05			;5 bytes
			lda #$00			;set up target -> sta preamble,y
			ldx #<preamble
			jsr .ld_set_block_tgt		;load 5 bytes preamble - returns with C = 0 at times

			ldy <preamble + 0		;load blocklength
			ldx <preamble + 1		;block_address lo
			lda <preamble + 2		;block_address hi
			;lda <preamble + 3		;can be omitted directly copied to barrier val in zp!
			;sta <barrier
			bit <preamble + 4		;status -> first_block?
			bmi .ld_set_block_tgt
			stx bitfire_load_addr_lo	;yes, store load_address
			sta bitfire_load_addr_hi
.ld_set_block_tgt
			stx .ld_store + 1		;setup target for block data
			sta .ld_store + 2
			sec
							;lax would be a7, would need to swap carry in that case: eof == clc, block read = sec
			ldx #$8e			;opcode for stx	-> repair any rts being set (also accidently) by y-index-check
			top
.ld_en_exit
			ldx #$60
			stx .ld_gend			;XXX TODO would be nice if we could do that with ld_store in same time, but happens at different timeslots :-(
			bpl +				;do bpl first
bitfire_ntsc5
			bmi .ld_gentry			;also bmi is now in right place to be included in ntsc case to slow down by another 2 cycles. bpl .ld_gloop will then point here and bmi will just fall through always
.ld_gloop
			ldx #$3f
bitfire_ntsc0		ora $dd00 - $3f,x
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
bitfire_ntsc3		adc $dd00			;XXX $DD = CMP $xxxx,x  ;a is anything between 38 and 3b after add (37 + 00..03 + carry), so bit 3 and 4 is always set, bits 6 and 7 are given by floppy
							;%xx111xxx
.ld_gend
			stx $dd02			;carry is cleared now, we can exit here and do our rts with .ld_gend
			lsr				;%xxx111xx
			lsr				;%xxxx111x
bitfire_ntsc4		bpl .ld_gloop			;BRA, a is anything between 0e and 3e

.loader_end
!warn "loader size: ", * - .loader_start
;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

.lz_l_page
			dec <lz_len_hi
			bcs .lz_cp_lit

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_m_page
			lda #$ff				;much shorter this way. if we recalculate m_src and dst, endcheck also hits in if we end with an multipage match, else maybe buggy?
.lz_dcp								;.lz_dcp is entered with A = $ff, the only valid condition where dcp sets the carrx always
			dcp <lz_len_hi
			bcs .lz_match_len2			;as Y = 0, we can skip the part that does Y = A xor $ff

bitfire_decomp_
							;copy over end_pos and lz_dst from stream
			ldy #$00			;needs to be set in any case, also plain decomp enters here
			ldx #$02
			+init_lz_bits
-
			lda (lz_src),y
			sta <lz_dst + 0 - 1, x
			inc <lz_src + 0
			bne +
			+inc_src_ptr
+
			dex
			bne -
			stx .lz_offset_lo + 1		;initialize offset with $0000
			stx .lz_offset_hi + 1
			stx <lz_len_hi			;reset len - XXX TODO could also be cleared upon installer, as the depacker leaves that value clean again

			;------------------
			;LITERAL
			;------------------
link_decomp
.lz_start_over
			lda #$01			;we fall through this check on entry and start with literal
			+get_lz_bit
			bcs .lz_match			;after each match check for another match or literal?
.lz_literal
			jsr .lz_length
			tax
			beq .lz_l_page			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it returns on c = 1, what is always true after jsr .lz_length
.lz_cp_lit
			lda (lz_src),y			;/!\ Need to copy this way, or we run into danger to copy from an area that is yet blocked by barrier, this totally sucks, loading in order reveals that
			sta (lz_dst),y

			inc <lz_src + 0
			bne +
			+inc_src_ptr
+
			inc <lz_dst + 0
			bne +
			inc <lz_dst + 1
+
			dex
			bne .lz_cp_lit

			lda <lz_len_hi			;more pages to copy?
			bne .lz_l_page			;happens very seldom

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			rol				;was A = 0, C = 1 -> A = 1 with rol, but not if we copy literal this way
			+get_lz_bit
			bcs .lz_match			;either match with new offset or old offset

			;------------------
			;REPEAT LAST OFFSET
			;------------------
.lz_repeat
			jsr .lz_length

			sbc #$01
			bcc .lz_dcp			;fix highbyte of length in case and set carry again (a = $ff -> compare delivers carry = 1)
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
			eor #$ff			;restore A
.lz_match_len2						;entry from new_offset handling
			adc <lz_dst + 0
			sta <lz_dst + 0
			bcs .lz_clc			;/!\ branch happens very seldom, if so, clear carry
			dec <lz_dst + 1			;subtract one more in this case
.lz_clc_back
.lz_offset_lo		sbc #$00			;carry is cleared, subtract (offset + 1) in fact we could use sbx here, but would not respect carry, but a and x are same, but need x later anyway for other purpose
			sta .lz_msrcr + 0
			lax <lz_dst + 1
.lz_offset_hi		sbc #$00
			sta .lz_msrcr + 1
.lz_cp_match
			;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (lz_dst),y
			iny
			bne .lz_cp_match
			inx
			stx <lz_dst + 1			;cheaper to get lz_dst + 1 into x than lz_dst + 0 for upcoming compare

			lda <lz_len_hi			;check for more loop runs
			bne .lz_m_page			;do more page runs? Yes? Fall through
.lz_check_poll
			cpx <lz_src + 1			;check for end condition when depacking inplace, lz_dst + 0 still in X
.lz_skip_poll		bne .lz_start_over		;-> can be changed to .lz_poll, depending on decomp/loadcomp

			ldx <lz_dst + 0
			cpx <lz_src + 0
			bne .lz_start_over
.lz_eof
			rts
			;jmp .ld_load_raw		;but should be able to skip fetch, so does not work this way
			;top				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway
							;as last block is forced, we would always wait for last block to be loaded if we enter this loop, no matter how :-)
			;------------------
			;MATCH
			;------------------
.lz_match
			jsr .lz_length
			sbc #$01			;subtract 1, elias numbers range from 1..256, we need 0..255
			bcc .lz_eof			;underflow, so offset was $100

			lsr				;set bit 15 to 0 while shifting hibyte
			sta .lz_offset_hi + 1		;hibyte of offset

			lda (lz_src),y			;fetch another byte directly, same as refill_bits...
			ror				;and shift -> first bit for lenth is in carry, and we have %0xxxxxxx xxxxxxxx as offset
			sta .lz_offset_lo + 1

			inc <lz_src + 0			;postponed, so no need to save A on next_page call
			bne +
			+inc_src_ptr
+
			lda #$01
			ldy #$fe
			bcs .lz_match_len2		;length = 1 ^ $ff, do it the very short way :-)
			ldy #$00			;only now y = 0 is needed
			jsr .lz_get_loop
			bcs .lz_match_big		;and enter match copy loop

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax
			lda (lz_src),y
			+set_lz_bit_marker
			sta <lz_bits
			inc <lz_src + 0 		;postponed, so no need to save A on next_page call
			bne +
			+inc_src_ptr			;preserves carry and A, clears X, Y, all sane
+
			txa				;also postpone, so A can be trashed on lz_inc_src above
			bcs .lz_lend
.lz_get_loop
			+get_lz_bit			;fetch payload bit
.lz_length_16_
			rol				;can also moved to front and executed once on start
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.lz_length
			+get_lz_bit

			bcc .lz_get_loop
			beq .lz_refill_bits
.lz_lend
			rts
.lz_length_16						;happens very rarely
			pha				;save LSB
			tya				;was lda #$01, but A = 0 + upcoming rol makes this also start with MSB = 1
			jsr .lz_length_16_		;get up to 7 more bits
			sta <lz_len_hi			;save MSB
			pla				;restore LSB
			rts

!warn "depacker size: ", * - .loader_end
bitfire_resident_size = * - CONFIG_RESIDENT_ADDR

;XXX TODO
;decide upon 2 bits with bit <lz_bits? bmi + bvs + bvc? bpl/bmi decides if repeat or not, bvs = length 2/check for new bits and redecide, other lengths do not need to check, this can alos be used on other occasions?
;do a jmp ($00xx) to determine branch?
