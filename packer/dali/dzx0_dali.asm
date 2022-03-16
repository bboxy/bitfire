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

!cpu 6510

LZ_BITS_LEFT		= 0
INPLACE			= 0
CONFIG_ZP_ADDR		= $f0

lz_bits			= CONFIG_ZP_ADDR + 0		;1 byte
lz_dst			= CONFIG_ZP_ADDR + 1		;2 byte
lz_src			= CONFIG_ZP_ADDR + 3		;2 byte
lz_len_hi		= CONFIG_ZP_ADDR + 5		;1 byte

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

!macro inc_src_ptr {
			inc <lz_src + 1
}

depack
			stx <lz_src + 0
			sta <lz_src + 1

			ldy #$00			;needs to be set in any case, also plain decomp enters here
			sty .lz_offset_lo + 1		;initialize offset with $0000
			sty .lz_offset_hi + 1
			lda #$01
			sec				;needs to be set, is set after send_byte but not if entering via link_decomp
			bcs .lz_start_depack		;start with a literal, X = 0, still annoying

			;------------------
			;SELDOM STUFF
			;------------------
.lz_inc_src3
			+inc_src_ptr
			bcs .lz_inc_src3_
.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_cp_page
			txa
.lz_cp_page_
			dec <lz_len_hi
			bne +
			jsr .disable
+
			tax
			beq .lz_l_page			;as Y = 0, we can skip the part that does Y = A xor $ff
			tya				;if entered from a match, x is anything between $01 and $ff due to inx stx <lz_dst + 1, except if we would depack to zp?
			beq .lz_m_page			;as Y = 0, we can skip the part that does Y = A xor $ff

			;------------------
			;LITERAL
			;------------------
-
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_literal
			+get_lz_bit			;fetch payload bit
			bcc -
+
			bne +
.lz_start_depack
			jsr .lz_refill_bits
			beq .lz_cp_page_
+
			tax
.lz_l_page
.lz_cp_lit						;XXX TODO copy with Y += 1 but have lz_src + 0 eor #$ff in x and countdown x, so that lz_src + 1 can be incremented in time?
			lda (lz_src),y			;/!\ Need to copy this way, or we run into danger to copy from an area that is yet blocked by barrier, this totally sucks, loading in order reveals that
			sta (lz_dst),y

			inc <lz_src + 0
			beq .lz_inc_src3
.lz_inc_src3_
			inc <lz_dst + 0
			beq .lz_dst_inc
.lz_dst_inc_
			dex
			bne .lz_cp_lit
							;XXX TODO we could also just manipulate a branch and make it go to either page handling or fall through? enable branch if 16 bits are fetched and lz_len  > 0? dop or bcs to disable and enable, 80 or b0
.lz_set1		bcc .lz_cp_page

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;XXX TODO fetch length first and then decide if literal, match, repeat? But brings our checks for last bit to the end? need to check then on typebit? therefore entry for fetch is straight?
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			lda #$01			;was A = 0, C = 1 -> A = 1 with rol, but not if we copy literal this way
			+get_lz_bit
			bcs .lz_match

			;------------------
			;REPEAT LAST OFFSET
			;------------------
.lz_repeat
			+get_lz_bit
			bcs +
-
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			+get_lz_bit			;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcc -
+
			bne +
			jsr .lz_refill_bits		;fetch more bits
			beq .lz_cp_page			;XXX TODO sec after sbc #1 is also sufficient, but slower
+
			sbc #$01			;subtract 1, will be added again on adc as C = 1
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
.lz_m_page
			eor #$ff			;restore A

			adc <lz_dst + 0			;add length
			sta <lz_dst + 0
			bcs .lz_clc			;/!\ branch happens very seldom, if so, clear carry, XXX TODO if we would branch to * + 3 and we use $18 as lz_dst, we have our clc there :-( but we want zp usage to be configureable
			dec <lz_dst + 1			;subtract one more in this case
.lz_clc_back
.lz_offset_lo		sbc #$00			;carry is cleared, subtract (offset + 1)
			sta .lz_msrcr + 0
			lax <lz_dst + 1
.lz_offset_hi		sbc #$00
			sta .lz_msrcr + 1
.lz_cp_match						;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (lz_dst),y
			iny
			bne .lz_cp_match
			inx
			stx <lz_dst + 1			;cheaper to get lz_dst + 1 into x than lz_dst + 0 for upcoming compare

.lz_set2		bcc .lz_cp_page
!if INPLACE = 1 {
			cpx <lz_src + 1			;check for end condition when depacking inplace, lz_dst + 0 still in X
			bne .lz_start_over		;-> can be changed to .lz_poll, depending on decomp/loadcomp

			lda <lz_dst + 0
			eor <lz_src + 0
			bne .lz_start_over
			rts
} else {
}
			;------------------
			;ENTRY POINT DEPACKER
			;------------------
.lz_start_over
			lda #$01			;we fall through this check on entry and start with literal
			+get_lz_bit
			bcc .lz_literal

			;------------------
			;MATCH
			;------------------
.lz_match
			+get_lz_bit
			bcs +
-
			+get_lz_bit			;fetch payload bit
			rol				;add bit to number
			+get_lz_bit			;fetch control bit
			bcc -				;not yet done, fetch more bits
+
			bne +				;last bit or bitbuffer empty? fetched 1 to 4 bits now
			jsr .lz_refill_bits		;refill bitbuffer
			beq .disable			;so offset was $100 as lowbyte is $00, only here 4-8 bits are fetched
+
			sbc #$01			;subtract 1, elias numbers range from 1..256, we need 0..255
			lsr				;set bit 15 to 0 while shifting hibyte
			sta .lz_offset_hi + 1		;hibyte of offset

			lda (lz_src),y			;fetch another byte directly, same as refill_bits...
			ror				;and shift -> first bit for lenth is in carry, and we have %0xxxxxxx xxxxxxxx as offset
			sta .lz_offset_lo + 1		;lobyte of offset

			inc <lz_src + 0			;postponed, so no need to save A on next_page call
			beq .lz_inc_src1
.lz_inc_src1_
			lda #$01			;fetch new number, start with 1
			bcs .lz_match_big		;length = 1, do it the very short way
-
			+get_lz_bit			;fetch more bits
			rol
			+get_lz_bit
			bcc -
			bne .lz_match_big
			jsr .lz_refill_bits		;fetch remaining bits
;.lz_jsr_addr = * - 1
;			bcs .lz_match_big		;lobyte != 0?
			bcs .lz_match_big		;lobyte != 0? If zero, fall through

			;------------------
			;SELDOM STUFF
			;------------------

.lz_inc_src1
			+inc_src_ptr
			bne .lz_inc_src1_
.lz_inc_src2
			+inc_src_ptr
			bne .lz_inc_src2_

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax				;save bits fetched so far
			lda (lz_src),y			;fetch another lz_bits byte from stream
			+set_lz_bit_marker
			sta <lz_bits
			inc <lz_src + 0 		;postponed pointer increment, so no need to save A on next_page call
			beq .lz_inc_src2
.lz_inc_src2_
			txa				;restore fetched bits, also postponed, so A can be trashed on lz_inc_src above
			bcs .lz_lend			;last bit fetched?
.lz_get_loop
			+get_lz_bit			;fetch payload bit
.lz_length_16_
			rol				;shift in new payload bit
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
			+get_lz_bit			;fetch control bit
			bcc .lz_get_loop		;need more bits
			beq .lz_refill_bits		;buffer is empty, fetch new bits
.lz_lend						;was the last bit
			rts
.lz_length_16						;this routine happens very rarely, so we can waste cycles
			pha				;save so far received lobyte
			tya				;was lda #$01, but A = 0 + upcoming rol makes this also start with A = 1
			jsr .lz_length_16_		;get up to 7 more bits
			sta <lz_len_hi			;and save hibyte
			ldx #$b0
			pla				;restore lobyte
			top
.disable
			ldx #$80
.enable
			stx .lz_set1
			stx .lz_set2
			rts
