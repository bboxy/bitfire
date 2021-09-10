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

;---------------------------------------------------------------------------------
;EXAMPLE FOR USAGE
;---------------------------------------------------------------------------------

;			;set up data source
;			lda #<.packed_data
;			sta .lz_src + 0
;			lda #>.packed_data
;			sta .lz_src + 1
;			;depack
;			jsr link_decomp
;.packed_data
;			;our packed data
;			!bin "packed_data.zx0"

;---------------------------------------------------------------------------------
;VARIABLES
;---------------------------------------------------------------------------------

!convtab pet
!cpu 6510

			* = $1000
CONFIG_ZP_ADDR		= $02

.CHECK_EVEN		= 1

.lz_bits		= CONFIG_ZP_ADDR + 0
.lz_dst			= CONFIG_ZP_ADDR + 1
.lz_src			= CONFIG_ZP_ADDR + 3
.lz_len_hi		= CONFIG_ZP_ADDR + 5

;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

bitfire_decomp_
link_decomp
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
			sec				;only needs to be set for consecutive rounds of literals, happens very seldom
			ldy #$00
.lz_l_page_
			dec <.lz_len_hi
			bcs .lz_cp_lit

			;------------------
			;GET BYTE FROM STREAM
			;------------------
.lz_get_byte
			lda (.lz_src),y
			inc <.lz_src + 0
			bne +
			inc <.lz_src + 1
+
			rts

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
			beq .lz_l_page_			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it returns on c = 1, what is always true after jsr .lz_length
.lz_cp_lit
			lda (.lz_src),y			;looks expensive, but is cheaper than loop
			sta (.lz_dst),y
			iny
			dex
			bne .lz_cp_lit

			dey				;this way we force increment of lz_src + 1 if y = 0
			tya				;carry is still set on first round
			adc <.lz_dst + 0
			sta <.lz_dst + 0		;XXX TODO final add of y, could be combined with next add? -> postpone until match that will happen necessarily later on? but this could be called mutliple times for several pages? :-(
			bcc +				;XXX TODO branch out and reenter
			inc <.lz_dst + 1
+
			tya
			sec				;XXX TODO meh, setting carry ...
			adc <.lz_src + 0
			sta <.lz_src + 0
			bcc +
			inc <.lz_src + 1
+
			ldy <.lz_len_hi			;more pages to copy?
			bne .lz_l_page			;happens very seldom

			;------------------
			;NEW OR OLD OFFSET
			;------------------
							;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			lda #$01			;same code as above, meh
			asl <.lz_bits
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
.lz_skip_poll		bne .lz_start_over			;we could check against src >= dst XXX TODO
}
			lda <.lz_dst + 1
			sbc <.lz_src + 1
!if .CHECK_EVEN = 1 {
			bne .lz_start_over
} else {
.lz_skip_poll		bcc .lz_start_over
}
			;jmp .ld_load_raw		;but should be able to skip fetch, so does not work this way
			rts				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway

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
			inc <.lz_src + 1
+
			lda #$01
			ldy #$fe
			bcs .lz_match_len2		;length = 2 ^ $ff, do it the very short way :-)
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
			txa
			inc <.lz_src + 0 		;postponed, so no need to save A on next_page call
			bne +				;XXX TODO if we would prefer beq, 0,2% saving
			inc <.lz_src + 1
+
			bcs .lz_lend


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

