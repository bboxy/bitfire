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

.depacker	= $01
.smc_offsetd 	= .depacker - (.depacker_start - .zx0_code_start)
;ZX0_SRC_HI1	= .lz_src1     - .smc_offsetd + 2
;ZX0_SRC_HI2	= .lz_src2     - .smc_offsetd + 2
;ZX0_SRC_HI3	= .lz_src3     - .smc_offsetd + 2
;ZX0_SRC_LO	= .lz_src_lo   - .smc_offsetd + 2
ZX0_SRC		= .lz_src - .smc_offsetd + 2
ZX0_DST		= .lz_dst      - .smc_offsetd + 2
ZX0_SFX_ADDR	= .lz_sfx_addr - .smc_offsetd + 2
ZX0_DATA_END 	= .lz_data_end      - .zx0_code_start + 2
ZX0_DATA_SIZE_HI = .lz_data_size_hi - .zx0_code_start + 2

		* = $0801
.zx0_code_start
                !byte $0b,$08
		!word 1602
		!byte $9e
		!text "2061"
		!byte $00,$00,$00

		;/!\ ATTENTION, the depacker just fits into ZP this way, if it gets larger, the copy routine will overwrite $00, as it is a 8-bit address sta
		sei
		lda $ba
		pha
		ldx #.depacker_end - .depacker_start
-
		lda .depacker_start - 1,x
		sta .depacker - 1,x
		dex
		bne -

.lz_data_size_hi = * + 1
                ldy #>(.data_end - .data) + 1
-
                dex
.lz_data_end = * + 1
.src		lda .data_end - $100,x
.dst		sta $ff00,x
                txa
                bne -

                dec .src + 2
                dec .dst + 2
                dey
                bne -
                jmp .depack

		;------------------
		;depacker starts here
		;------------------
.depacker_start
!pseudopc .depacker {
		!byte $38
.depack
		;ldy #$00			;is already 0
		;------------------
		;LITERAL
		;------------------
.lz_start_over
		lda #$01			;we fall through this check on entry and start with literal
		asl <.lz_bits
		bcs .lz_new_offset		;after each match check for another match or literal?
.literal
		jsr .get_length
		tax
		beq .lz_l_page_
;		dec <.lz_len_hi			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it sets carry and resets Y, what is unnecessary, but happens so seldom it doesn't hurt
.cp_literal
.lz_src = * + 1
		lda .data,y			;looks expensive, but is cheaper than loop
		sta (.lz_dst),y
		iny
		dex
		bne .cp_literal

		dey				;this way we force increment of lz_dst + 1 if y = 0
		tya
		adc <.lz_dst + 0
		sta <.lz_dst + 0		;XXX TODO final add of y, coudl be combined with next add? -> postpone until match that will happen necessarily later on?
		bcc +
		inc <.lz_dst + 1
+
		tya
		sec
		adc <.lz_src + 0
		sta <.lz_src + 0
		bcc +
		inc <.lz_src + 1
+
		ldy <.lz_len_hi
		bne .lz_l_page			;happens very seldom

		;------------------
		;NEW OR OLD OFFSET
		;------------------

.cp_literal_done
		lda #$01
		asl <.lz_bits
		bcs .lz_new_offset		;either match with new offset or old offset

		;------------------
		;DO MATCH
		;------------------
.lz_match_repeat
		jsr .get_length
						;XXX TODO encode length - 1 for rep match? but 0 can't be detected then?
		sbc #$01			;saves the sec and iny later on, if it results in a = $ff, no problem, we branch with the beq later on
		;sec				;need sec here if we want to forgo in the beq .lz_calc_msrc
		bcs +
		dcp <.lz_len_hi
+
.lz_match_
		eor #$ff
		;beq .lz_calc_msrc		;just fall through on zero? $ff + sec -> addition is neutralized and carry is set, so no harm
		tay
		eor #$ff			;restore A
.lz_match__					;entry from new_offset handling
		adc <.lz_dst + 0
		sta <.lz_dst + 0
		bcs +				;/!\ branch happens less than fall through, only in case of branch carry needs to be cleared :-(
		dec <.lz_dst + 1
+
		clc				;can this be avoided by receiving offset - 1 from stream?
.lz_offset_lo = * + 1
		sbc #$00
		sta <.lz_msrcr + 0
		lda <.lz_dst + 1
.lz_offset_hi = * + 1
		sbc #$00
		sta <.lz_msrcr + 1
.cp_match
.lz_msrcr = * + 1
		lda $beef,y
.lz_dst = * + 1
		sta $4000,y
		iny
		bne .cp_match
		inc <.lz_dst + 1

.lz_len_hi = * + 1
		lda #$00			;check for more loop runs
		beq .lz_start_over
		dec <.lz_len_hi
		inc <.lz_msrcr + 1		;XXX TODO only needed if more pages follow
		bne .cp_match
.lz_l_page
		sec				;only needs to be set for consecutive rounds of literals, happens very seldom
		ldy #$00
.lz_l_page_
		dec <.lz_len_hi
		bcs .cp_literal

		;------------------
		;FETCH A NEW OFFSET
		;------------------
-						;get_length as inline
		asl <.lz_bits			;fetch payload bit
		rol				;can also moved to front and executed once on start
.lz_new_offset
		asl <.lz_bits
		bcc -
+
		bne +
		jsr .lz_refill_bits
+
		sbc #$01

		bcc .lz_eof			;underflow. must have been 0
		lsr
		sta <.lz_offset_hi		;hibyte of offset

		lda (.lz_src),y			;fetch another byte directly
		ror
		sta <.lz_offset_lo

		inc <.lz_src + 0
		bne +
		inc <.lz_src + 1
+
						;XXX TODO would be nice to have inverted data sent, but would mean MSB also receives inverted bits? sucks. As soon as we refill bits we fall into loop that checks overflow on LSB, should check for bcc however :-( then things would work
						;would work on offset MSB, but need to clear lz_len_hi after that
		lda #$01
		ldy #$fe
		bcs .lz_match__			;length = 2 ^ $ff, do it the very short way :-)
-
		asl <.lz_bits			;fetch first payload bit

		rol				;can also moved to front and executed once on start
		asl <.lz_bits
		bcc -
		bne .lz_match_
		ldy #$00
		jsr .lz_refill_bits		;fetch remaining bits
		bcs .lz_match_
.lz_bits	!byte $40
.lz_refill_bits
		tax
		lda (.lz_src),y
		rol
		sta <.lz_bits
		inc <.lz_src + 0
		bne +
		inc <.lz_src + 1
+
		txa
		bcs .end_bit_16

		;fetch up to 8 bits first, if first byte overflows, stash away byte and fetch more bits as MSB
.lz_get_loop
		asl <.lz_bits			;fetch payload bit
.get_length_
		rol				;can also moved to front and executed once on start
		bcs .get_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.get_length
		asl <.lz_bits
		bcc .lz_get_loop
		beq .lz_refill_bits
		rts

.get_length_16
		pha				;save LSB
		tya				;start with MSB = 1
		jsr .get_length_		;get up to 7 more bits
		sta <.lz_len_hi			;save MSB
		pla				;restore LSB
.end_bit_16
		rts
.lz_eof
		;------------------
		;exit code for sfx only
		;------------------

		dec $01
		;cli
		sty $98
		pla
		sta $ba
.lz_sfx_addr = * + 1
		jmp $0000
}
.depacker_end
;!warn "sfx size: ", .depacker_end - .depacker_start
.data
		;!bin "test.lz"
.data_end
