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

BITS_LEFT	= 0

.depacker	= $01
.smc_offsetd 	= .depacker - (.dali_code_end - .dali_code_start)
DALI_SRC	= lz_src - .smc_offsetd + 2
DALI_DST	= lz_dst      - .smc_offsetd + 2
DALI_SFX_ADDR	= lz_sfx_addr - .smc_offsetd + 2
DALI_DATA_END 	= lz_data_end      - .smc_offsetd + 2
DALI_DATA_SIZE_HI = lz_data_size_hi - .smc_offsetd + 2
DALI_01		= lz_01 - .smc_offsetd + 2
DALI_CLI	= lz_cli - .smc_offsetd + 2

!macro get_lz_bit {
	!if BITS_LEFT = 1 {
		asl <lz_bits
	} else {
		lsr <lz_bits
	}
}

!macro set_lz_bit_marker {
	!if BITS_LEFT = 1 {
		rol
	} else {
		ror
	}
}

		* = $0801
.dali_code_start
                !byte $0b,$08
		!word 1602
		!byte $9e
		!text "2061"
		!byte $00,$00,$00

		;/!\ ATTENTION, the depacker just fits into ZP this way, if it gets larger, the copy routine will overwrite $00, as it is a 8-bit address sta
		sei

		;full zp code will be copied, but later less bytes will be copied back
		ldx #<($100 + (.depacker_end - .restore_end))
		txs

		ldy #.depacker_end - .depacker_start
-
		pha				;saved zp to stack down to $02
		lax <.depacker - 1,y		;saves a byte, 2 byte compared to lda $0000,y
		ldx .depacker_code - 1,y
		stx <.depacker - 1,y
		dey
		bne -
                jmp .depack

		;------------------
		;depacker starts here
		;------------------
.dali_code_end
.depacker_code
!pseudopc .depacker {
.depacker_start
		!byte $34
lz_bits
!if BITS_LEFT = 1 {
		!byte $40
} else {
		!byte $02
}

.depack
lz_01 = * + 1
		lda #$37			;replace value for $01 in saved ZP on stack
		pha
-						;copy data to end of ram ($ffff)
                dey
lz_data_end = * + 1
.src		lda .data_end - $100,y
.dst		sta $ff00,y
                tya				;annoying, but need to copy from $ff ... $00
                bne -

                dec <.src + 2
lz_data_size_hi = * + 1
                lda #>(.data_end - .data) + 1	;check for last page to copy
                dcp <.dst + 2
                bne -

		;ldy #$00			;is already 0

		;------------------
		;LITERAL
		;------------------
.lz_start_over
		lda #$01			;we fall through this check on entry and start with literal
		+get_lz_bit
		bcs .lz_new_offset		;after each match check for another match or literal?
.literal
		jsr .get_length
		tax
		beq .lz_l_page_
.cp_literal
lz_src = * + 1
		lda .data,y			;looks expensive, but is cheaper than loop
		sta (lz_dst),y
		iny
		dex
		bne .cp_literal

		dey				;this way we force increment of lz_dst + 1 if y = 0
		tya
		adc <lz_dst + 0
		sta <lz_dst + 0			;XXX TODO final add of y, could be combined with next add? -> postpone until match that will happen necessarily later on?
		bcc +
		inc <lz_dst + 1
+
		tya
		sec
		adc <lz_src + 0
		sta <lz_src + 0
		bcc +
		inc <lz_src + 1
+
		ldy <.lz_len_hi
		bne .lz_l_page			;happens very seldom

		;------------------
		;NEW OR OLD OFFSET
		;------------------

		lda #$01
		+get_lz_bit
		bcs .lz_new_offset		;either match with new offset or old offset

		;------------------
		;DO MATCH
		;------------------
.lz_match
		jsr .get_length
.lz_m_page
		sbc #$01			;saves the sec and iny later on, if it results in a = $ff, no problem, we branch with the beq later on
		bcc .lz_dcp
.lz_match_
		eor #$ff
		tay
		eor #$ff			;restore A
.lz_match__					;entry from new_offset handling
		adc <lz_dst + 0
		sta <lz_dst + 0
		bcs .lz_clc			;/!\ branch happens less than fall through, only in case of branch carry needs to be cleared :-(
		dec <lz_dst + 1
.lz_clc_
.lz_offset_lo = * + 1
		sbc #$00
		sta <.lz_msrcr + 0
		lda <lz_dst + 1
.lz_offset_hi = * + 1
		sbc #$00
		sta <.lz_msrcr + 1
.cp_match
.lz_msrcr = * + 1
		lda $beef,y
lz_dst = * + 1
		sta $4000,y
		iny
		bne .cp_match
		inc <lz_dst + 1

.lz_len_hi = * + 1
		lda #$00			;check for more loop runs
		beq .lz_start_over
		tya
		beq .lz_m_page
.lz_dcp
		dcp <.lz_len_hi			;as a = $ff this will decrement <.lz_len_hi and set carry again in any case
		bcs .lz_match_
.lz_clc
		clc
		bcc .lz_clc_

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
		+get_lz_bit			;fetch payload bit
		rol				;can also moved to front and executed once on start
.lz_new_offset
		+get_lz_bit
		bcc -
+
		bne +
		jsr .lz_refill_bits
+
		sbc #$01

		bcc .lz_eof			;underflow. must have been 0
		lsr
		sta <.lz_offset_hi		;hibyte of offset

		lda (lz_src),y			;fetch another byte directly
		ror
		sta <.lz_offset_lo

		inc <lz_src + 0
		bne +
		inc <lz_src + 1
+
						;XXX TODO would be nice to have inverted data sent, but would mean MSB also receives inverted bits? sucks. As soon as we refill bits we fall into loop that checks overflow on LSB, should check for bcc however :-( then things would work
						;would work on offset MSB, but need to clear lz_len_hi after that
		lda #$01
		ldy #$fe
		bcs .lz_match__			;length = 2 ^ $ff, do it the very short way :-)
-
		+get_lz_bit			;fetch first payload bit

		rol				;can also moved to front and executed once on start
		+get_lz_bit
		bcc -
		bne .lz_match_
		ldy #$00
		jsr .lz_refill_bits		;fetch remaining bits
		bcs .lz_match_

.lz_refill_bits
		tax
		lda (lz_src),y
		+set_lz_bit_marker
		sta <lz_bits
		inc <lz_src + 0
		bne +
		inc <lz_src + 1
+
		txa
		bcs .end_bit_16

		;fetch up to 8 bits first, if first byte overflows, stash away byte and fetch more bits as MSB
.lz_get_loop
		+get_lz_bit			;fetch payload bit
.get_length_
		rol				;can also moved to front and executed once on start
		bcs .get_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.get_length
		+get_lz_bit
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

.restore_end
		;restore zp up to $dc
-
		pla
		tsx
		sta <(.depacker - ($100 - (.restore_end - .depacker_start))),x
		bne -
		pha				;end up with SP = $ff, let's be nice :-)
lz_cli
		sei
lz_sfx_addr = * + 1
		jmp $0000
.depacker_end
}

;!warn "fixup size: ",.depacker_end - .restore_end
!warn "zp saved up to: ",.restore_end - .depacker
;!warn "sfx zp size: ", .depacker_end - .depacker_start
!warn "sfx size: ", * - .dali_code_start
.data
		;!bin "test.lz"
.data_end
