;/!\ Attention, this depacker only works with the original zx0 version contained within this folder as well, it does not work with the modified version coming with bitfire, as some things on the encoding got changed due to speed optimizations

!cpu 6510

;ZX0_INPLACE		= 0

.ZP_ADDR		= $f8
.lz_dst			= .ZP_ADDR + 0
.lz_bits		= .ZP_ADDR + 2
.lz_len_hi		= .ZP_ADDR + 4

.depacker_start
		;------------------
		;INIT
		;------------------

		;lowbyte of data start must be in X, highbyte in A
		sta .lz_src1
		sta .lz_src2
		sta .lz_src3

		lda #$00
		sta <.lz_dst + 0
		lda #$a0
		sta <.lz_dst + 1

		ldy #$ff
		sty .lz_offset_lo + 1
		sty .lz_offset_hi + 1

		iny
		sty <.lz_len_hi
		lda #$40
		sta <.lz_bits			;will make us fall through on next test and force us to load a new byte into bit-buffer upon next .lz_get_len

		;------------------
		;LITERAL
		;------------------
.lz_start_over
		lda #$01
		asl <.lz_bits
		bcs .lz_new_offset
.lz_literal
		jsr .lz_get_len
		sta .lz_y + 1
		and #$ff                        ;annoying, but flags are not set corresponding to A
		beq .lz_l_page_
.lz_cp_literal
.lz_src1 = * + 2
		lda $1000,x
		inx
		bne +
		jsr .lz_inc_src_hi
+
		sta (.lz_dst),y
		iny
.lz_y		cpy #$00
		bne .lz_cp_literal

		dey				;this way we force increment of lz_dst + 1 if y = 0
		tya				;carry is still set on first round
		adc <.lz_dst + 0		;correct dst after copy loop
		sta <.lz_dst + 0
		bcc +
		inc <.lz_dst + 1
+
		ldy <.lz_len_hi
		bne .lz_l_page			;happens very seldom -> move away to prefer Z = 0 case

		;------------------
		;NEW OR OLD OFFSET
		;------------------

		lda #$01
		asl <.lz_bits
		bcs .lz_new_offset		;either match with new offset or old offset
.lz_match_repeat
		jsr .lz_get_len
;!if ZX0_INPLACE == 0 {
;}
		sbc #$01			;saves the iny later on
		bcc .lz_dcp			;dec highbyte of length by one, a = $ff, so cmp will always set carry for free on top
.lz_match_
		eor #$ff
		;beq .lz_calc_msrc		;just fall through on zero? $ff + sec -> addition is neutralized and carry is set, so no harm
		tay
		eor #$ff			;restore A
.lz_match__					;entry from new_offset handling
		adc <.lz_dst + 0
		sta <.lz_dst + 0
		bcs .lz_clc			;/!\ branch happens very seldom
		dec <.lz_dst + 1
.lz_clc_back
		clc
.lz_offset_lo	adc #$ff			;carry is cleared, subtract (offset + 1)
		sta .lz_msrcr + 0
		lda <.lz_dst + 1
.lz_offset_hi	adc #$ff
		sta .lz_msrcr + 1
.lz_cp_match
.lz_msrcr = * + 1
		lda $beef,y
		sta (.lz_dst),y
		iny
		bne .lz_cp_match
		inc <.lz_dst + 1

		lda <.lz_len_hi			;check for more loop runs
;!if ZX0_INPLACE == 1 {
;		bne .lz_m_page			;do more page runs
;
;		cpx <.lz_dst + 0		;check for end condition when depacking inplace
;		bne .lz_start_over
;		lda <.lz_dst + 1
;		sbc <.lz_src1
;		bne .lz_start_over
;		rts
;.lz_m_page
;		lda #$ff
;} else {
		beq .lz_start_over		;do more page runs
		lda #$ff
;}
		;------------------
		;SELDOM STUFF
		;------------------
.lz_dcp
		dcp .lz_len_hi
		bcs .lz_match_
.lz_clc
		clc
		bcc .lz_clc_back
.lz_l_page
		sec				;only needs to be set for consecutive rounds of literals, happens very seldom
		ldy #$00
.lz_l_page_
		dec <.lz_len_hi
		bcs .lz_cp_literal

		;------------------
		;FETCH A NEW OFFSET
		;------------------

.lz_new_offset
		lda #$fe
		jsr .lz_get_len
		sty .lz_len_hi
		adc #$00
		beq .lz_eof			;underflow. must have been 0

		sec
		ror
		sta .lz_offset_hi + 1		;hibyte of offset

.lz_src2 = * + 2
		lda $1000,x			;looks expensive, but is cheaper than loop
		inx
		bne +
		jsr .lz_inc_src_hi
+
		ror
		sta .lz_offset_lo + 1

		lda #$01
		ldy #$fe
		bcs .lz_match__			;length = 2 ^ $ff, do it the very short way :-)
		ldy #$00
		jsr .lz_get_len_
		bcs .lz_match_

.lz_inc_src_hi
		inc .lz_src1
		inc .lz_src2
		inc .lz_src3
		rts

.lz_get_len_
-						;lz_get_len as inline
		asl <.lz_bits			;fetch payload bit
		rol
.lz_get_len
		asl <.lz_bits
		bcc -
		bne .lz_get_end
.lz_refill_bits					;refill bits, this happens after 4 payload-bits bestcase
.lz_src3 = * + 2
		ldy $1000,x
		inx
		bne +
		jsr .lz_inc_src_hi
+
		sty <.lz_bits
		ldy #$00
		rol <.lz_bits
		bcs .lz_get_end
-						;continue with 16 bit shifting
		asl <.lz_bits			;fetch payload bit
		rol				;can also moved to front and executed once on start
.lz_get_len_16
		rol <.lz_len_hi
		asl <.lz_bits
		bcc -
		beq .lz_refill_bits
.lz_get_end
.lz_eof
		rts
.depacker_end
