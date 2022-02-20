!cpu 6510

CONFIG_ZP_ADDR		= $f0
LZ_BITS_LEFT            = 0
INPLACE			= 0
SETUP_LZ_DST		= 0

lz_bits			= CONFIG_ZP_ADDR + 0
lz_dst			= CONFIG_ZP_ADDR + 1
lz_src			= CONFIG_ZP_ADDR + 3
lz_len_hi		= CONFIG_ZP_ADDR + 5

!macro get_lz_bit {
        !if LZ_BITS_LEFT = 1 {
                asl <lz_bits
        } else {
                lsr <lz_bits
        }
}

!macro set_lz_bit_marker {
        !if LZ_BITS_LEFT = 1{
                rol
        } else {
                ror
        }
}

!macro init_lz_bits {
        !if LZ_BITS_LEFT = 1 {
                        lda #$40
                        sta <lz_bits                    ;start with an empty lz_bits, first +get_lz_bit leads to literal this way and bits are refilled upon next shift
        } else {
                        stx <lz_bits
        }
}

!macro inc_lz_src {
			inc <lz_src + 1
}

;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

			sta <lz_src + 1
			stx <lz_src + 0

			lda #$00
			sta <lz_dst + 0
			lda #$a0
			sta <lz_dst + 1

                        ldx #$02
			+init_lz_bits
                        ldy #$00                        ;needs to be set in any case, also plain decomp enters here
!if SETUP_LZ_DST = 1 {
-
                        lda (lz_src),y
                        sta <lz_dst + 0 - 1, x
                        inc <lz_src + 0
                        bne +
                        +inc_lz_src
+
                        dex
                        bne -
}
                        sty .lz_offset_lo + 1           ;initialize offset with $0000
                        sty .lz_offset_hi + 1
                        sty <lz_len_hi
!if INPLACE = 1 {
			beq .lz_start_over
.lz_end_check_
			ldx <lz_dst + 0			;check for end condition when depacking inplace, lz_dst + 0 still in X
			cpx <lz_src + 0
			bne .lz_start_over
.lz_eof
			rts				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway
.lz_end_check
			cpx <lz_src + 1
			beq .lz_end_check_		;we could check against src >= dst XXX TODO
} else {
.lz_end_check
}
.lz_start_over
			lda #$01			;we fall through this check on entry and start with literal
			+get_lz_bit
			bcs .lz_match			;after each match check for another match or literal?

			;------------------
			;LITERAL
			;------------------
.lz_literal
			+get_lz_bit
			bcs +
-							;lz_length as inline
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			+get_lz_bit
			bcc -
+
			bne +
			jsr .lz_refill_bits
+
			tax
.lz_l_page_
.lz_cp_lit
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
			+get_lz_bit			;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcs +
-
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			+get_lz_bit			;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcc -
+
			bne +
			jsr .lz_refill_bits		;fetch more bits
			beq .lz_m_page			;avoid underflow of A on sbc #$01 faster than forcing carry to 1 with a sec all times
+
			sbc #$01			;subtract 1, will be added again on adc as C = 1
.lz_match_big						;we enter with length - 1 here from normal match
			eor #$ff
			tay
.lz_m_page_
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
			beq .lz_end_check		;do more page runs? Yes? Fall through
.lz_m_page
.lz_l_page
			dec <lz_len_hi
			txa				;much shorter this way. if we recalculate m_src and dst, endcheck also hits in if we end with an multipage match, else maybe buggy?
			beq .lz_l_page_
			tya
			bcs .lz_m_page_			;as Y = 0, we can skip the part that does Y = A xor $ff


			;------------------
			;SELDOM STUFF
			;------------------
.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_
.lz_inc_src3
			+inc_lz_src
			bcs .lz_inc_src3_

			;------------------
			;MATCH
			;------------------
-							;lz_length as inline
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_match
			+get_lz_bit
			bcc -

			bne +
			jsr .lz_refill_bits
			beq .lz_lend			;underflow, so offset was $100
+
			sbc #$01			;subtract 1, elias numbers range from 1..256, we need 0..255

			lsr				;set bit 15 to 0 while shifting hibyte
			sta .lz_offset_hi + 1		;hibyte of offset

			lda (lz_src),y			;fetch another byte directly, same as refill_bits...
			ror				;and shift -> first bit for lenth is in carry, and we have %0xxxxxxx xxxxxxxx as offset
			sta .lz_offset_lo + 1

			inc <lz_src + 0			;postponed, so no need to save A on next_page call
			beq .lz_inc_src1
.lz_inc_src1_
			lda #$01
			ldy #$fe
			bcs .lz_match_len2		;length = 1 ^ $ff, do it the very short way :-)
-
			+get_lz_bit
			rol
			+get_lz_bit
			bcc -
			bne .lz_match_big
			ldy #$00			;only now y = 0 is needed
			jsr .lz_refill_bits		;fetch remaining bits
			bne .lz_match_big
			inc <lz_len_hi
			bcs .lz_match_big		;and enter match copy loop

			;------------------
			;SELDOM STUFF
			;------------------
.lz_clc
			clc
			bcc .lz_clc_back
.lz_inc_src1
			+inc_lz_src			;preserves carry, all sane
			bne .lz_inc_src1_
.lz_inc_src2
			+inc_lz_src			;preserves carry and A, clears X, Y, all sane
			bne .lz_inc_src2_

			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax
			lda (lz_src),y
			+set_lz_bit_marker
			sta <lz_bits
			inc <lz_src + 0 		;postponed, so no need to save A on next_page call
			beq .lz_inc_src2
.lz_inc_src2_
			txa				;also postpone, so A can be trashed on lz_inc_src above
			bcs .lz_lend
.lz_get_loop
			+get_lz_bit			;fetch payload bit
.lz_length_16_
			rol				;can also moved to front and executed once on start
			bcs .lz_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
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
			bne +
			dec <lz_len_hi
			tya
+
			rts
