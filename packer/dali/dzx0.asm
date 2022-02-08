!cpu 6510

CONFIG_ZP_ADDR		= $f0
LZ_BITS_LEFT            = 0

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

;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

			sta <lz_src + 1
			stx <lz_src + 0

                        ldy #$00                        ;needs to be set in any case, also plain decomp enters here
                        ldx #$02
			+init_lz_bits
-
                        lda (lz_src),y
                        sta <lz_dst + 0 - 1, x
                        inc <lz_src + 0
                        bne +
                        inc <lz_src + 1
+
                        dex
                        bne -
                        stx .lz_offset_lo + 1           ;initialize offset with $0000
                        stx .lz_offset_hi + 1
                        stx <lz_len_hi

			;------------------
			;LITERAL
			;------------------
.lz_start_over
			lda #$01			;we fall through this check on entry and start with literal
			+get_lz_bit
			bcc .lz_literal
			jmp .lz_match			;after each match check for another match or literal?
-							;lz_length as inline
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_literal
			+get_lz_bit
			bcc -

			bne +
			jsr .lz_refill_bits
			beq .lz_l_page			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it returns on c = 1, what is always true after jsr .lz_length
.lz_l_page_
+
			tax
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
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
			bcc .lz_repeat
+
			bne +
			jsr .lz_refill_bits		;fetch more bits
			beq .lz_m_page			;if > 8 bits are fetched, correct page_couter if lowbyte == 0
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
			bne .lz_m_page			;do more page runs? Yes? Fall through

			cpx <lz_src + 1
			bne .lz_start_over		;we could check against src >= dst XXX TODO
			ldx <lz_dst + 0			;check for end condition when depacking inplace, lz_dst + 0 still in X
			cpx <lz_src + 0
			bne .lz_start_over
.lz_eof
			rts				;if lz_src + 1 gets incremented, the barrier check hits in even later, so at least one block is loaded, if it was $ff, we at least load the last block @ $ffxx, it must be the last block being loaded anyway

.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_
.lz_inc_src3
			inc <lz_src + 1
			bcs .lz_inc_src3_

			;------------------
			;SELDOM STUFF
			;------------------
.lz_l_page
			tya
			dec <lz_len_hi
			bcs .lz_cp_lit
.lz_clc
			clc
			bcc .lz_clc_back
.lz_m_page
			tya
			dec lz_len_hi
			bcs .lz_m_page_

			;------------------
			;FETCH A NEW OFFSET
			;------------------
-							;lz_length as inline
			+get_lz_bit			;fetch payload bit
			rol				;can also moved to front and executed once on start
.lz_match
			+get_lz_bit
			bcc -

			bne +
			jsr .lz_refill_bits
			beq .lz_eof			;underflow, so offset was $100
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
			bcs .lz_match_big		;and enter match copy loop

			;------------------
			;POINTER HIGHBYTE HANDLING
			;------------------
.lz_inc_src1
			inc <lz_src + 1			;preserves carry, all sane
			bne .lz_inc_src1_
.lz_inc_src2
			inc <lz_src + 1			;preserves carry and A, clears X, Y, all sane
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
			rts
