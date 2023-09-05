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

LZ_BITS_LEFT		= 1							;shift lz_bits left or right, might make a difference on testing and init, left is the original way

;if you do not make use of the nmi-gaps, these optimizations will be enabled, with gaps, they don't fit :-(
OPT_FULL_SET		= (CONFIG_NMI_GAPS | CONFIG_NEXT_DOUBLE) xor 1		;adds 1,4% more performance, needs 10 bytes extra
OPT_PRIO_LEN2		= (CONFIG_NMI_GAPS | CONFIG_NEXT_DOUBLE) xor 1		;adds 0,1% more performance, needs 4 bytes extra
OPT_LZ_INC_SRC1		= 1							;give non equal case priority on lz_src checks
OPT_LZ_INC_SRC2		= 1							;give non equal case priority on lz_src checks
OPT_LZ_INC_SRC3		= 1							;give non equal case priority on lz_src checks
OPT_LZ_DST_INC		= CONFIG_NMI_GAPS xor 1
OPT_LZ_CLC		= CONFIG_NMI_GAPS xor 1

bitfire_load_addr_lo	= CONFIG_ZP_ADDR + 0					;in case of no loadcompd, store the hi- and lobyte of loadaddress separatedly
bitfire_load_addr_hi	= CONFIG_ZP_ADDR + 1
lz_bits			= CONFIG_ZP_ADDR + 2					;1 byte	- ommits preamble in case and occupies space with those vars
lz_dst			= CONFIG_ZP_ADDR + 3					;2 bytes
lz_len_hi		= CONFIG_ZP_ADDR + 5					;1 byte
lz_src			= bitfire_load_addr_lo
preamble		= CONFIG_ZP_ADDR + 6					;5 bytes

block_length		= preamble + 0
block_addr_lo		= preamble + 1
block_addr_hi		= preamble + 2
block_barrier		= preamble + 3
block_status		= preamble + 4
block_offset		= preamble

filenum			= block_barrier

!if CONFIG_DECOMP = 1 {
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
}

!macro inc_src_ptr {
	;!if CONFIG_LOADER = 1 {
			jsr lz_next_page					;sets X = 0, so all sane
	;} else {
	;		inc <lz_src + 1
	;}
}

bitfire_install_	= CONFIG_INSTALLER_ADDR					;define that label here, as we only aggregate labels from this file into loader_*.inc

			* = CONFIG_RESIDENT_ADDR
.lz_gap1
!if CONFIG_AUTODETECT = 1 {
link_chip_types
link_sid_type		;%00000001						;bit set = new, bit cleared = old
link_cia1_type		;%00000010
link_cia2_type		;%00000100
			!byte $00
}
!if CONFIG_NMI_GAPS = 1 {
	!if CONFIG_AUTODETECT = 0 {
			nop
	}
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
}

!if CONFIG_NEXT_DOUBLE = 1 {
			!warn "disabling some optimizations to make load_next_double fit"
                        ;those calls could be a macro, but they are handy to be jumped to so loading happens while having all mem free, and code is entered afterwards
        !if CONFIG_DECOMP = 1 {
;                       ;expect $01 to be $35
                !if CONFIG_LOADER = 1 {
link_load_next_double
                        ;loads a splitted file, first part up to $d000 second part under IO
                        jsr link_load_next_comp
link_load_next_raw_decomp
                        jsr link_load_next_raw
                }
link_decomp_under_io
                        dec $01							;bank out IO
                        jsr link_decomp						;depack
                        inc $01							;bank in again
                        rts
        }
}

!if CONFIG_CRT = 1 {
.ld_pblock
crt_load_file
link_load_next_raw
			jsr read_byte
			sta lz_dst + 0
			sta lz_src + 0
			jsr read_byte
			sta lz_dst + 1
			sta lz_src + 1

			jsr read_byte
			sta .endpos_lo + 1
			jsr read_byte
			sta .endpos_hi + 1

			lxa #0
			tay
-
			jsr read_byte
			sta (lz_dst),y
			iny
			beq .chk_inc
.chk_done
.endpos_lo 		cpy #$00
			bne -
.endpos_hi		cpx #$00
			bne -
			rts
.chk_inc
			inc lz_dst + 1
			inx
			bne .chk_done
read_byte
.off			lda $df00
			inc .off + 1
			bne +
			bit $de00
+
;XXX TODO remove to trap the turn disk positions
;XXX TODO do macro for request_turn_disk + check_turn_disk and wait_turn_disk
bitfire_send_byte_
bitfire_loadraw_
			rts
}

;---------------------------------------------------------------------------------
;LOADER STUFF
;---------------------------------------------------------------------------------

!if CONFIG_LOADER = 1 {
			;/!\ we do not wait for the floppy to be idle, as we waste enough time with depacking or the fallthrough on load_raw to have an idle floppy
bitfire_send_byte_
			sec
			ror
			sta <filenum
			lda #$3f
			;sta $dd02						;unlock bus beforehand
.ld_loop
			eor #$10
			jsr .ld_set_dd02					;waste lots of cycles upon write, so bits do not arrive too fast @floppy
			;nop							;XXX TODO might be omitted, need to check
			lsr <filenum
			bne .ld_loop
			;clc							;force final value to be $3f again (bcc will hit in later on)
.ld_set_dd02									;moved loop code to here, so that enough cycles pass by until $dd02 is set back to $3f after whole byte is transferred, also saves a byte \o/
			tax							;x = $1f/$3f / finally x is always $3f after 8 rounds (8 times eor #$20)
			bcs +
			sbx #$20						;x = $0f/$2f
+
			stx $dd02						;restore $dd02
			rts							;filenum and thus barrier is $00 now, so whenever we enter load_next for a first time, it will load until first block is there

	!if CONFIG_FRAMEWORK = 1 {
link_load_next_raw
			lda #BITFIRE_LOAD_NEXT
link_load_raw
	}
bitfire_loadraw_
			jsr bitfire_send_byte_					;easy, open...
-
.ld_load_raw
			jsr .ld_pblock						;fetch all blocks until eof
			bpl -
			;rts							;just run into ld_pblock code again that will then branch to rts upon block poll
.ld_pblock
			lda $dd00						;bit 6 is always set if not ready or idle/EOF
			and #$c0						;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
			bne .ld_en_exit + 1					;block ready? if so, a = 0 (block ready + busy) if not -> rts

			sec							;loadraw enters ld_pblock with C = 0
			ldy #$05						;fetch 5 bytes of preamble
			;lda #$00						;is already zero due to anc #$c0, that is why we favour anc #$co over asl, as we save a byte
			ldx #<preamble						;target for received bytes
			jsr .ld_set_block_tgt					;load 5 bytes preamble - returns with C = 1 always

			ldx <block_addr_lo					;block_address lo
			lda <block_addr_hi					;block_address hi
			ldy <block_status					;status -> first_block?
			bmi +
			stx bitfire_load_addr_lo				;yes, store load_address (also lz_src in case depacker is present)
			sta bitfire_load_addr_hi
+
										;XXX TODO, should be a4 (ldy preamble) for normal run and a2 (ldx #imm) for preamblerun, hm
			ldy <block_length					;load blocklength
.ld_set_block_tgt
			stx .ld_store + 1					;setup target for block data
			sta .ld_store + 2
										;XXX TODO, change busy signal 1 = ready, 0 = eof, leave ld_pblock with carry set, also tay can be done after preamble load as last value is still in a
			ldx #$6d						;opcode for adc	-> repair any rts being set (also accidently) by y-index-check
.ld_set
			stx .ld_gend
			bcs .ld_gentry
.ld_gloop
			lsr							;%0dddd111
			lsr							;%00dddd11 1
			ldx <CONFIG_LAX_ADDR					;waste one cycle
bitfire_ntsc0		ora $dd00						;%dddddd11 1, ora again to preserve
			stx $dd02

			ror							;%1dddddd1 1
			ror							;%11dddddd 1
			ldx #$3f
			sax .ld_nibble + 1
bitfire_ntsc2		and $dd00						;%ddxxxxxx might loose some lower bits, but will be repaired later on ora
			stx $dd02

.ld_nibble		ora #$00						;%dddddddd -> merge in lower bits again and heal bits being dropped by previous and
.ld_store		sta $b00b,y
.ld_gentry		lax <CONFIG_LAX_ADDR
.ld_gend
bitfire_ntsc3		adc $dd00						;%dd1110xx will be like #$38 (A = $37 + carry) be added
			stx $dd02						;carry is cleared now after last adc, we can exit here with carry cleared (else set if EOF) and do our rts with .ld_gend

			lsr							;%0dd111xx
			lsr							;%00dd111x
			dey
			cpy #$01						;check on 0 in carry, too bad we can't use that result with direct bail out, still some bits to transfer
			ldx #$3f
bitfire_ntsc1		ora $dd00						;%dddd111x, ora to preserve the 3 set bits
			stx $dd02

!if >* != >.ld_gloop { !error "getloop code crosses page!" }			;XXX TODO in fact the branch can also take 4 cycles if needed, ldx <CONFIG_LAX_ADDR wastes one cycle anyway
			bcs .ld_gloop
.ld_en_exit
			ldx #$60
			bne .ld_set						;set rts to end loop at right position

}

;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

!if CONFIG_DECOMP = 1 {
			;------------------
			;ELIAS FETCH
			;------------------
.lz_refill_bits
			tax							;save bits fetched so far
			lda (lz_src),y						;fetch another lz_bits byte from stream
			+set_lz_bit_marker
			sta <lz_bits
			inc <lz_src + 0 					;postponed pointer increment, so no need to save A on next_page call
!if OPT_LZ_INC_SRC1 = 1 {
			beq .lz_inc_src_refill
.lz_inc_src_refill_
} else {
			bne +
			+inc_src_ptr
+
}
			txa							;restore fetched bits, also postponed, so A can be trashed on lz_inc_src above
			bcs .lz_lend						;last bit fetched?
.lz_get_loop
			+get_lz_bit						;fetch payload bit
.lz_length_16_
			rol							;shift in new payload bit
			bcs .lz_length_16					;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.lz_length									;XXX TODO basically can be omitted until refill bits is done once. we read max 4 bits from current lz_bits and another 4 bits after first refill bits. only then a check makes sense
			+get_lz_bit						;fetch control bit
			bcc .lz_get_loop					;need more bits
			beq .lz_refill_bits					;buffer is empty, fetch new bits
.lz_lend									;was the last bit
			rts
.lz_length_16									;this routine happens very rarely, so we can waste cycles
			pha							;save so far received lobyte
			tya							;was lda #$01, but A = 0 + upcoming rol makes this also start with A = 1
			jsr .lz_length_16_					;get up to 7 more bits
			sta <lz_len_hi						;and save hibyte
		!if OPT_FULL_SET = 1 {						;transform beq .lz_cp_page to a lda #$01 and by that do not waste a single cycle on a page check if not needed
			ldx #$c0
			;ldx #.lz_cp_page - .lz_set1 - 2			;we are very lucky here, we can jump in two steps to bcs of set1 and then to lz_cp_page, so we can set up both bcs with the same value ($c0 that is) and reach or goal in two hops, while keeping the setup code small
			top							;leads to ora ($a9,x) lda #$f0 nop
.lz_lenchk_dis
.lz_eof
			pha
			ldx #$01
			lda #$a9
			beq *-($fe-$ea)						;aka !byte $f0, $ea
+
			sta .lz_set1
			sta .lz_set2
			pla
			stx .lz_set1 + 1
			stx .lz_set2 + 1
		} else {							;only transform bcs .lz_cp_page to anything that is not executed, like a nop #imm
			ldx #$b0
			pla
			top
.lz_lenchk_dis
.lz_eof
			ldx #$80
			stx .lz_set1
			stx .lz_set2
                }
			rts

			;------------------
			;DECOMP INIT
			;------------------

	!if CONFIG_CRT = 1 {
link_load_next_comp
			jsr crt_load_file
        }
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
			jsr bitfire_send_byte_					;returns now with x = $3f
			lda #(.lz_poll - .lz_skip_poll) - 2
			ldx #$08
.loadcomp_entry
			sta .lz_skip_poll + 1
			stx .lz_skip_fetch

			jsr .lz_next_page_					;shuffle in data first until first block is present, returns with Y = 0, but on loadcomp only, so take care!
	}
										;copy over end_pos and lz_dst from stream XXX would also work from x = 0 .. 2 -> lax #0 tay txa inx cpx #2 -> a = 1 + sec at end
			ldy #$00						;needs to be set in any case, also plain decomp enters here
			sty .lz_offset_lo + 1					;initialize offset with $0000
			sty .lz_offset_hi + 1
			ldx #$ff
-
			lda (lz_src),y
			sta <lz_dst + 1, x
			inc <lz_src + 0
			bne +
			+inc_src_ptr
+
			inx							;dex + sec set
			beq -
			txa							;-> A = $01
			sec							;needed on entry :-(
			bne .lz_start_depack					;start with a literal

			;------------------
			;SELDOM STUFF
			;------------------
!if OPT_LZ_INC_SRC1 = 1 {
.lz_inc_src_refill
			+inc_src_ptr
			bne .lz_inc_src_refill_
}
!if OPT_LZ_INC_SRC2 = 1 {
.lz_inc_src_match
			+inc_src_ptr
			bne .lz_inc_src_match_
}
!if OPT_LZ_INC_SRC3 = 1 {
.lz_inc_src_lit
			+inc_src_ptr
			bcs .lz_inc_src_lit_
}

!if CONFIG_NMI_GAPS = 1 {
			!ifdef .lz_gap2 {
				!warn "disabling some optimizations to make gaps fit"
				!warn .lz_gap2 - *, " bytes left until gap2"
			}
!align 255,0
.lz_gap2
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop
			nop

!if .lz_gap2 - .lz_gap1 > $0100 {
		!error "code on first page too big, second gap does not fit!"
}
}
			;------------------
			;MATCH
			;------------------
-
			+get_lz_bit						;fetch payload bit
			rol							;add bit to number
.lz_match
			+get_lz_bit						;fetch control bit
			bcc -							;not yet done, fetch more bits

			bne +							;last bit or bitbuffer empty? fetched 1 to 4 bits now
			jsr .lz_refill_bits					;refill bitbuffer
			beq .lz_eof						;so offset was $100 as lowbyte is $00, only here 4-8 bits are fetched, this is our eof marker -> disable len-check and rts
+
			sbc #$01						;subtract 1, elias numbers range from 1..256, we need 0..255
			lsr							;set bit 15 to 0 while shifting hibyte
			sta .lz_offset_hi + 1					;hibyte of offset

			lda (lz_src),y						;fetch another byte directly, same as refill_bits...
			ror							;and shift -> first bit for lenth is in carry, and we have %0xxxxxxx xxxxxxxx as offset
			sta .lz_offset_lo + 1					;lobyte of offset

			inc <lz_src + 0						;postponed, so no need to save A on next_page call
!if OPT_LZ_INC_SRC2 = 1 {
			beq .lz_inc_src_match
.lz_inc_src_match_
} else {
			bne +
			+inc_src_ptr
+
}
			lda #$01						;fetch new number, start with 1
		!if OPT_PRIO_LEN2 = 1 {
			ldy #$fe						;XXX preferring that path gives ~0,1% speed gain
			bcs .lz_match_len2
                } else {
			bcs .lz_match_big					;length = 1, do it the very short way
                }
-
			+get_lz_bit						;fetch more bits
			rol
			+get_lz_bit
			bcc -

			bne .lz_match_big
		!if OPT_PRIO_LEN2 = 1 {
			ldy #$00
		}
			jsr .lz_refill_bits					;fetch remaining bits
			bcs .lz_match_big					;BRA

			;------------------
			;SELDOM STUFF
			;------------------
!if OPT_LZ_DST_INC = 1 {
.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_
}
!if OPT_LZ_CLC = 1 {
.lz_clc
			clc
			bcc .lz_clc_back
}
.lz_cp_page									;if we enter from a literal, we take care that x = 0 (given after loop run, after length fetch, we force it to zero by tax here), so that we can distinguish the code path later on. If we enter from a match x = $b0 (elias fetch) or >lz_dst_hi + 1, so never zero.
			txa
.lz_cp_page_									;a is already 0 if entered here
			dec <lz_len_hi
			bne +
			jsr .lz_lenchk_dis
+
			tax							;check a/x
			beq .lz_l_page						;do another page
			tya							;if entered from a match, x is anything between $01 and $ff due to inx stx >lz_dst + 1, except if we would depack to zp on a wrap around?
			beq .lz_m_page						;as Y = 0 and A = 0 now, we can skip the part that does Y = A xor $ff

			;------------------
			;POLLING
			;------------------
	;!if CONFIG_LOADER = 1 {
.lz_ld_blk
			jsr .ld_pblock						;yes, fetch another block, call is disabled for plain decomp
		!if OPT_FULL_SET = 1 {
			lda #$01						;restore initial length val
		}
.lz_poll
			bit $dd00
			bvc .lz_ld_blk
	;}
			;------------------
			;ENTRY POINT DEPACKER
			;------------------
.lz_start_over
		!if OPT_FULL_SET = 0 {
			lda #$01						;restore initial length val
		}
			+get_lz_bit
			bcs .lz_match						;after each match check for another match or literal?

			;------------------
			;LITERAL
			;------------------
.lz_literal
			+get_lz_bit
			bcs +
-
			+get_lz_bit						;fetch payload bit
			rol							;can also moved to front and executed once on start
			+get_lz_bit						;fetch payload bit
			bcc -
+
			bne +							;lz_bits is empty, so was last bit to fetch for #len
.lz_start_depack
			jsr .lz_refill_bits
			beq .lz_cp_page_					;handle special case of length being $xx00
+
			tax
.lz_l_page
.lz_cp_lit									;XXX TODO copy with Y += 1 but have lz_src + 0 eor #$ff in x and countdown x, so that lz_src + 1 can be incremented in time?
			lda (lz_src),y						;/!\ Need to copy this way, or we run into danger to copy from an area that is yet blocked by barrier, this totally sucks, loading in order reveals that
			sta (lz_dst),y

			inc <lz_src + 0
!if OPT_LZ_INC_SRC3 = 1 {
			beq .lz_inc_src_lit
.lz_inc_src_lit_
} else {
			bne +
			+inc_src_ptr
+
}
			inc <lz_dst + 0
!if OPT_LZ_DST_INC = 1 {
			beq .lz_dst_inc
} else {
			bne .lz_dst_inc_
			inc <lz_dst + 1
}
.lz_dst_inc_
			dex
			bne .lz_cp_lit
.lz_set1
		!if OPT_FULL_SET = 0 {						;if optimization is enabled, the lda #$01 is modified to a bcs .lz_cp_page/lda #$01
			bcc .lz_cp_page						;next page to copy, either enabled or disabled (bcc/nop #imm/bcs)
                }
			;------------------
			;NEW OR OLD OFFSET
			;------------------
										;XXX TODO fetch length first and then decide if literal, match, repeat? But brings our checks for last bit to the end? need to check then on typebit? therefore entry for fetch is straight?
										;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
			lda #$01
			+get_lz_bit
			bcs .lz_match						;either match with new offset or old offset

			;------------------
			;REPEAT LAST OFFSET
			;------------------
.lz_repeat
			+get_lz_bit
			bcs +
-
			+get_lz_bit						;fetch payload bit
			rol							;can also moved to front and executed once on start
			+get_lz_bit						;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcc -
+
			bne +
			jsr .lz_refill_bits					;fetch more bits
			beq .lz_cp_page						;handle special case of length being $xx00
+
			sbc #$01						;subtract 1, will be added again on adc as C = 1
.lz_match_big									;we enter with length - 1 here from normal match
			eor #$ff
			tay
.lz_m_page
			eor #$ff						;restore A
.lz_match_len2
			adc <lz_dst + 0						;add length
			sta <lz_dst + 0
!if OPT_LZ_CLC = 1 {
			bcs .lz_clc						;/!\ branch happens very seldom, if so, clear carry, XXX TODO if we would branch to * + 3 and we use $18 as lz_dst, we have our clc there :-( but we want zp usage to be configureable
			dec <lz_dst + 1						;subtract one more in this case
.lz_clc_back
} else {
			bcs +							;/!\ branch happens very seldom, if so, clear carry, XXX TODO if we would branch to * + 3 and we use $18 as lz_dst, we have our clc there :-( but we want zp usage to be configureable
			dec <lz_dst + 1						;subtract one more in this case
+
			clc
}
.lz_offset_lo		sbc #$00						;carry is cleared, subtract (offset + 1)
			sta .lz_msrcr + 0
			lax <lz_dst + 1
			inx
.lz_offset_hi		sbc #$00
			sta .lz_msrcr + 1
.lz_cp_match									;XXX TODO if repeated offset: add literal size to .lz_msrcr and done?
.lz_msrcr = * + 1
			lda $beef,y
			sta (lz_dst),y
			iny
			bne .lz_cp_match
			stx <lz_dst + 1						;cheaper to get lz_dst + 1 into x than lz_dst + 0 for upcoming compare
.lz_set2
		!if OPT_FULL_SET = 0 {
			bcc .lz_set1						;next page to copy, either enabled or disabled (bcc/nop #imm/bcs)
                } else {
			lda #$01
		}
.lz_check_poll
			cpx <lz_src + 1						;check for end condition when depacking inplace, lz_dst + 0 still in X
.lz_skip_poll		bne .lz_start_over					;-> can be changed to .lz_poll, depending on decomp/loadcomp

			ldx <lz_dst + 0
			cpx <lz_src + 0
			bne .lz_start_over

			lda #$fe						;force the barrier check to always hit in (eof will end this loop), will give $ff after upcoming inc
			sta <lz_src + 1

			;------------------
			;NEXT PAGE IN STREAM
			;------------------
lz_next_page
			inc <lz_src + 1
	!if CONFIG_LOADER = 1 {
.lz_next_page_									;preserves carry and X, clears Y, all sane
.lz_skip_fetch
			php							;save carry
			txa							;and x
			pha
.lz_fetch_sector								;entry of loop
			jsr .ld_pblock						;fetch another block
			bmi .lz_fetch_eof					;eof? yes, finish, only needed if files reach up to $ffxx -> barrier will be 0 then and upcoming check will always hit in -> this would suck
										;XXX TODO send a high enough barrier on last block being sent
			lda <lz_src + 1						;get current depack position
			cmp <block_barrier					;next pending block/barrier reached? If barrier == 0 this test will always loop on first call or until first-block with load-address arrives, no matter what .bitfire_lz_sector_ptr has as value \o/
										;on first successful .ld_pblock they will be set with valid values and things will be checked against correct barrier
			bcs .lz_fetch_sector					;already reached, loop
.lz_fetch_eof									;not reached, go on depacking
			;Y = 0							;XXX TODO could be used to return somewhat dirty from a jsr situation, this would pull two bytes from stack and return
			pla
			tax
			plp
			rts
	} else {
			rts
	}
}

;---------------------------------------------------------------------------------
;FRAMEWORK STUFF
;---------------------------------------------------------------------------------

!if CONFIG_FRAMEWORK = 1 {
	!if CONFIG_FRAMEWORK_BASEIRQ = 1 {
link_player
			pha
			tya
			pha
			txa
			pha
			inc $01							;should be save with $01 == $34/$35, except when music is @ >= $e000
link_ack_interrupt
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
										;this is the music play hook for all parts that they should call instead of for e.g. jsr $1003, it has a variable music location to be called
										;and advances the frame counter if needed
link_music_play
	!if CONFIG_FRAMEWORK_FRAMECOUNTER = 1 {
			inc link_frame_count + 0
			bne +
			inc link_frame_count + 1
+
	}
link_music_addr = * + 1
			jmp link_music_play_side1

	!if CONFIG_FRAMEWORK_FRAMECOUNTER = 1 {
link_frame_count
			!word 0
	}
}


bitfire_resident_size = * - CONFIG_RESIDENT_ADDR
