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

;if you do not make use of the nmi-gaps, these optimizations will be enabled, with gaps, they don't fit :-(
OPT_FULL_SET		= (CONFIG_NEXT_DOUBLE) xor 1				;adds 1,4% more performance, needs 10 bytes extra
OPT_PRIO_LEN2		= (CONFIG_NEXT_DOUBLE) xor 1				;adds 0,1% more performance, needs 4 bytes extra

bitfire_load_addr_lo	= CONFIG_ZP_ADDR + 0					;in case of no loadcompd, store the hi- and lobyte of loadaddress separatedly
bitfire_load_addr_hi	= CONFIG_ZP_ADDR + 1
lz_bits			= CONFIG_ZP_ADDR + 2					;1 byte	- ommits preamble in case and occupies space with those vars
lz_dst			= CONFIG_ZP_ADDR + 3					;2 bytes
lz_len_hi		= CONFIG_ZP_ADDR + 5					;1 byte
lz_src			= bitfire_load_addr_lo
!if CONFIG_LOADER_ONLY = 0 {
preamble		= CONFIG_ZP_ADDR + 6					;5 bytes
} else {
preamble		= CONFIG_ZP_ADDR + 2					;4 bytes
}

block_length		= preamble + 0 + CONFIG_DEBUG
block_addr_lo		= preamble + 1 + CONFIG_DEBUG
block_addr_hi		= preamble + 2 + CONFIG_DEBUG
block_barrier		= preamble + 3 + CONFIG_DEBUG
block_status		= preamble + 4 - CONFIG_LOADER_ONLY + CONFIG_DEBUG
!if CONFIG_DEBUG != 0 {
bitfire_error		= preamble
bitfire_error_acc	= preamble + 5 - CONFIG_LOADER_ONLY + CONFIG_DEBUG
}

filenum			= block_barrier

bitfire_install_	= CONFIG_INSTALLER_ADDR					;define that label here, as we only aggregate labels from this file into loader_*.inc

			* = CONFIG_RESIDENT_ADDR
!if CONFIG_LOADER_ONLY = 0 {
;.lz_gap1
			;------------------
			;MUSIC PLAY HOOK AND FRAME COUNTER
			;------------------
link_music_play									;this is the music play hook for all parts that they should call instead of for e.g. jsr $1003, it has a variable music location to be called
			inc link_frame_count + 0				;and advances the frame counter if needed
			bne +
			inc link_frame_count + 1
+
link_music_addr = * + 1
			jmp link_music_play_side1

link_chip_types
link_sid_type		;%00000001						;bit set = new, bit cleared = old
link_cia1_type		;%00000010
link_cia2_type		;%00000100
			!byte $00

			;XXX Only there for Bob/Censor, basically that is also a macro and can be placed in stack as code for loading
	!if CONFIG_NEXT_DOUBLE = 1 {
			!warn "disabling some optimizations to make load_next_double fit"
			;expect $01 to be $35
link_load_next_double
			;loads a splitted file, first part up to $d000 second part under IO
                        jsr link_load_next_comp
link_load_next_raw_decomp
                        jsr link_load_next_raw
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
.rts
			rts							;filenum and thus barrier is $00 now, so whenever we enter load_next for a first time, it will load until first block is there

;bitfire_send_byte_
;			ldx #$07
;			sta <filenum
;			lda #$0f
;.ld_loop
;			and #$0f
;			nop
;			nop
;			lsr <filenum
;.doit
;			bcc +
;			ora #$20
;+
;			eor #$10
;			dex
;			bpl .ld_loop
;			rts
;bitfire_send_byte_
;			ldx #$07
;			sta <filenum
;			;sta $dd02						;unlock bus beforehand
;			txa
;.ld_loop
;			ora #$2f
;			nop
;			nop
;			lsr <filenum
;.doit
;			bcs +
;			eor #$20
;+
;			sta $dd02
;			eor #$10
;			dex
;			bpl .ld_loop
;			rts

;.jam
;			jam
link_load_next_raw
			lda #BITFIRE_LOAD_NEXT
link_load_raw
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
			bne .rts						;block ready? if so, a = 0 (block ready + busy) if not -> rts

			ldy #$05 - CONFIG_LOADER_ONLY + CONFIG_DEBUG		;fetch 5 bytes of preamble
			;lda #$00						;is already zero due to anc #$c0, that is why we favour anc #$co over asl, as we save a byte
			ldx #<preamble						;target for received bytes
			jsr .ld_set_block_tgt					;load 5 bytes preamble - returns with C = 1 always

!if CONFIG_DEBUG != 0 {
			lda <bitfire_error_acc
			clc
			adc <bitfire_error
			sta <bitfire_error_acc
			sec
}
			ldx <block_addr_lo					;block_address lo
			lda <block_addr_hi					;block_address hi
			;beq .jam
			ldy <block_status					;status -> first_block?
			bmi +
			stx bitfire_load_addr_lo				;yes, store load_address (also lz_src in case depacker is present)
			sta bitfire_load_addr_hi
+
			ldy <block_length					;load blocklength
.ld_set_block_tgt
			stx .ld_store + 1					;setup target for block data
			sta .ld_store + 2

			sec							;loadraw enters ld_pblock with C = 0
bitfire_ntsc3_op	ldx #$6d						;opcode for adc	-> repair any rts being set (also accidently) by y-index-check
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

;XXX TODO
;cpy #$ff
;bcc loop
;block enter: ldx #$3f -> $dd02
;block exit: ldx #$37 -> $dd02
;need to bail out on lsr?

!if >* != >.ld_gloop { !error "getloop code crosses page!" }			;XXX TODO in fact the branch can also take 4 cycles if needed, ldx <CONFIG_LAX_ADDR wastes one cycle anyway
			bcs .ld_gloop
.ld_en_exit
			ldx #$60
			bne .ld_set						;set rts to end loop at right position

!if CONFIG_LOADER_ONLY = 0 {
;---------------------------------------------------------------------------------
;DEPACKER STUFF
;---------------------------------------------------------------------------------

			;------------------
			;ELIAS FETCH
			;------------------
.lz_inc_src_refill
			jsr lz_next_page
			bne .lz_inc_src_refill_
.lz_refill_bits
			tax							;save bits fetched so far
			lda (lz_src),y						;fetch another lz_bits byte from stream
			rol
			sta <lz_bits
			inc <lz_src + 0 					;postponed pointer increment, so no need to save A on next_page call
			beq .lz_inc_src_refill
.lz_inc_src_refill_
			txa							;restore fetched bits, also postponed, so A can be trashed on lz_inc_src above
			bcs .lz_lend						;last bit fetched?
.lz_get_loop
			asl <lz_bits						;fetch payload bit
.lz_length_16_
			rol							;shift in new payload bit
			bcs .lz_length_16					;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.lz_length									;XXX TODO basically can be omitted until refill bits is done once. we read max 4 bits from current lz_bits and another 4 bits after first refill bits. only then a check makes sense
			asl <lz_bits						;fetch control bit
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
			top							;leads to lda #$f0 top
.lz_lenchk_dis
			;XXX TODO depending on code we come from we have Z = 0 (lz_cp_page) and Z = 1 (length_16)
.lz_eof
			pha
			lda #$a9
			!byte $f0						;leads to beq *+$0c, will be skipped and is basically a nop, as A != 0
			top
			ldx #$01						;set x if entered via .lz_lenchk_dis
			sta .lz_set1
			sta .lz_set2
			stx .lz_set1 + 1
			stx .lz_set2 + 1
			pla
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
	!if CONFIG_CRT = 0 {
			lda #(.lz_start_over - .lz_skip_poll) - 2
			ldx #$60
			bne .loadcomp_entry
link_load_next_comp
			lda #BITFIRE_LOAD_NEXT
link_load_comp
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
			jsr lz_next_page
+
			inx							;dex + sec set
			beq -
			txa							;-> A = $01
			sec							;needed on entry :-(
			bne .lz_start_depack					;start with a literal

			;------------------
			;BASE IRQ/NMI
			;------------------

;			!ifdef .lz_gap2 {
;				;!warn "disabling some optimizations to make gaps fit"
;				!warn .lz_gap2 - *, " bytes left until gap2 @", *
;				!if .lz_gap2 - .lz_gap1 > $0100 {
;					!warn "code on first page too big, second gap does not fit!", .lz_gap2 - .lz_gap1
;				}
;			}
;.lz_gap2

link_player
			pha
			tya
			pha
			txa
			pha
			inc $01							;should be save with $01 == $34/$35, except when music is @ >= $e000
link_ack_interrupt
			lda $dd0d
			jsr link_music_play
			dec $01

			pla
			tax
			pla
			tay
			pla
			rti

			;------------------
			;MATCH
			;------------------
.lz_inc_src_match
			jsr lz_next_page
			bne .lz_inc_src_match_
-
			asl <lz_bits						;fetch payload bit
			rol							;add bit to number
.lz_match
			asl <lz_bits						;fetch control bit
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
			beq .lz_inc_src_match
.lz_inc_src_match_
			lda #$01						;fetch new number, start with 1
		!if OPT_PRIO_LEN2 = 1 {
			ldy #$fe						;XXX preferring that path gives ~0,1% speed gain
			bcs .lz_match_len2
                } else {
			bcs .lz_match_big					;length = 1, do it the very short way
                }
-
			asl <lz_bits						;fetch more bits
			rol
			asl <lz_bits
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
.lz_dst_inc
			inc <lz_dst + 1
			bcs .lz_dst_inc_
.lz_clc
			clc
			bcc .lz_clc_back
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
.lz_ld_blk
			jsr .ld_pblock						;yes, fetch another block, call is disabled for plain decomp
		!if OPT_FULL_SET = 1 {
			lda #$01						;restore initial length val
		}
.lz_poll
			bit $dd00
			bvc .lz_ld_blk

			;------------------
			;ENTRY POINT DEPACKER
			;------------------
.lz_start_over
		!if OPT_FULL_SET = 0 {
			lda #$01						;restore initial length val
		}
			asl <lz_bits
			bcs .lz_match						;after each match check for another match or literal?

			;------------------
			;LITERAL
			;------------------
.lz_literal
			asl <lz_bits
			bcs +
-
			asl <lz_bits						;fetch payload bit
			rol							;can also moved to front and executed once on start
			asl <lz_bits						;fetch payload bit
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
			beq .lz_inc_src_lit
.lz_inc_src_lit_
			inc <lz_dst + 0
			beq .lz_dst_inc
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
			asl <lz_bits
			bcs .lz_match						;either match with new offset or old offset

			;------------------
			;REPEAT LAST OFFSET
			;------------------
.lz_repeat
			asl <lz_bits
			bcs +
-
			asl <lz_bits						;fetch payload bit
			rol							;can also moved to front and executed once on start
			asl <lz_bits						;cheaper with 2 branches, as initial branch to .lz_literal therefore is removed
			bcc -
+
			bne +
			jsr .lz_refill_bits					;fetch more bits
			beq .lz_cp_page						;handle special case of length being $xx00
+
			sbc #$01						;subtract 1, will be added again on adc as C = 1
										;XXX could we start with #$ff for number and shift in inverted bits, so we get a negative number?
.lz_match_big									;we enter with length - 1 here from normal match
			eor #$ff
			tay
.lz_m_page
			eor #$ff						;restore A
.lz_match_len2
			adc <lz_dst + 0						;add length
			sta <lz_dst + 0
			bcs .lz_clc						;/!\ branch happens very seldom, if so, clear carry, XXX TODO if we would branch to * + 3 and we use $18 as lz_dst, we have our clc there :-( but we want zp usage to be configureable
			dec <lz_dst + 1						;subtract one more in this case
.lz_clc_back
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
.lz_inc_src_lit
			jsr lz_next_page
			bcs .lz_inc_src_lit_

link_frame_count
			!word 0
}

bitfire_resident_size = * - CONFIG_RESIDENT_ADDR
