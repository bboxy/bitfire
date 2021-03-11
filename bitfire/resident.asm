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

!zone resident {
!src "../music.inc"
!src "config.inc"
!convtab pet
!cpu 6510

!if BITFIRE_LOADER = 1 {
;loader zp-addresses
.filenum	= BITFIRE_ZP_ADDR + 0
.barrier	= .filenum
!if BITFIRE_DECOMP = 0 {
bitfire_load_addr_lo = .filenum			;in case of no loadcompd, store the highbyte of loadaddress separatedly
bitfire_load_addr_hi = .filenum + 1		;in case of no loadcompd, store the highbyte of loadaddress separatedly
} else {
bitfire_load_addr_lo = .lz_src + 0
bitfire_load_addr_hi = .lz_src + 1
}
}

!if BITFIRE_DECOMP = 1 {
.lz_bits	= BITFIRE_ZP_ADDR + 1
.lz_src		= BITFIRE_ZP_ADDR + 2
.lz_dst		= BITFIRE_ZP_ADDR + 4
.lz_offset	= BITFIRE_ZP_ADDR + 6
.lz_len_hi	= BITFIRE_ZP_ADDR + 8
}

;define that label here, as we only aggregate labels from this file into loader_*.inc
bitfire_install_ = BITFIRE_INSTALLER_ADDR


		* = BITFIRE_RESIDENT_ADDR

!if BITFIRE_FRAMEWORK = 1 & BITFIRE_FRAMEWORK_FRAMECOUNTER = 1 {
link_frame_count
		!word 0
}

!if BITFIRE_NMI_GAPS = 1 {
!align 255,2
.lz_gap1
		nop
		nop
		nop
		nop
		nop
		nop
}

!if BITFIRE_AUTODETECT = 1 {
link_chip_types
link_sid_type			;%00000001	;bit set = new, bit cleared = old
link_cia1_type			;%00000010
link_cia2_type			;%00000100
		!byte $00
}
!if BITFIRE_FRAMEWORK = 1 {

!if BITFIRE_FRAMEWORK_BASEIRQ = 1 {
link_player
		pha
		tya
		pha
		txa
		pha
		inc $01				;should be save with $01 == $34/$35, except when music is @ >= $e000
!if BITFIRE_FRAMEWORK_MUSIC_NMI = 1 {
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

link_music_play
!if BITFIRE_FRAMEWORK_FRAMECOUNTER = 1 {
		inc link_frame_count + 0
		bne +
		inc link_frame_count + 1
+
link_music_addr = * + 1
		jmp link_music_play_side1
}
		;this is the music play hook for all parts that they should call instead of for e.g. jsr $1003, it has a variable music location to be called
		;and advances the frame counter if needed

		;those calls could be a macro, but they are handy to be jumped to so loading happens while having all mem free, and code is entered afterwards
!if BITFIRE_DECOMP = 1 {
link_decomp	= bitfire_decomp_
;		;expect $01 to be $35
!if BITFIRE_LOADER = 1 {
link_load_next_double
		;loads a splitted file, first part up to $d000 second part under IO
		jsr link_load_next_comp
link_load_next_raw_decomp
		jsr link_load_next_raw
}
link_decomp_under_io
		dec $01				;bank out IO
		jsr link_decomp			;depack
		inc $01				;bank in again
		rts
}

}

!if BITFIRE_LOADER = 1 {
		;XXX we do not wait for the floppy to be idle, as we waste enough time with depacking or the fallthrough on load_raw to have an idle floppy
		;XXX ATTENTION /!\ never ever get back to the idea of swapping the two bits on sending a filename for the sake of saving cycls or bytes, it only works this way round when doing bus_lock, as checks on driveside define the bitpositions where the clock is low and hi, that is more sane

bitfire_send_byte_
		ldx #$ff
		sec
		ror
		sta .filenum
		txa
.bit_loop	and #$1f
		bcc +
		eor #$20
+
		eor #$30
		sta $dd02
		lsr <(.filenum - $ff), x	;fetch next bit from filenumber and waste cycles
		bne .bit_loop
.poll_end
		rts

!if BITFIRE_FRAMEWORK = 1 {
link_load_next_raw
		lda #BITFIRE_LOAD_NEXT
link_load_raw
}

bitfire_loadraw_
		jsr bitfire_send_byte_		;easy, open...
-
		jsr .pollblock
		bcc -
						;just run into pollblock code again that will then jump to .poll_end and rts
.pollblock
		lda $dd00			;bit 6 is always set if not ready or idle/EOF so no problem with just an ASL
		asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
		bmi .poll_end			;block ready?
.poll_start
		ldx #$60			;set rts
		jsr .bitfire_ack_		;start data transfer (6 bits of payload possible on first byte, as first two bits are used to signal block ready + no eof). Also sets an rts in receive loop
		php				;preserve flag

!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		jsr .get_one_byte		;fetch barrier
		sta .barrier
}
.bitfire_load_block
		jsr .get_one_byte		;fetch blockaddr hi
		sta .bitfire_block_addr + 1	;where to place the block?
		pha				;save over A
		jsr .get_one_byte
		sta .bitfire_block_addr + 0
		tax
		pla				;lo/hi in x/a for later use

		plp				;XXX TODO php + plp is really annoying. how to catch that ack val, or can we put it into another get_one_byte?
						;we could use %00xxxx00 and %11xxxx00 opcode for smc?
		bmi .skip_load_addr		;#$fc -> first block, fetch load-address
		;lda .bitfire_block_addr + 1	;fetch loadaddr hi

		stx <bitfire_load_addr_lo
		sta <bitfire_load_addr_hi
.skip_load_addr
		jsr .get_one_byte		;fetch blocklen
		tay
		ldx #$99			;sta $xxxx,y
.bitfire_ack_
		stx .store
.get_one_byte
		ldx #$8e			;opcode for stx	-> repair any rts being set (also accidently) by y-index-check
		top
.get_en_exit
		ldx #$60
		stx .finish			;XXX TODO would be nice if we could do that with store in same time, but happens at different timeslots :-(
		bpl +				;do bpl first and waste another 2 cycles on loop entry, so that floppy manages to switch from preamble to send_data
		bmi .get_entry
.get_loop
		ldx #$3f
		ora $dd00 - $3f,x
		stx $dd02
		lsr				;%xxxxx111
		lsr				;%xxxxxx11 1
		dey
		beq .get_en_exit
+
		ldx #$37
		ora $dd00
		stx $dd02
		ror				;c = 1
		ror				;c = 1 a = %11xxxxxx
		ldx #$3f
		sax .nibble + 1
		and $dd00
		stx $dd02
.nibble		ora #$00
.bitfire_block_addr = * + 1
.store		sta $b00b,y

.get_entry
		lax <BITFIRE_LAX_ADDR
		adc $dd00			;a is anything between 38 and 3b after add (37 + 00..03 + carry), so bit 3 and 4 is always set, bits 6 and 7 are given by floppy
						;%xx111xxx
.finish		stx $dd02			;carry is cleared now, we can exit here and do our rts
		lsr				;%xxx111xx
		lsr				;%xxxx111x
		bne .get_loop			;BRA, a is anything between 0e and 3e

!if >* != >.get_loop { !error "getloop code crosses page!" }
;XXX TODO in fact the branch can also take 4 cycles if needed, ora $dd00 - $3f,x wastes one cycle anyway

}

;---------------------------------------------------------------------------------
;REFILL ROUTINES
;---------------------------------------------------------------------------------


!if BITFIRE_DECOMP = 1 {
bitfire_decomp_
		ldy #$00
	!if BITFIRE_LOADER = 1 {
		lda #(.lz_start_over - .lz_skip_poll) - 2
		ldx #$60
		bne .loadcomp_entry
	!if BITFIRE_FRAMEWORK = 1 {
link_load_next_comp
		lda #BITFIRE_LOAD_NEXT
link_load_comp
	}
bitfire_loadcomp_
		jsr bitfire_send_byte_		;returns now with x = $ff
		lda #(.lz_poll - .lz_skip_poll) - 2
		ldx #$08
.loadcomp_entry
		sta .lz_skip_poll + 1
		stx .lz_skip_fetch

		jsr .lz_next_page_		;shuffle in data first, returns with y = 0
	}
						;copy over end_pos and lz_dst from stream
		ldx #$01
-
		lda (.lz_src),y
		sta <.lz_dst,x
		inc <.lz_src + 0
		bne +
		jsr .lz_next_page
+
		dex
		bpl -

		sty .lz_offset_lo + 1		;initialize offset with $0000
		sty .lz_offset_hi + 1
						;start with an empty lz_bits, first asl <.lz_bits leads to literal this way and bits are refilled upon next shift
		sty <.lz_len_hi			;reset len - XXX TODO could also be cleared upon installer, as the depacker leaves that value clean again

		lda #$40
		sta <.lz_bits

!if BITFIRE_LOADER = 1 {
		bne .lz_start_over		;ugly, but we better skip? We don't want to do polling on a decomp call :-(

!if BITFIRE_NMI_GAPS = 1 {

		nop				;1 byte free here

		;!ifdef .lz_gap2 {
		;	!warn .lz_gap2 - *, " bytes left until gap2"
		;}
!align 255,2
.lz_gap2
!if .lz_gap2 - .lz_gap1 > $0100 {
		!error "code on first page too big, second gap does not fit! "
}
		nop
		nop
		nop
}

.lz_next_page
		inc <.lz_src + 1
.lz_next_page_
.lz_skip_fetch
		php
		txa
		pha
.lz_fetch_sector				;entry of loop
		jsr .pollblock			;fetch another block
		bcs .lz_fetch_eof		;eof? yes, finish, only needed if files reach up to $ffxx -> barrier will be 0 then and upcoming check will always hit in -> this would suck
		lda <.lz_src + 1		;get current depack position
		cmp .barrier			;next pending block/barrier reached? If barrier == 0 this test will always loop on first call or until first-block with load-address arrives, no matter what .bitfire_lz_sector_ptr has as value \o/
						;on first successful .pollblock they will be set with valid values and things will be checked against correct barrier
		bcs .lz_fetch_sector		;already reached, loop
.lz_fetch_eof					;not reached, go on depacking
		;Y = 0
		pla
		tax
		plp
		rts

.lz_poll
		bit $dd00
		bvs .lz_start_over
		jsr .poll_start			;yes, fetch another block
}
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
		beq .lz_l_page
;		dec <.lz_len_hi			;happens very seldom, so let's do that with lz_l_page that also decrements lz_len_hi, it sets carry and resets Y, what is unnecessary, but happens so seldom it doesn't hurt
.cp_literal
		lda (.lz_src),y			;looks expensive, but is cheaper than loop
		sta (.lz_dst),y
		iny
		dex
		bne .cp_literal

		dey				;this way we force increment of lz_src + 1 if y = 0
		tya				;carry is still set on first round
		adc <.lz_dst + 0
		sta <.lz_dst + 0		;XXX TODO final add of y, could be combined with next add? -> postpone until match that will happen necessarily later on? but this could be called mutliple times for several pages? :-(
		bcc +
		inc <.lz_dst + 1
+
		tya
		sec				;XXX TODO meh, setting carry ...
		adc <.lz_src + 0
		sta <.lz_src + 0
		bcc +
		jsr .lz_next_page		;/!\ this destroys A!
+
		ldy <.lz_len_hi
		bne .lz_l_page			;happens very seldom

		;------------------
		;NEW OR OLD OFFSET
		;------------------

		;in case of type bit == 0 we can always receive length (not length - 1), can this used for an optimization? can we fetch length beforehand? and then fetch offset? would make length fetch simpler? place some other bit with offset?
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
		sec				;need sec here if we want to forgo in the beq .lz_calc_msrc
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
		tax				;remember for later end check, cheaper this way
.lz_offset_lo	sbc #$00			;carry is cleared, subtract (offset + 1)
		sta .lz_msrcr + 0
		lda <.lz_dst + 1
.lz_offset_hi	sbc #$00
		sta .lz_msrcr + 1
.cp_match
.lz_msrcr = * + 1
		lda $beef,y
		sta (.lz_dst),y
		iny
		bne .cp_match
		inc <.lz_dst + 1

		lda <.lz_len_hi			;check for more loop runs
		bne .lz_m_page			;do more page runs

		cpx <.lz_src + 0		;check for end condition when depacking inplace, .lz_dst + 0 still in X
.lz_skip_poll	bne .lz_start_over		;we could check against src >= dst XXX TODO
		lda <.lz_dst + 1
		sbc <.lz_src + 1
		bne .lz_start_over
		rts

		;------------------
		;SELDOM STUFF
		;------------------
.lz_clc
		clc
		bcc .lz_clc_back
.lz_m_page
		dec <.lz_len_hi
		inc .lz_msrcr + 1		;XXX TODO only needed if more pages follow
		bne .cp_match
.lz_l_page
		dec <.lz_len_hi
		sec				;only needs to be set for consecutive rounds of literals, happens very seldom
		ldy #$00
		beq .cp_literal

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
		sta .lz_offset_hi + 1		;hibyte of offset

		lda (.lz_src),y			;fetch another byte directly
		ror
		sta .lz_offset_lo + 1

		inc <.lz_src + 0		;postponed, so no need to save A on next_page call
		bne +
		jsr .lz_next_page
+
						;XXX TODO would be nice to have inverted data sent, but would mean MSB also receives inverted bits? sucks. As soon as we refill bits we fall into loop that checks overflow on LSB, should check for bcc however :-( then things would work
						;would work on offset MSB, but need to clear lz_len_hi after that
		lda #$01
		ldy #$fe
		bcs .lz_match__			;length = 2 ^ $ff, do it the very short way :-)
		ldy #$00
-
		asl <.lz_bits			;fetch first payload bit
						;XXX TODO we could check bit 7 before further asl?
		rol				;can also moved to front and executed once on start
		asl <.lz_bits
		bcc -
		bne .lz_match_
		jsr .lz_refill_bits		;fetch remaining bits
		bcs .lz_match_

.lz_refill_bits
		tax
		lda (.lz_src),y
		rol
		sta <.lz_bits
		inc <.lz_src + 0 		;postponed, so no need to save A on next_page call
		bne +				;XXX TODO if we would prefer beq, 0,2% saving
		jsr .lz_next_page
+
		txa
		bcs .end_bit_16

		;fetch up to 8 bits first, if first byte overflows, stash away byte and fetch more bits as MSB
.lz_get_loop
		asl <.lz_bits			;fetch payload bit
		rol				;can also moved to front and executed once on start
		bcs .get_length_16		;first 1 drops out from lowbyte, need to extend to 16 bit, unfortunatedly this does not work with inverted numbers
.get_length
		asl <.lz_bits
		bcc .lz_get_loop
		beq .lz_refill_bits
.lz_eof
		rts

.get_length_16
		pha				;save LSB
		lda #$01			;start with MSB = 1
		jsr .get_length			;get up to 7 more bits
		sta <.lz_len_hi			;save MSB
		pla				;restore LSB
.end_bit_16
		rts

		;XXX TODO delete lzlen only when entering 16 bit mode? else load lz_len_hi in x as zero?
;.lz_refill_bits
;		lda (.lz_src),y
;		inc <.lz_src + 0
;		bne +
;		jsr .lz_next_page
;+
;		rol
;		bcs .end_bit_16		;last bit, end
;.get_length_
;.lz_get_loop
;		asl
;		rol <.lz_len_lo		;LSB overflows
;		bcs .get_length_16
;.get_length__
;		asl
;		bcc .lz_get_loop
;		beq .lz_refill_bits
;.end_bit_16
;		sta <.lz_bits
;		lda <.lz_len_lo		;load result
;		rts
;.get_length
;		sta <.lz_len_lo		;store inital value ($01)
;		lda <.lz_bits		;load lz_bits to A
;		jmp .get_length__
;.get_length_16
;		ldx <.lz_len_lo		;lowbyte
;		lda #$01
;		jsr .get_length
;		sta <.lz_len_hi		;restore lz_len_hi
;		txa			;LSB in A
;		rts
}

bitfire_resident_size = * - BITFIRE_RESIDENT_ADDR
}	;!zone resident

;XXX TODO
;zx0 needs
;--from $0000
;--to $xxxx
;alternative: --range $0000 $0000
;--use-prefix
;--relocate-packed $0000 -> no more inplace
;--relocate-input $0000 -> relocate beforehand, inplace possible
;--inplace
;--sfx $0000
;--add-load-address $0000 -> make binary to a cbm file, set --cbm afterwards for that purpose -> inplace possible

;set end_address for inplace to real end of file if not inplaced? so nothing can go wrong? or set it to $ffff? also sane


;reduce to $100 max runlen, if max: fetch 1 type bits and decide again: rep, literal? -> with new len?
;in packer, just split up long matches and repeats: 
;after a $100 run: check type bit, 1 = was $100, continue, else fetch anotehr length and do same loop again.


;XXX TODO do eof marker one earlier, place last match/literal after that?, then let final match happen? also okay for overlap version? would avoid other of check? can we do end_marker and bogus match? where to check that eof? after match? last thing can be either match/literal? but hows about rep? :-(


;XXX TODO
;bitnax encoding with 3 bits for offset class, where bit 1 also means 2 byte match, so short offsets have an even number of bits to fetch 3 + even + 1 = even, length = 2, long matches have an odd number of offset bits to fetch 3 + odd + odd length + 1 = even
;decide upon 2 bits with bit <.lz_bits? bmi + bvs + bvc? bpl/bmi decides if repeat or not, bvs = length 2/check for new bits and redecide, other lengths do not need to check, this can alos be used on other occasions?
