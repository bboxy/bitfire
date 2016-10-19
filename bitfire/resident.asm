!zone resident {
!src "../music.inc"
!src "config.inc"
!convtab pet
!cpu 6510

;loader zp-addresses
.barrier	= BITFIRE_ZP_ADDR + 0
.filenum	= .barrier

;depacker zp-addresses
.lz_bits	= BITFIRE_ZP_ADDR + 1
.lz_dst		= BITFIRE_ZP_ADDR + 2
.lz_match	= BITFIRE_ZP_ADDR + 4

!if BITFIRE_DEBUG = 1 {
bitfire_debug_filenum	= BITFIRE_ZP_ADDR + 6
}

;define that label here, as we only aggregate labels from this file into loader_*.inc
bitfire_install_ = BITFIRE_INSTALLER_ADDR

!if BITFIRE_DECOMP = 0 {
bitfire_load_addr_hi = .filenum			;in case of no loadcompd, store the highbyte of loadaddress separatedly
}

		* = BITFIRE_RESIDENT_ADDR

!if BITFIRE_FRAMEWORK = 1 & BITFIRE_FRAMEWORK_FRAMECOUNTER = 1 {
link_frame_count
		!word 0
}

!if BITFIRE_NMI_GAPS = 1 & BITFIRE_DEBUG = 0 {
!align 255,2
		nop
		nop
		nop
		nop
		nop
		nop
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
link_decomp	= bitfire_decomp_

!if BITFIRE_DECOMP = 1 {
;		;expect $01 to be $35
link_load_next_double
		;loads a splitted file, first part up to $d000 second part under IO
		jsr link_load_next_comp
link_load_next_raw_decomp
		jsr link_load_next_raw
link_decomp_under_io
		dec $01				;bank out IO
		jsr link_decomp			;depack
		inc $01				;bank in again
		rts
}

}

bitfire_send_byte_
		;XXX we do not wait for the floppy to be idle, as we waste enough time with depacking or the fallthrough on load_raw to have an idle floppy

		ldx #$07			;do 8 turns, as the last turn sets $dd02 at least back to $1f or $3f this is enough to get the idle signal on first pollblock, but not EOF yet (but we load one block minimum, right? So things are healed after the first get_byte call that sets back $dd02 to $3f in any case.)
		sta .filenum			;save value
		lda #$1f			;start value
.bit_loop
		lsr .filenum			;fetch next bit from filenumber and waste cycles
		bcc +
		ora #$20
+
		eor #$30			;flip bit 4 and 5
		sta $dd02			;only write out lower 6 bits
		and #$1f			;clear bit 4 and waste some cycles here
		pha				;slow down, or floppy might not keep up, most of all if NTSC
		pla
		dex
		bpl .bit_loop			;last bit?
						;this all could be done shorter (save on the eor #$30 and invert on floppy side), but this way we save a ldx #$ff later on, and we do not need to reset $dd02 to a sane state after transmission, leaving it at $1f is just fine. So it is worth.
						;also enough cycles are wasted after last $dd02 write, just enough for standalone, full config and ntsc \o/
		rts

!if BITFIRE_DECOMP = 1 {
.bitfire_set_ptrs
		sta bitfire_lz_sector_ptr1 - $3f,x
		sta bitfire_lz_sector_ptr2 - $3f,x
		sta bitfire_lz_sector_ptr3 - $3f,x
		rts
}

!if BITFIRE_FRAMEWORK = 1 {
link_load_next_raw
		lda #BITFIRE_LOAD_NEXT
link_load_raw
}

bitfire_loadraw_
		jsr bitfire_send_byte_		;easy, open...
!if BITFIRE_DECOMP = 1 {
.lz_pollloop
-
		jsr .pollblock
		bcc -
;		rts				;just run into pollblock code again that will then jump to .poll_end and rts
}
.pollblock
		lda $dd00			;bit 6 is always set if not ready or idle/EOF so no problem with just an ASL
		asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
		bmi .poll_end			;block ready?
.poll_start
		lda #$60			;set rts
		jsr .bitfire_ack_		;signal that we accept data and communication direction, by basically sending 2 atn strobes by fetching a bogus byte (6 bits are used for barrier delta, first two bist are cleared/unusable. Also sets an rts in receive loop

		bpl .skip_load_addr		;#$fc -> first block, all positive numbers = delta for barrier << 2

!if BITFIRE_DEBUG = 1 {
		jsr .get_one_byte		;fetch filenum
		sta bitfire_debug_filenum
}

		jsr .get_one_byte		;fetch load/blockaddr lo
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		jsr .bitfire_set_ptrs		;set initial pointers
}
		sta bitfire_load_addr_lo	;destination lowbyte

		jsr .get_one_byte		;fetch loadaddr hi, returns with a cleared carry
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		inx
		jsr .bitfire_set_ptrs		;set initial pointers highbyte
		;sta bitfire_load_addr_hi	;same as .bitfire_lz_sector_ptr1 + 1
		top				;skip 2 lsr, carry is clear and .barrier = 0, so it is basically sta .barrier
.skip_load_addr
		lsr				;lower two bits always cleared, can ommit clc
		lsr
		adc .barrier
		sta .barrier			;updated or initial barrier position = loadaddr_hi
} else {
		sta bitfire_load_addr_hi
.skip_load_addr
}

.bitfire_load_block
		jsr .get_one_byte		;fetch blockaddr hi
		sta .bitfire_block_addr_hi	;where to place the block?

		jsr .get_one_byte		;fetch blocklen
		tax
		lda #$a2			;ldx #imm
.bitfire_ack_
		sta .blockpos
		ldy #$37
.get_one_byte
bitfire_ntsc_fix1				;ntsc fix will be done on those labels by installer (opcode $xxxx-$37,y)
		lda $dd00-$37,y
		sty $dd02
		lsr
		lsr
		stx .blockpos+1			;store initial x, and in further rounds do bogus writes with correct x value anyway, need to waste 4 cycles, so doesn't matter. Saves a byte (tax + stx .blockpos+1) compared to sta .blockpos+1 + nop + nop.
		ldx #$3f

bitfire_ntsc_fix2
		ora $dd00-$37,y			;can be omitted? 3 cycles overhead
		stx $dd02
		lsr
		lsr
		dec .blockpos+1			;waste 6 cycles and decrement

bitfire_ntsc_fix3
		ora $dd00-$37,y			;now ATN is 0 and ora can happen without killing bit 3
		sty $dd02
		lsr
		asr #$7e			;clear carry for free
		sta .nibble + 1
		lda #$c0

bitfire_ntsc_fix4
		and $dd00-$37,y			;can be omitted? 2 cycles overhead
		stx $dd02
.nibble		ora #$00			;also adc could be used, or sbc -nibble?
.blockpos	ldx #$00
.bitfire_block_addr_hi = * + 2
bitfire_load_addr_lo = * + 1
		sta $b00b,x
		bne .get_one_byte		;74 cycles per loop

!if >* != >.get_one_byte { !error "getloop code crosses page!" }
.poll_end
!if BITFIRE_DECOMP = 0 {
		bcc .pollblock
}
		rts

!if BITFIRE_DECOMP = 1 {
;---------------------------------------------------------------------------------
; REFILL ROUTINES
;---------------------------------------------------------------------------------

.lz_refill_bits
bitfire_lz_sector_ptr1	= * + 1
bitfire_load_addr_hi = * + 2
		ldy $beef,x
		sty .lz_bits
		rol .lz_bits
		inx
		bne .lz_same_page

.lz_next_page					;/!\ ATTENTION things entered here as well during depacking
		inc bitfire_lz_sector_ptr1 + 1	;use inc to keep A untouched!
		inc bitfire_lz_sector_ptr2 + 1
		inc bitfire_lz_sector_ptr3 + 1
.lz_next_page_
.lz_skip_fetch
		php				;turned into a rts in case of standalone decomp
		pha				;preserve Z, carry, A and Y, needed depending on call
		sty .lz_match
.lz_fetch_sector				;entry of loop
		jsr .pollblock			;fetch another block, returns with x = 0
		bcs .lz_fetch_eof		;eof? yes, finish
		lda bitfire_lz_sector_ptr1 + 1	;get current depack position
		cmp .barrier			;next pending block/barrier reached? If barrier == 0 this test will always loop on first call, no matter what .bitfire_lz_sector_ptr has as value \o/
						;on first successful .pollblock they will be set with valid values and things will checked against correct barrier
		bcs .lz_fetch_sector		;already reached, loop
.lz_fetch_eof					;not reached, go on depacking
		ldy .lz_match			;restore regs + flags
		pla
		plp
.lz_same_page
		rts

!if BITFIRE_FRAMEWORK = 1 {
link_load_next_comp
		lda #BITFIRE_LOAD_NEXT
link_load_comp
}

bitfire_loadcomp_
		jsr bitfire_send_byte_		;returns now with x = $ff
		lda #$08			;enable pollblock/fetch_sector calls (php)
		ldy #.lz_poll-.lz_skip_poll-2	;currently ldy #$0b
		;ldx #$ff			;force to load a new sector upon first read, first read is a bogus read and will be stored on lz_bits, second read is then the really needed data
		bne .loadcompd_entry		;load + decomp file

!if BITFIRE_NMI_GAPS = 1 & BITFIRE_DEBUG = 0 {
.lz_gap2
						;jmp will be placed here
!align 255,5
!if * - .lz_gap2 < 3 {
	!error "too few bytes left for second gap :-("
}
}

bitfire_decomp_
.lz_end_of_file	= * + 1				;point to rts, this is always reachable \o/
		lda #$60			;disable calls
		ldy #.lz_skip_end-.lz_skip_poll-2	;#$17
		ldx #$00			;start with first byte of block
.loadcompd_entry
		sta .lz_skip_fetch
		sty .lz_skip_poll + 1
		;address stuff is already set by loadraw_/pollblock

;---------------------------------------------------------------------------------
; DECRUNCHER
;---------------------------------------------------------------------------------

.lz_decrunch
!if BITFIRE_DECOMP_ZERO_OVERLAP = 0 {
-
		jsr .lz_refill_bits		;fetch depack addr
		sty .lz_dst-1,x			;x = 0, x = 1 and x = 2
		cpx #$02
		bne -
} else {
-
		tya				;copy previous Y to A for later use
		jsr .lz_refill_bits		;fetch depack addr and end addr, preserves A
		sty .lz_dst-1,x			;x = 0 .. x = 4
		cpx #$04			;fetch 2 more bytes
		bne -
		sta .lz_end_low+1		;a = previous fetched byte
		sty .lz_end_hi+1		;y = last fetched byte
						;voila, there's our end address \o/
}
		;sec				;set for free by last compare
.lz_type_refill
		jsr .lz_refill_bits		;refill bit buffer .lz_bits

		;******** Start the next match/literal run ********
.lz_type_check
		bcc .lz_do_match
		beq .lz_type_refill		;we will fall through on entry

		;******** Process literal run ********

		lda #$00
-
		rol				;-> a = $01 after first round
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits		;kills y
		bcc .lz_lrun_gotten

		asl .lz_bits
		bne -
		jsr .lz_refill_bits
		bne -

-
		jsr .lz_next_page
		bne +
.lz_lrun_gotten
		sta .lz_lcopy_len		;Store LSB of run-length
		ldy #$00
.lz_lcopy
bitfire_lz_sector_ptr2	= * + 1			;Copy the literal data, forward or overlap is getting a pain in the ass.
		lda $beef,x
		sta (.lz_dst),y
		inx
		beq -
+
		iny
.lz_lcopy_len = * + 1
		cpy #$00
		bne .lz_lcopy

		tya
		beq .lz_maximum			;maximum literal run, bump sector pointers and so on and force new type bit
						;XXX TODO can we reuse the same code? In one case continue with match, in other case redecide
		clc
		adc .lz_dst
		sta .lz_dst
		bcc .lz_do_match
		inc .lz_dst+1
						;no need for a type bit, after each literal a match follows, except for maximum runlength literals

		;******** Process match ********

.lz_do_match
		lda #$01			;this could be made shorter by using the last bitfetch of the upcoming loop and restoring the carry again by a cmp #$02. Saves bytes, but makes things slower, as eof check is also done with all short matches then

		asl .lz_bits			;first length bit (where a one identifies
		bne *+5				;a two-byte match)
		jsr .lz_refill_bits
		bcc .lz_get_offs		;all done, length is 2, skip further bitfetches (and eof check)
-
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol

		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		bcc -
.lz_got_len
		tay				;XXX TODO could this be placed elsewhere to make the tay obsolete?
		beq .lz_end_of_file		;A 257-byte (=>$00) run serves as a sentinel, but not with zero-overlap, except when depacking from a non inplace address, then it is still appended
.lz_get_offs
		sta .lz_mcopy_len		;store length at final destination

		lda #%11000000			;fetch 2 more prefix bits
		rol				;previous bit is still in carry \o/

		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol

		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol

		beq .lz_far			;0 + 8 bits to fetch, branch out before table lookup to save a few cycles and one byte in the table
		tay
		lda .lz_lentab,y
-						;same as above
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs -

		bmi .lz_short			;either 3,4,6 or 7 bits fetched -> highbyte will be $ff
.lz_far
		eor #$ff			;5 of 13, 2 of 10, 0 of 8 bits fetched as highbyte, lowbyte still to be fetched
		tay

bitfire_lz_sector_ptr3	= * + 1
		lda $beef,x			;For large offsets we can load the
		inx				;low-byte straight from the stream
		bne .lz_join			;without going throught the shift
		jsr .lz_next_page		;register
		top
.lz_short
		ldy #$ff			;XXX TODO silly, y is set twice in short case
.lz_join
		adc .lz_dst			;subtract offset from lz_dst
		sta .lz_m+1
		tya				;hibyte
		adc .lz_dst+1
		sta .lz_m+2

		ldy #$ff			;The copy loop. This needs to be run
						;forwards since RLE-style matches can overlap the destination
.lz_mcopy
		iny
.lz_m		lda $face,y			;copy one byte
		sta (.lz_dst),y
.lz_mcopy_len	= * + 1
		cpy #$ff
		bne .lz_mcopy

		tya				;advance destination pointer
;		sec				;XXX TODO carry set = type check needed, cleared (literal) = match follows anyway
		adc .lz_dst
		sta .lz_dst

!if BITFIRE_DECOMP_ZERO_OVERLAP = 0 {
.lz_skip_poll	bcc .lz_poll
.lz_maximum	inc .lz_dst+1			;this is also used by maximum length
		bcs .lz_skip_end

} else {
		bcc .lz_end_low			;proceed to check
.lz_maximum
		inc .lz_dst+1			;advance hi byte
;		lda .lz_dst			;if entering via .lz_maximum, a = 0, so we would pass the following check only if the endadress is @ $xx00
						;if so, the endaddress can't be $xx00 and the highbyte check will fail, as we just successfully wrote a literal with type bit, so the end address must be greater then the current lz_dst
.lz_end_low	eor #$00			;check end address
.lz_skip_poll	bne .lz_poll			;all okay, poll for a new block
		lda .lz_dst+1			;check highbyte
.lz_end_hi	eor #$00
		bne .lz_skip_end		;skip poll, so that only one branch needs to be manipulated
		sta .barrier			;clear barrier and force to load until EOF
		jmp .lz_next_page_		;load any remaining literal blob if there, or exit with rts in case of plain decomp (rts there instead of php). So we are forced until either the sector_ptr reaches $00xx or EOF happens, so nothing can go wrong
}

.lz_poll
		bit $dd00
		bvs .lz_skip_end

		stx .lz_match			;save x, lz_match is available at that moment
		jsr .pollblock			;yes, fetch another block and mark block in bitmap
		ldx .lz_match			;restore x
.lz_skip_end
						;literals needing an explicit type bit
		asl .lz_bits			;fetch next type bit
		jmp .lz_type_check
						;XXX TODO refill_bits -> do no shifting yet, but do in code, so we could reuse the asl ?!
.lz_lentab = * - 1
		;short offset init values
		;!byte %00000000			;2
		!byte %11011111			;0
		!byte %11111011			;1
		!byte %10000000			;3

		;long offset init values
		!byte %11101111			;offset 0
		!byte %11111101			;offset 1
		!byte %10000000			;offset 2
		!byte %11110000			;offset 3
}	;endif DECOMP

!if BITFIRE_AUTODETECT = 1 {
link_chip_types
link_sid_type			;%00000001	;bit set = new, bit cleared = old
link_cia1_type			;%00000010
link_cia2_type			;%00000100
		!byte $00
}

bitfire_resident_size = * - BITFIRE_RESIDENT_ADDR
}	;!zone resident
