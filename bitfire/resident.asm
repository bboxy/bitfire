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
bitfire_load_addr_hi = bitfire_lz_sector_ptr1 + 1
bitfire_load_addr_lo = bitfire_lz_sector_ptr1 + 0
}
}

;depacker zp-addresses
.lz_bits	= BITFIRE_ZP_ADDR + 0 + (BITFIRE_LOADER)
.lz_dst		= BITFIRE_ZP_ADDR + 1 + (BITFIRE_LOADER)
.lz_end		= BITFIRE_ZP_ADDR + 3 + (BITFIRE_LOADER)
.lz_tmp		= BITFIRE_ZP_ADDR + 5 + (BITFIRE_LOADER)

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

;bitfire_send_byte_
;		sta .filenum			;save value
;		ldx #$ff
;		lda #$ef
;		sec				;on first run we fall through bcc and thus end up with carry set and $0f after adc -> with eor #$30 we end up with $3f, so nothing happens on the first $dd02 write
;.bit_loop
;		bcc +
;		adc #$1f
;+
;		eor #$30			;flip bit 5 and toggle bite 4
;		sta $dd02
;		and #$1f			;clear bit
;		pha
;		pla
;		ror <(.filenum - $ff), x	;fetch next bit from filenumber and waste cycles
;		bne .bit_loop			;last bit?
						;carry is set here, important for entering receive loop
						;this all could be done shorter (save on the eor #$30 and invert on floppy side), but this way we save a ldx #$ff later on, and we do not need to reset $dd02 to a sane state after transmission, leaving it at $1f is just fine. So it is worth.

		;try again to swap bits, but do no eor #$30 but eor $20 here only, rest of the eor should happen on driveside, so we end with a sane $dd02 value taht is good for the adc $dd00?
		;

;bitfire_send_byte_
;		;one byte longer, but saves us teh ldx #$ff later on on depacking
;		sta <.filenum
;		ldx #$08			;do 9 runs to end up with a sane value ($1f) and waste a bit of time after filename is sent to give floppy time to enter busy mode
;		lda #$2f
;-
;		lsr <.filenum
;		bcs +
;		ora #$10
;+
;		eor #$20
;		sta $dd02
;		and #$2f
;		pha
;		pla
;		dex
;		bpl -
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
		pha

!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		jsr .get_one_byte		;fetch barrier
		sta .barrier
}
.bitfire_load_block
		jsr .get_one_byte		;fetch blockaddr hi
		sta .bitfire_block_addr + 1	;where to place the block?
		jsr .get_one_byte
		sta .bitfire_block_addr + 0
		tax

		pla				;XXX TODO pha + pla is really annoying. how to catch that ack val, or can we put it into another get_one_byte?
		bmi .skip_load_addr		;#$fc -> first block, fetch load-address
		lda .bitfire_block_addr + 1	;fetch loadaddr hi

		stx bitfire_load_addr_lo
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		stx bitfire_lz_sector_ptr2
}
		sta bitfire_load_addr_hi
!if BITFIRE_DECOMP = 1 {			;decompressor only needs to be setup if there
		sta bitfire_lz_sector_ptr2 + 1
}
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

		;33 and 3b
		;%bb0001xx			lda $dd00
		;%0bb0001x			lsr
		;%00bb0000			asr #$fc
		;%bbbb01xx			adc $dd00
		;%0bbbb01x			lsr
		;%00bbbb01			lsr
		;%bbbbbexx			eor $dd00
		;%0bbbbbex			lsr
		;%00bbbbbe			lsr
		;%11bbbbbb			eor #$c1
		;				sax .nibble + 1
		;ccxxxxxxx			and $dd00
		;				ora #$00

;.get_one_byte
;		ldx #$8e			;opcode for stx	-> repair any rts being set (also accidently) by y-index-check
;		stx .finish
;		bne .get_entry
;.get_loop
;		ldx #$3f
;
;		ora $dd00
;		stx $dd02			;19	-> 14 on 1541
;
;		nop
;		ora #06
;		lsr				;0xxxx011
;		lsr				;00xxxx01
;		ldx #$37
;
;		ora $dd00
;		stx $dd02			;16	-> 16 on 1541
;
;		ror				;100xxxx0	1
;		ror				;1100xxxx	0
;		clc
;		nop
;		ldx #$3f
;		sax .nibble + 1
;		and $dd00
;		stx $dd02			;18	-> 19 on 1541
;
;.nibble		ora #$00
;.bitfire_block_addr = * + 1
;.store		sta $b00b,y
;.get_entry
;		ldx <BITFIRE_LAX_ADDR
;		nop
;		nop
;		lda $dd00
;.finish		stx $dd02			;17	-> 14 on 1541
;
;		lsr
;		lsr
;		dey
;		bne .get_loop
;.last
;		ldx #$60
;		stx .finish
;		bpl .get_loop



!if >* != >.get_loop { !error "getloop code crosses page!" }
;XXX TODO in fact the branch can also take 4 cycles if needed, ora $dd00 - $3f,x wastes one cycle anyway

}
!if BITFIRE_DECOMP = 1 {

;---------------------------------------------------------------------------------
; OFFSET TABLES
;---------------------------------------------------------------------------------

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

;---------------------------------------------------------------------------------
; REFILL ROUTINES
;---------------------------------------------------------------------------------

.lz_refill_bits
bitfire_lz_sector_ptr1	= * + 1
		ldy $beef,x
						;store bits? happens on all calls, except when a whole literal is fetched
		bcc +				;only store lz_bits if carry is set (in all cases, except when literal is fetched for offset)
		sty .lz_bits			;preserve A this way
		rol .lz_bits
+
		inx
		bne .lz_same_page

.lz_next_page					;/!\ ATTENTION things entered here as well during depacking
		inc bitfire_lz_sector_ptr1 + 1	;use inc to keep A untouched!
		inc bitfire_lz_sector_ptr2 + 1	;Z flag should never be set, except when this wraps around to $00, but then one would need to load until $ffff?
.lz_next_page_
.lz_skip_fetch
!if BITFIRE_LOADER = 1 {
		php				;turned into a rts in case of standalone decomp
		pha				;preserve Z, carry, A and Y, needed depending on call
		sty .lz_tmp
.lz_fetch_sector				;entry of loop
		jsr .pollblock			;fetch another block
		bcs .lz_fetch_eof		;eof? yes, finish, only needed if files reach up to $ffxx -> barrier will be 0 then and upcoming check will always hit in -> this would suck
		lda bitfire_lz_sector_ptr1 + 1	;get current depack position
		cmp .barrier			;next pending block/barrier reached? If barrier == 0 this test will always loop on first call or until first-block with load-address arrives, no matter what .bitfire_lz_sector_ptr has as value \o/
						;on first successful .pollblock they will be set with valid values and things will be checked against correct barrier
		bcs .lz_fetch_sector		;already reached, loop
.lz_fetch_eof					;not reached, go on depacking
		ldx #$00			;restore x = 0 again
		ldy .lz_tmp			;restore regs + flags
		pla
		plp				;XXX TODO plp + rts != rti :-( as pc is off by one
}
.lz_same_page
!if BITFIRE_LOADER = 0 {
.lz_end_of_file					;point to rts, this is always reachable \o/
}
		rts



!if BITFIRE_NMI_GAPS = 1 {
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

!if BITFIRE_DECOMP = 1 {
bitfire_decomp_
	!if BITFIRE_LOADER = 1 {
.lz_end_of_file	= * + 1				;point to rts, this is always reachable \o/
		lda #$60			;disable calls
		ldy #.lz_skip_end-.lz_skip_poll-2	;#$17
		ldx #$00			;start with first byte of block
		beq .loadcompd_entry
	!if BITFIRE_FRAMEWORK = 1 {
link_load_next_comp
		lda #BITFIRE_LOAD_NEXT		;XXX TODO duplicate code
link_load_comp
	}
bitfire_loadcomp_
		jsr bitfire_send_byte_		;returns now with x = $ff
		lda #$08			;enable pollblock/fetch_sector calls (php)
		ldy #.lz_poll-.lz_skip_poll-2	;currently ldy #$0b
		;ldx #$ff			;force to load a new sector upon first read, first read is a bogus read and will be stored on lz_bits, second read is then the really needed data
.loadcompd_entry
		sta .lz_skip_fetch
		sty .lz_skip_poll + 1
		;address stuff is already set by loadraw_/pollblock
	}
}
;---------------------------------------------------------------------------------
; DECRUNCHER
;---------------------------------------------------------------------------------

.lz_decrunch
-
		jsr .lz_refill_bits		;fetch depack addr
		sty .lz_dst-1,x			;x = 0, x = 1 and x = 2
!if BITFIRE_DECOMP_ZERO_OVERLAP = 0 {
		cpx #$02
} else {
		cpx #$04
}
		bne -
		;sec				;set for free by last compare
.lz_type_refill
		jsr .lz_refill_bits		;refill bit buffer .lz_bits
						;called only once per depack with X = 2 or 4

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

.lz_lrun_gotten
		sta .lz_lcopy_len		;Store LSB of run-length
		ldy #$00
.lz_lcopy
bitfire_lz_sector_ptr2	= * + 1			;Copy the literal data, forward or overlap is getting a pain in the ass.
		lda $beef,x
		sta (.lz_dst),y
		inx
		bne +
		jsr .lz_next_page
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
-
		asl .lz_bits
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs -

		beq .lz_8_and_more		;0 + 8 bits to fetch, branch out before table lookup to save a few cycles and one byte in the table, also save complexity on the bitfetcher
		tay
		lda .lz_lentab,y		;XXX TODO can we in fact choose from either offset group? $80 exists twice in that tab :-(
-						;same as above
		asl .lz_bits			;XXX same code as above, so annoying :-(
		bne *+5
		jsr .lz_refill_bits
		rol
		bcs -

		bmi .lz_less_than_8		;either 3,4,6 or 7 bits fetched -> highbyte will be $ff
.lz_8_and_more
		jsr .lz_refill_bits
		eor #$ff			;5 of 13, 2 of 10, 0 of 8 bits fetched as highbyte, lowbyte still to be fetched
		sta .lz_tmp			;XXX this is a pain in the arse that A and Y need to be swapped :-(
		tya
		ldy .lz_tmp
		top
.lz_less_than_8
		ldy #$ff			;XXX TODO silly, y is set twice in short case
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
.lz_skip_poll	bcc +
.lz_maximum	inc .lz_dst+1			;this is also used by maximum length
		bcs .lz_skip_end
+

} else {
		bcc +				;proceed to check
.lz_maximum
		inc .lz_dst+1			;advance hi byte
;		lda .lz_dst			;if entering via .lz_maximum, a = 0, so we would pass the following check only if the endadress is @ $xx00
+						;if so, the endaddress can't be $xx00 and the highbyte check will fail, as we just successfully wrote a literal with type bit, so the end address must be greater then the current lz_dst, as either another literal or match must follow. Can you still follow me?! :-D
		eor .lz_end			;check end address
.lz_skip_poll	bne .lz_poll			;all okay, poll for a new block

		eor .lz_dst+1			;check highbyte
		eor .lz_end+1
		bne .lz_skip_end		;skip poll, so that only one branch needs to be manipulated
		;sta .barrier			;clear barrier and force to load until EOF, XXX does not work, but will at least force one additional block before leaving as barrier will be set again upon next block being fetched. Will overlap be > than 2 blocks? most likely not? CRAP, tony taught me that there is /o\
		lda #$ff
		sta bitfire_load_addr_hi	;needed if the barrier method will not work out, plain jump to poll loop will fail on stand alone depack?
		jmp .lz_next_page_		;load any remaining literal blob if there, or exit with rts in case of plain decomp (rts there instead of php). So we are forced until either the sector_ptr reaches $00xx or EOF happens, so nothing can go wrong
						;XXX TODO could be beq .lz_next_page_ but we get into trouble with 2nd nmi gap then :-(
}

.lz_poll
!if BITFIRE_LOADER = 1 {
						;XXX TODO can be omitted as done in pollblock, but a tad faster this way
		bit $dd00
		bvs .lz_skip_end

		stx .lz_tmp			;save x, lz_tmp is available at that moment
		jsr .poll_start			;yes, fetch another block
		ldx .lz_tmp			;restore x
}
.lz_skip_end
						;literals needing an explicit type bit
		asl .lz_bits			;fetch next type bit
		jmp .lz_type_check
						;XXX TODO refill_bits -> do no shifting yet, but do in code, so we could reuse the asl ?!
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
