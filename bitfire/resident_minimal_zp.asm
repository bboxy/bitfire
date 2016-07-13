!zone resident {
!src "../music.inc"
!src "config.inc"
!convtab pet
!cpu 6510


;define that label here, to not collect labels from installer too
bitfire_install_ = BITFIRE_INSTALLER_ADDR

		* = BITFIRE_RESIDENT_ADDR
;XXX TODO set filenum in .nibble -> will be overwritten anyway
;bitfire_load_addr_hi
;		!byte $00
bitfire_loadraw_
		ldx #$08			;do 9 turns, as the last turn sets $dd02 at least back to $1f, enough to get the idle signal on first pollblock, but not EOF yet (but we load one block minimum, right? So things are healed after the first get_byte call.)
		sta .filenum			;save value
		lda #$20			;start value
.bit_loop
		lsr .filenum			;fetch next bit from filenumber and waste cycles
		adc #$0f			;bit 4 depending on carry -> adc #$00/01
		ora #$0f			;fill up all lower bits, in case adc did add with carry set
		eor #$30			;flip bit 4 and 5
		sta $dd02			;only write out lower 6 bits
		and #$20			;clear bit 4 and waste some cycles here
		dex
		bpl .bit_loop			;last bit?
						;this all could be done shorter (save on teh eor #$30 and invert on floppy side), but this way we save a ldx #$ff later on, and we do not need to reset $dd02 to a sane state after transmission, leaving it at $1f is just fine. So it is worth.
.pollblock
		lda $dd00
		asl				;focus on bit 7 and 6 and copy bit 7 to carry (set if floppy is idle/eof is reached)
		bmi .poll_end			;block ready?
.poll_start
		lda #$60			;set rts
		jsr .bitfire_ack_		;signal that we accept data and communication direction, by basically sending 2 atn strobes by fetching a bogus byte (6 bits are used for barrier delta, first two bist are cleared/unusable. Also sets an rts in receive loop

		bpl .skip_load_addr		;#$fc -> first block, all positive numbers = delta for barrier << 2

		jsr .get_one_byte		;fetch load/blockaddr lo
		sta <bitfire_load_addr_lo

		jsr .get_one_byte		;fetch loadaddr hi, returns with a cleared carry
		;sta bitfire_load_addr_hi	;XXX in standlone mode this is optional, only needed if you need to know the load address of the file for depacking
.skip_load_addr
.bitfire_load_block
		jsr .get_one_byte		;fetch blockaddr hi
		sta <.bitfire_block_addr_hi	;where to place the block?

		jsr .get_one_byte		;fetch blocklen
		tax
		lda #$a2			;ldx #imm
.bitfire_ack_
		sta <.blockpos
		ldy #$37
.get_one_byte
bitfire_ntsc_fix1				;ntsc fix will be done on those labels by installer (opcode $xxxx-$37,y)
		lda $dd00
		sty $dd02
		lsr
		lsr
		stx <(.blockpos - $37 + 1),y	;store initial x, and in further rounds do bogus writes with correct x value anyway, need to waste 4 cycles, so doesn't matter. Saves a byte (tax + stx .blockpos+1) compared to sta .blockpos+1 + nop + nop.
		ldx #$3f

bitfire_ntsc_fix2
		ora $dd00
		stx $dd02
		lsr
		lsr
		dec <(.blockpos - $3f + 1),x	;waste 6 cycles and decrement

bitfire_ntsc_fix3
		ora $dd00			;now ATN is 0 and ora can happen without killing bit 3
		sty $dd02
		lsr
		asr #$7e			;clear carry for free
		sta <(.nibble - $3f + 1),x
		lda #$c0

bitfire_ntsc_fix4
		and $dd00
		stx $dd02
.nibble		ora #$00			;also adc could be used, or sbc -nibble?
.blockpos	ldx #$00
.bitfire_block_addr_hi = * + 2
bitfire_load_addr_lo = * + 1
		sta $b00b,x
		bne .get_one_byte		;74 cycles per loop

!if >* != >.get_one_byte { !error "getloop code crosses page!" }
.poll_end
		bcc .pollblock
		rts

bitfire_resident_size = * - BITFIRE_RESIDENT_ADDR
}	;!zone resident
