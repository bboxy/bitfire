!cpu 6510

;EXAMPLE FOR OFFICIAL VERSION

lz_sector      = ($ffff - (data_end-data) + 1) & $ff00

decruncher = $0020

		* = $0801
		!byte $1c,$08,$00,$00,$9e,$32,$30,$37,$38,$3a,$22,$14,$14,$14,$14
		!byte $14,$14,$14,$14,$14,$42,$49,$54,$4e,$41,$58,$00,$00,$00
		;!byte $0b,$08,$39,$05,$9e,$32
		;!byte $30,$36,$31,$00,$00,$00

		sei
		inc $01

		ldx #$00
		;txs

		;inx
-
		lda copy_start,x
		sta decruncher,x
		inx
		bne -

		ldy #(>(data_end-data)) + 1
-
		;src should be data + packed_size
		dex
src		lda data_end-$100,x
dst		sta $ff00,x
		txa
		bne -

		dec src+2
		dec dst+2
		dey
		bne -

		ldx #<($ffff - (data_end-copy_end) + 1)
		jmp go

copy_start
!pseudopc decruncher {
		;fetch depack addr (use --add-depack-addr on lz)
go
		;******** Start the next match/literal run ********
lz_decrunch
		;XXX TODO lz_bist auch gleich passend füllen bei sfx, nicht in stream schreiben?
		sec			;This is the main entry point. Forcibly
_lz_type_refill	jsr _lz_refill_bits	;fill up the the bit buffer on entry
_lz_type_check	bcc _lz_do_match
		beq _lz_type_refill

		;******** Process literal run ********

		lda #%00000000		;Decode run length
-
                rol
                asl+1 lz_bits
                bne *+5
                jsr _lz_refill_bits
                bcc _lz_lrun_gotten

                asl+1 lz_bits
                bne -
                jsr _lz_refill_bits
                bne -

_lz_lrun_gotten
		sta+1 _lz_copy_cnt+1	;Store LSB of run-length
                ldy #$00
_lz_lcopy
lz_sector_ptr2	= *+1			;Copy the literal data.
		lda+2 lz_sector,x
		inx
		bne *+5
		jsr lz_fetch_sector
lz_dst = * + 1
		sta $3800,y
		iny
_lz_copy_cnt	cpy #$00
		bne _lz_lcopy

		;Time to advance the destination pointer.
		;Maximum run length literals exit here as a type-bit needs
		;to be fetched afterwards
		tya
		bne *+5
		jmp _lz_maximum		;maximum literal run, bump sector pointers and so on
		clc
		adc+1 lz_dst+0
		sta+1 lz_dst+0
		bcc _lz_do_match
		inc+1 lz_dst+1

		;******** Process match ********
_lz_do_match
                lda #%00000001
                asl+1 lz_bits
                bne *+5
                jsr _lz_refill_bits
                bcs _lz_mrun_start
_lz_mrun_loop
                asl+1 lz_bits             ;fetch bit 2
                bne *+5
                jsr _lz_refill_bits
                bcc out                 ;clear? all ok, else 8 bits are enough, skip last stopbit and exit

                asl+1 lz_bits
                bne *+5
                jsr _lz_refill_bits
                rol
                bcc _lz_mrun_loop
                bcs _lz_end_of_file     ;A >= 258-byte run (8 bits received) serves as a sentinel
out
                adc #$01
_lz_mrun_start
		sta+1 _lz_mcopy_len

		lda #%00100000		;Determine offset length by a two-bit
		rol
_lz_moff_range	asl+1 lz_bits		;prefix combined with the first run
		bne *+5			;length bit (where a one identifies
		jsr _lz_refill_bits	;a two-byte match).
		rol			;The rest of the length bits will
		bcc _lz_moff_range	;then follow *after* the offset data

		tay
		lda _lz_moff_length,y
		beq _lz_moff_far

_lz_moff_loop	asl+1 lz_bits		;Load partial offset byte
		bne +
		sty+1 _lz_y
		jsr _lz_refill_bits
_lz_y = * + 1
		ldy #$00
+
		rol
		bcc _lz_moff_loop

		bmi _lz_moff_near

_lz_moff_far	sta+1 _lz_hi		;Save the bits we just read as the
					;high-byte

lz_sector_ptr3	= *+1
		lda+2 lz_sector,x	;For large offsets we can load the
		inx			;low-byte straight from the stream
		bne *+5			;without going throught the shift
		jsr lz_fetch_sector	;register

;		sec
		adc _lz_moff_adjust_lo,y ;y .. 2 .. 5? ?! necessary with a full lowbyte?!?!
		bcs _lz_moff_pageok
		dec+1 _lz_hi
		sec
_lz_moff_pageok	adc+1 lz_dst+0
		sta+1 lz_match+0

_lz_hi= * + 1
		lda #$00
		adc _lz_moff_adjust_hi,y
		sec
		bcs _lz_moff_join	;(BRA)

_lz_moff_near
;		sec			;Special case handling of <8 bit offsets.
	 	adc _lz_moff_adjust_lo,y;We may can safely ignore the MSB from
;		sec			;the base adjustment table as the
		adc+1 lz_dst+0		;maximum base (for a 4/5/6/7 bit
		sta+1 lz_match+0	;length sequence) is 113
		lda #$ff
_lz_moff_join	adc+1 lz_dst+1
		sta+1 lz_match+1

		ldy #$ff		;The copy loop. This needs to be run
					;forwards since RLE-style matches can overlap the destination
_lz_mcopy
		iny
lz_match = * + 1
		lda $1000,y		;Copy one byte
		sta (lz_dst),y
_lz_mcopy_len	= *+1
		cpy #$ff
		bne _lz_mcopy

		tya			;Advance destination pointer
;		sec
		adc+1 lz_dst+0
		sta+1 lz_dst+0
		;Wrap the high-byte of the destination pointer.
		bcc *+4
_lz_maximum	inc+1 lz_dst+1		;This is also used by maximum length
					;literals needing an explicit type bit

		asl+1 lz_bits
		jmp _lz_type_check
lz_bits 	!byte $00

lz_sector_ptr1	= *+1
_lz_refill_bits	ldy+2 lz_sector,x
		sty+1 lz_bits
;		sec
		rol+1 lz_bits
		inx
		bne +

lz_fetch_sector
		inc lz_sector_ptr1+1
		inc lz_sector_ptr2+1
		inc lz_sector_ptr3+1
+
		rts

_lz_end_of_file

		dec $01
		cli
		sta $98			;keep away trouble from fastload installers and start with 0 opened files
		!byte $4c

_lz_moff_length = * + 2
_lz_moff_adjust_lo = _lz_moff_length + 8
_lz_moff_adjust_hi = _lz_moff_length + 16

}
copy_end = * + 26
data
!bin "broken_files/21.pet.lz",,2
data_end

!if copy_end-copy_start > $100 {
	!warn "depackersize > $100"
}

