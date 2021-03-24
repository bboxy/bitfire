!cpu 6510

.decruncher 		= $0020
.lz_sector      	= ($10000 - (.bitnax_packed_size)) & $ff00

.bitnax_decrunch_offset	= .bitnax_decruncher_start - .bitnax_code_start - .decruncher + 2
BITNAX_SIZE_HI		= .bitnax_size_hi - .bitnax_code_start + 2
BITNAX_SIZE_LO 		= .bitnax_size_lo - .bitnax_code_start + 2
BITNAX_DATA_ADDR	= .bitnax_data_addr - .bitnax_code_start + 2
BITNAX_DEST_ADDR 	= .bitnax_dest_addr + .bitnax_decrunch_offset
BITNAX_SECTOR_PTR_1	= .bitnax_sector_ptr_1 + .bitnax_decrunch_offset
BITNAX_SECTOR_PTR_2	= .bitnax_sector_ptr_2 + .bitnax_decrunch_offset
BITNAX_SECTOR_PTR_3	= .bitnax_sector_ptr_3 + .bitnax_decrunch_offset
.bitnax_decruncher_size	= .bitnax_decruncher_end - .bitnax_decruncher_start
.bitnax_packed_size	= .data_end - .data_start
		* = $0801
.bitnax_code_start
		!byte $1c,$08,$00,$00,$9e,$32,$30,$37,$38,$3a,$22,$14,$14,$14,$14
		!byte $14,$14,$14,$14,$14,$42,$49,$54,$4e,$41,$58,$00,$00,$00

		sei
		inc $01

		lda $ba
		pha
		ldx #$00
		;txs

		;inx
-
		lda .bitnax_decruncher_start,x
		sta .decruncher,x
;!if .bitnax_decruncher_end - .bitnax_decruncher_start > $100 {
		lda .bitnax_decruncher_start + (.bitnax_decruncher_size & $ff),x
		sta .decruncher + (.bitnax_decruncher_size & $ff),x
;}
		inx
		bne -
.bitnax_size_hi = * + 1
		ldy #(>(.bitnax_packed_size)) + 1
-
		;src should be data + packed_size
		dex
.bitnax_data_addr = * + 1
src		lda .data_end - $100,x
dst		sta $ff00,x
		txa
		bne -

		dec src + 2
		dec dst + 2
		dey
		bne -
.bitnax_size_lo = * + 1
		ldx #<($10000 - (.bitnax_packed_size))
		jmp go

.bitnax_decruncher_start
!pseudopc .decruncher {
		;fetch depack addr (use --add-depack-addr on lz)
go
		;******** Start the next match/literal run ********
.lz_decrunch
		;XXX TODO fill .lz_bits directly with sfx, no need to place it in stream?
		sec			;This is the main entry point. Forcibly
.lz_type_refill	jsr .lz_refill_bits	;fill up the the bit buffer on entry
.lz_type_check	bcc .lz_do_match
		beq .lz_type_refill

		;******** Process literal run ********

		lda #%00000000		;Decode run length
-
                rol
                asl <.lz_bits
                bne *+5
                jsr .lz_refill_bits
                bcc .lz_lrun_gotten

                asl <.lz_bits
                bne -
                jsr .lz_refill_bits
                bne -

.lz_lrun_gotten
		sta <.lz_copy_cnt + 1	;Store LSB of run-length
                ldy #$00
.lz_lcopy
.lz_sector_ptr2	= * + 1			;Copy the literal data.
.bitnax_sector_ptr_2 = * + 2
		lda+2 .lz_sector,x
		inx
		bne *+5
		jsr .lz_fetch_sector
.lz_dst = * + 1
.bitnax_dest_addr = * + 1
		sta $3800,y
		iny
.lz_copy_cnt	cpy #$00
		bne .lz_lcopy

		;Time to advance the destination pointer.
		;Maximum run length literals exit here as a type-bit needs
		;to be fetched afterwards
		tya
		bne *+5
		jmp .lz_maximum		;maximum literal run, bump sector pointers and so on
		clc
		adc <.lz_dst + 0
		sta <.lz_dst + 0
		bcc .lz_do_match
		inc <.lz_dst + 1

		;******** Process match ********
.lz_do_match
                lda #%00000001
                asl <.lz_bits
                bne *+5
                jsr .lz_refill_bits
                bcs .lz_mrun_start
.lz_mrun_loop
                asl <.lz_bits		;fetch bit 2
                bne *+5
                jsr .lz_refill_bits
                bcc .out		;clear? all ok, else 8 bits are enough, skip last stopbit and exit

                asl <.lz_bits
                bne *+5
                jsr .lz_refill_bits
                rol
                bcc .lz_mrun_loop
                bcs .lz_end_of_file	;A >= 258-byte run (8 bits received) serves as a sentinel
.out
                adc #$01
.lz_mrun_start
		sta <.lz_mcopy_len

		lda #%00100000		;Determine offset length by a two-bit
		rol
.lz_moff_range	asl <.lz_bits		;prefix combined with the first run
		bne *+5			;length bit (where a one identifies
		jsr .lz_refill_bits	;a two-byte match).
		rol			;The rest of the length bits will
		bcc .lz_moff_range	;then follow *after* the offset data

		tay
		lda .lz_moff_length,y
		beq .lz_moff_far

.lz_moff_loop	asl <.lz_bits		;Load partial offset byte
		bne +
		sty <.lz_y
		jsr .lz_refill_bits
.lz_y = * + 1
		ldy #$00
+
		rol
		bcc .lz_moff_loop

		bmi .lz_moff_near

.lz_moff_far	sta <.lz_hi		;Save the bits we just read as the
					;high-byte

.lz_sector_ptr3	= * + 1
.bitnax_sector_ptr_3 = * + 2
		lda+2 .lz_sector,x	;For large offsets we can load the
		inx			;low-byte straight from the stream
		bne *+5			;without going throught the shift
		jsr .lz_fetch_sector	;register

;		sec
		adc .lz_moff_adjust_lo,y ;y .. 2 .. 5? ?! necessary with a full lowbyte?!?!
		bcs .lz_moff_pageok
		dec <.lz_hi
		sec
.lz_moff_pageok	adc <.lz_dst + 0
		sta <.lz_match + 0

.lz_hi= * + 1
		lda #$00
		adc .lz_moff_adjust_hi,y
		sec
		bcs .lz_moff_join	;(BRA)

.lz_moff_near
;		sec			;Special case handling of <8 bit offsets.
	 	adc .lz_moff_adjust_lo,y;We may can safely ignore the MSB from
;		sec			;the base adjustment table as the
		adc <.lz_dst + 0	;maximum base (for a 4/5/6/7 bit
		sta <.lz_match + 0	;length sequence) is 113
		lda #$ff
.lz_moff_join	adc <.lz_dst + 1
		sta <.lz_match + 1

		ldy #$ff		;The copy loop. This needs to be run
					;forwards since RLE-style matches can overlap the destination
.lz_mcopy
		iny
.lz_match = * + 1
		lda $1000,y		;Copy one byte
		sta (.lz_dst),y
.lz_mcopy_len	= * + 1
		cpy #$ff
		bne .lz_mcopy

		tya			;Advance destination pointer
;		sec
		adc <.lz_dst + 0
		sta <.lz_dst + 0
		;Wrap the high-byte of the destination pointer.
		bcc *+4
.lz_maximum	inc <.lz_dst + 1	;This is also used by maximum length
					;literals needing an explicit type bit

		asl <.lz_bits
		jmp .lz_type_check
.lz_bits 	!byte $00

.bitnax_sector_ptr_1 = * + 2
.lz_sector_ptr1	= * + 1
.lz_refill_bits	ldy+2 .lz_sector,x
		sty <.lz_bits
;		sec
		rol <.lz_bits
		inx
		bne +

.lz_fetch_sector
		inc .lz_sector_ptr1 + 1
		inc .lz_sector_ptr2 + 1
		inc .lz_sector_ptr3 + 1
+
		rts

.lz_end_of_file

		dec $01
		cli
		sta $98			;keep away trouble from fastload installers and start with 0 opened files
		pla
		sta $ba
		!byte $4c

.lz_moff_length = * + 2
.lz_moff_adjust_lo = .lz_moff_length + 8
.lz_moff_adjust_hi = .lz_moff_length + 16

}
.bitnax_decruncher_end = * + 26
.data_start
.data_end
