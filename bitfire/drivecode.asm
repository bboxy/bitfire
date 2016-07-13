!src "config.inc"
!convtab pet
!cpu 6510

;XXX TODO have sector loading in ZP instead of all this?
;XXX TODO load of $80 does a loadnext and autoinc own counter that start with 0 after each load_dir -> thus it is reset after change disk \o/
;XXX TODO sector readahead -> after load, loadnext -> wishlist an all populated, load one sector and do not transfer but return. then compare new filenum if not load_next -> on equal, go directly with send:block, else clear wishlist and continue with other file

;--------------- ZP usage ----------------
;.. = unused
;bl = blocks on list
;ms = max sectors on current track
;fs = filesize in blocks (256 byte blocks)
;tr = current track
;in = number of currently loaded block
;pl = preamble bytes lo nibbles
;ph = preamble bytes hi nibbles
;sh = sector header data (gcr encoded -> 5 lo- and hinibbles)
;ds = dirsect
;bs = blocksize, size of transferred block
;fn = filenumber

;00: 0e 06 0c 04 0a 02 08 00 <-+
;08: ff 0e 0f 07 ff 0a 0b 03 <-+
;10: ff ff 0d 05 ff 00 09 01 <-| gcr2ser table with gaps being used by other variables
;18: ff 06 0c 04 ff 02 08 .. <-+ <- same as bin2ser lookup table
;20: .. ff ff xx xx xx xx xx <-+
;28: xx xx xx xx xx xx xx xx <-| list of wanted sectors	;XXX here still gaps can be zero
;30: xx xx xx xx xx xx xx xx <-+
;38: .. ff ff pl pl pl pl pl
;40: pl ff ff .. .. .. .. ..
;48: 0e 08 80 sh sh sh sh sh
;50: 0f 00 00 sh sh sh sh sh
;58: 07 01 10 fi fi fi fi fi
;60: fi ff ff .. .. .. .. ..		;filestruct
;68: 0a 0c c0 bs lb it ba fn
;70: 0b 04 40 .. .. .. .. ..
;78: 03 05 50 ph ph ph ph ph
;80: ph ff ff bl tr ms ds in
;88: fs ff ff .. .. .. .. ..
;90: 0d 02 20 .. .. .. .. ..
;98: 05 03 30 .. .. .. .. ..
;a0: .. ff ff .. .. .. .. ..
;a8: 00 0f f0 .. .. .. .. ..
;b0: 09 06 60 .. .. .. .. ..
;b8: 01 07 70 .. .. .. .. ..
;c0: .. ff ff .. .. .. .. ..
;c8: 06 09 90 .. .. .. .. ..
;d0: 0c 0a a0 .. .. .. .. ..
;d8: 04 0b b0 .. .. .. .. ..
;e0: .. ff ff .. .. .. .. ..
;e8: 02 0d d0 .. .. .. .. ..
;f0: 08 0e e0 .. .. .. .. ..
;f8: 0f 07 0d 05 0b 03 09 01 <-+ bin2ser lookup table
;    ^  ^  ^
;    |  |  |
;    |  |  gcr dec_hi with index << 3
;    |  gcr_dec_lo with index << 3
;    gcr2ser with index << 3
;
;thus it is sufficient to inflate the gcr values in that way that they are either right or left aligned
;-----------------------------------------

.STEPPING_SPEED	= $98

.drivecode	= $0000
.bootstrap	= $0700

.VIA2_LED_OFF	= $f7
.VIA2_LED_ON	= $08

.VIA2_MOTOR_OFF	= $fb
.VIA2_MOTOR_ON	= $04

.blocks_on_list = $83		;blocks tagged on wanted list
.track		= $84		;current track
.max_sectors	= $85		;maximum sectors on current track
.dirsect	= $86
.index		= $87		;current blockindex
.blocks		= $88		;number of blocks the file occupies
.blocksize	= $67

.file_descriptor = $5b
.to_track	= .file_descriptor + 0
.sector		= .file_descriptor + 1
.ld_addr	= .file_descriptor + 2
.file_size	= .file_descriptor + 4

.temp		= .sector
.cmp1		= .sector
.cmp2		= .index
.dest		= $52		;-> 52 is always zero for free $53 will be set, sectorheader is unused during upload

.preamble_lo	= $3b		;preamble data gcr coded lonibbles
.preamble_hi	= $7b		;preamble data gcr coded hinibbles

.dst		= .preamble_lo

.wanted		= $23		;list of wanted sectors/blockindices -> $15 bytes space

.head_lo	= $4b		;sector header gcr data lonibbles
.head_hi	= $53		;sector header gcr data hinibbles

.last_barr	= $6c
.indexts	= $6d
.barrier	= $6e
.filenum 	= $6f

.gcr2ser	= $00		;gcr to serial-port lookuptablea -> from $08-$1f
.bin2ser	= $f8		;binary to serial data

.directory	= $0500		;directory
.lonibbles	= $0600
.hinibbles	= $0700

.gcr_dec_lo	= $f8c0
.gcr_dec_hi	= $f8a0
.gcr_dec_lo_shf	= $01
.gcr_dec_hi_shf	= $02

.DIR_SECT	= 18

BUSY		= $02
BLOCK_READY	= $08
IDLE		= $00

.bootstrap_start
!pseudopc .bootstrap {
.bootstrap_run
		lda #$12
		sta $0a
		lda #.DIR_SECT
		sta $0b

		;lda #$c0
		;sta $02
		;lda $02
		;bmi *-2

		;fetch first dir sect and by that position head at track 18 to have a relyable start point for stepping
		lda #$80
		sta $02
		lda $02
		bmi *-2

;		lda $1c00
;		and #(.VIA2_LED_OFF & .VIA2_MOTOR_OFF)
;		sta $1c00

		sei

		ldx #<.drivecode_-1
		txs
;		lda #$00
;-
;		sta $0100,x
;		dex
;		bpl -

		;lda #$08
		;sta $1800

		lda #%01111010		;DDR set bits for drivenumber to 0, ATN out, CLK out and DATA out are outputs
		sta $1802

		lda #%11101110		;CB2 manual output high, CB1 low, CA2 manual output high, CA1 low (NC)
		sta $1c0c
		lda #%01000001		;PB disable latching, PA enable latching
		sta $1c0b

		lda #$7f		;disable all interrupts
		sta $180e
		sta $1c0e

		sta $180d		;clear all IRQ flags to ack possibly pending IRQs
		sta $1c0d

		cli			;now it is save to allow interrupts again, as they won't happen anymore

		ldy #$00

		ldx #BUSY		;signal that no block is ready yet, asap
		stx $1800

		;wait for atn coming high
		bit $1800
		bpl *-3
.get_block
		lda #$80		;clear all lines and set bit 7 as bit counter, to allow data in and clk in to be set/cleared by host
		sta $1800
		ldx $1800
-
		cpx $1800		;did a bit arrive? (bit flip in data in, atn is dropped in same go in first bit)
		beq *-3
		ldx $1800		;load register
		cpx #$04		;push bit into carry
		ror			;shift in
		bcc -			;do until our counter bit ends up in carry
.block = * + 1
		sta .drivecode,y

		iny
		bne .get_block

		inc .block+1
		dec .blks
		bne .get_block

		;wait for c64 to set $dd02 to $3f
		lda $1800
		bne *-3

		lda #BUSY
		sta $1800
		jmp .drivecode_launch
.blks		!byte (>.drivecode_size)+1
}
.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

.drivecode_start
!pseudopc .drivecode {
		!byte $0e, $06, $0c, $04, $0a, $02, $08, $00 ;<-+
		!byte $ff, $0e, $0f, $07, $ff, $0a, $0b, $03 ;<-+
		!byte $ff, $ff, $0d, $05, $ff, $00, $09, $01 ;<-| gcr2ser table with gaps being used by other variables
		!byte $ff, $06, $0c, $04, $ff, $02, $08, $ff ;<-+
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff

		* = .preamble_lo + 6
		!byte      $ff, $ff, $ff, $ff, $ff, $ff, $ff	;$40
		!byte $0e, $08, $80, $ff, $ff, $ff, $ff, $ff
		!byte $0f, $00, $00, $ff, $ff, $ff, $ff, $ff
		!byte $07, $01, $10, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $0a, $0c, $c0, $ff, $00, $00, $00, $00
		!byte $0b, $04, $40, $ff, $ff, $ff, $ff, $ff
		!byte $03, $05, $50, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $00, $12, $ff, $ff, $ff	;$80
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $0d, $02, $20, $ff, $ff, $ff, $ff, $ff
		!byte $05, $03, $30, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $00, $0f, $f0, $ff, $ff, $ff, $ff, $ff
		!byte $09, $06, $60, $ff, $ff, $ff, $ff, $ff
		!byte $01, $07, $70, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff	;c0
		!byte $06, $09, $90, $ff, $ff, $ff, $ff, $ff
		!byte $0c, $0a, $a0, $ff, $ff, $ff, $ff, $ff
		!byte $04, $0b, $b0, $ff, $ff, $ff, $ff, $ff
		!byte $ff, $ff, $ff, $ff, $ff, $ff, $ff, $ff
		!byte $02, $0d, $d0, $ff, $ff, $ff, $ff, $ff
		!byte $08, $0e, $e0, $ff, $ff, $ff, $ff, $ff
		!byte $0f, $07, $0d, $05, $0b, $03, $09, $01 ;<-+ bin2ser lookup table

;		* = .preamble_lo
		;used only once at startup to be able to kill $0500-$0700 where our installer is lcoated
;.drivecode_launch
		;read dir without any disk side check
		;jsr .read_dir
		;jmp .idle_

		;leave 8 byte of stack, enough

		* = $108
		;-------------------------------------------------------------------------------------------------------------------------------------------------------------
		;NOTE TO SELF: DO NEVER TOUCH THAT SECTOR READ CODE AGAIN, FOR FUCKINGS SAKE! DO NOT TRY TO SAVE BYTES HERE, IT WILL BREAK TESTS WITH THCM'S SLOPPY FLOPPY :-D
		;-------------------------------------------------------------------------------------------------------------------------------------------------------------
.drivecode_
.read_sector	;need to read sector header beforehand to compare with our wanted list, if we would seek a certain sector, the kernal routines would do
		lda #$00
		sta $1801
.read_sector_
		jsr .wait_sync_mark
		bvc *
		lda $1c01		;22333334
		clv
					;11111 222	header type
		cpx #$52
		bne .read_sector_
		;lda $1c01		;22333334	type  / checksum
		asr #%00111111
		sta .head_hi+1		;store @ pos 1						;7
.chksum
		bvc *

		lax $1c01		;44445555	checksum / sector
		ror			;lots of shifting, we could of course also work on left aligned values, but would then need a double lookup when checksumming :-(
		lsr
		lsr
		lsr
		sta .head_lo+1

		clv
		bvc *

		txa
		ldx $1c01		;56666677	sector / track
		cpx #$80
		rol
		and #%00011111
		sta .head_hi+0		;store @ pos 0 instead of pos 1 to reuse value later on

		clv
		bvc *

		txa
		asr #%01111111		;bit 4 of 7 in carry
		sta .head_lo+0		;1 bit of 7 left
		lda $1c01		;77788888	track
		clv
		ror			;-> 77778888
		bvc *

		lsr .head_lo+0		;fetch msb of 7


		;skip loading track information, we have it at hand and let checksum decide if we are on right track, it will fail on wrong track anyway, so we have our track check for free

;		ror			;-> 77777888
;		lsr
;		lsr
;		lsr
;		lda .head_hi+2		;-> ---77777
;		lda #%00011111
;		lax .head_lo+2		;-> ---88888
				;read after 40 cycles
		ldx #$07
		lda $1c01		;11111222	id2
		sax .head_lo+2

		clv
		bvc *

		lsr
		lsr
		lsr
		sta .head_hi+2		;---11111

		lax $1c01		;22333334	id2/id1
		asl
		rol .head_lo+2
		asl
		rol .head_lo+2
		txa
		asr #%00111111
				;read on cycle 40
		sta .head_hi+3

		lda $1c01		;44445555	id2
!if >*-1 != >.chksum { !error "header-checksum not in one page: ", .chksum, " - ", * }
		ror
		lsr
		lsr
		lsr
		sta .head_lo+3

		ldx #$03
-
		ldy .head_hi,x		;decode sector header
		lda .gcr_dec_hi,y
		ldy .head_lo,x
		ora .gcr_dec_lo,y
		sta .head_lo,x
		dex
		bpl -
		tax			;sector number

		eor .head_lo + 1	;build header checksum A = .head_lo + 0
		eor .head_lo + 2
		eor .head_lo + 3
		eor .track		;no need to fetch/check track, checksum will fail on wrong track
.rs_back
		bne .read_sector_	;urgh, bad checksum, find another sector

		;will be transformed into a lda .wanted,x
.skip_wcheck	ldy .wanted,x		;sector on list?
		iny			;$ff + 1 = 0?
		;yes, read, we should still have ~20 cycles overhead here until next sync arrives
		bne *+5
		jmp .read_sector_

.wait_sector_data
;!if BITFIRE_CONFIG_IN_ORDER = 1 {	;no barriers needed with standalone loadraw
;		jmp .check
;.check_back
;}
		jsr .wait_sync_mark
		bvc *
		lda $1c01		;22333334
		clv
		cpx #$55
		bne .rs_back
		bvc *
		asr #%00111111
		ldy #$00		;waste 6 cycles to have more or less same entry point into loop, y = $ff due to sync mark?
		bit $ea

.load_sector_data
		sta .hinibbles,y	;---33333 4
		lax $1c01		;44445555				;read @ 16 / 16 / 16 / 16
		arr #$f0		;44444---				;/!\ ATTENTION, arr potentially fucks the v flag status!
		sta .lonibbles,y	;44444---
		txa			;44445555
		and #%00001111		;----5555
		iny
		sty .lindex+1		;avoid index on lsr

.smc1		bne *			;BRA, y is never 0, could also use bvs, but arr possibly influences V flag /o\
		nop			;so waste either 3,5,7,9 cycles depending on speedzone
		nop
		nop

		ldx $1c01		;56666677				;read @ 40 / 42 / 44 / 46
		cpx #$80		;copy 5 to carry
		rol
		sta .hinibbles,y
		txa			;56666677
		asr #%01111111		;--666667
		sta .lonibbles,y
		iny

.smc2		bne *
		nop
		nop
		nop

		lax $1c01		;77788888				;read @ 67 / 71 / 75 / 79
		and #%00011111
		sta .lonibbles,y	;lda #$0f sax .lonibbles,y if ZP
		txa			;77788888 is fetched again
		ror
.lindex		lsr .lonibbles		;shift out last 7 -> ---66666
		arr #$f0		;77777---
		sta .hinibbles,y
		clv								;now finally clear v flag, as we are past the last arr operation
		lda $1c01		;11111222				;read @ 95 / 99 / 103 / 107
		lsr
				;XXX TODO store $22...222 as ll...hhh
		bvc *

		iny
		sta .hinibbles,y	;-1111122 2
		lda $1c01		;22333334
		clv
		ror			;22233333 4
		sta .lonibbles,y	;store for later processing		;120 cycles min

		bvc *

		and #%00011111		;---33333 4
		iny
		bne .load_sector_data
!if >*-1 != >.load_sector_data { !error "gcr-decoding not in one page: ", .load_sector_data, " - ", * }

		tay
		lda $1c01
		arr #$f0
		tax
		lda .gcr_dec_hi,y	;XXX TODO would also work with gcr2ser table but would reduce reliability
		eor .gcr_dec_lo_shf,x
		tay			;checksum

		lda #$20
		sta $1801

		ldx #$fc

		;finish gcr inflation and checksum the data
		;XXX TODO place code on zp and use smc instead of tay/tya.
		;place lo/hinibbles+3 on zp?
-
		lda .lonibbles + 3,x	;4
		lsr .hinibbles + 3,x	;7
		ror			;2
		lsr .hinibbles + 3,x	;7
		arr #$f0		;2
		sta .lonibbles + 3,x	;5

		tya
		ldy .hinibbles + 0,x
		eor .gcr_dec_hi,y
		ldy .lonibbles + 0,x
		eor .gcr_dec_lo_shf,y
		ldy .hinibbles + 1,x
		eor .gcr_dec_hi,y
		ldy .lonibbles + 1,x
		eor .gcr_dec_lo,y
		ldy .hinibbles + 2,x
		eor .gcr_dec_hi_shf,y
		ldy .lonibbles + 2,x
		eor .gcr_dec_lo,y
		ldy .hinibbles + 3,x
		eor .gcr_dec_hi,y
		ldy .lonibbles + 3,x
		eor .gcr_dec_lo_shf,y
		tay

		txa
		sbx #$04
		bcs -

		tya
		bne ++			;checksum okay?

		ldx .head_lo + 0	;current sector number
		lda .wanted,x		;grab index from list (A with index reused later on after this call)
		dey			;blocksize full sector ($ff)
		sty .wanted,x		;clear entry in wanted list
		cmp .file_size + 1	;as we count up in $100 blocks, if index equals highbyte of size, we have loaded last block
		bne +
		ldy .file_size		;block size of last sector (lowbyte filesize)
+
		sty .blocksize		;remember for later use
		rts			;done
++
		jmp .read_sector


.send_block
		ldy #$00		;start with 0, we will send 2 or 4 bytes of blockinfo as preamble
		ldx .preamble_lo,y
		lda .bin2ser,x
		asl
		ora #$10
		ldx #BLOCK_READY | BUSY
		stx $1800		;signal block ready + busy
		bne +			;will send 6 valid bits

		;on atn going low we have alraedy first data on bus with the upcoming code
.preloop
		lda .bin2ser,x
		bit $1800
		bmi *-3
		sta $1800		;send first bits on end of atn strobe -> 4th sta on c64 side
		asl
		ora #$10
+
		ldx .preamble_hi,y	;hinibbles are already shifted into right position
		bit $1800
		bpl *-3
		sta $1800		;second -> first sta $dd02 on c64 side

		lda .bin2ser,x
		iny
		bit $1800
		bmi *-3
		sta $1800		;third -> second sta $dd02 on c64 side

		asl
		ora #$10
		ldx .preamble_lo,y
		bit $1800
		bpl *-3
		sta $1800		;last -> third sta $dd02 on c64 side
.pre_cmp = * + 1
		cpy #$00
		bcc .preloop
!if >*-1 != >.preloop {
	!error "preloop not in one page! Overlapping bytes: ", * & 255
}

		ldy .blocksize
		ldx .lonibbles,y
.sendloop				;send the data block
					;this loop is much tighter then the preamble, no cycles to be wasted
		lda .gcr2ser,x
		bit $1800
		bmi *-3
		sta $1800		;read by first lda $dd00 on c64 side

		asl
		ora #$10
		ldx .hinibbles,y
		bit $1800
		bpl *-3
		sta $1800		;read by second lda $dd00 on c64 side

		lda .gcr2ser,x
		dey
		bit $1800
		bmi *-3
		sta $1800		;read by third lda $dd00 on c64 side

		asl
		ora #$10
		ldx .lonibbles,y
		bit $1800
		bpl *-3
		sta $1800		;read by fourth lda $dd00 on c64 side
		cpy #$ff
		bcc .sendloop

		;XXX carry is set here, always, might be useful somewhen
		lda #BUSY
		bit $1800
		bmi *-3
		sta $1800

		bpl .reset_pre_cmp 	;BRA reset size for preamble

!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}

;!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
;.check
;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		tya			;y = block index + 1
;		clc			;subtract 1 too much
;		sbc .barrier		;distance
;		cmp #$01		;only allow distance of 0 (first block) or 1
;		bcs +
;		jmp .check_back
;+
;		jmp .read_sector_
;} else {
;		;force 1. sector of file to be loaded first
;;		lda .barrier
;;		bne +
;;		dey			;fetch blockindex
;;		bne ++
;;+
;;		jmp .check_back
;;++
;;		jmp .read_sector_
;}
;}

.preamble
		clc
		adc .ld_addr+1		;add load_addr hibyte to index to make the index = absolute address on c64 side

		ldy .pre_cmp		;first block being sent?
		bne .first_block	;ack byte already in preamble for first block

		pha			;block number is in A, save
!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
		lda .index		;max index to match against
		ldx #$14		;walk through list of sectors to be loaded
-
		cmp .wanted,x		;compare
		bcc +			;bigger index, next please
		lda .wanted,x		;smaller (or same, but can't happen, as index is unique) remember new minimum
+
		dex			;next entry
		bpl -

;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		sta .barrier		;save new barrier
;}
		tax			;load barrier
		sec
		sbc .last_barr		;subtract last position to build delta
		stx .last_barr		;remember current barrier
		asl			;shift, as lower 2 bits will be clobbered
		asl			;a bit silly, as it is shifted >> 4 in .preamble_add_byte :-(
} else {
		tya
}
		jsr .preamble_add_byte	;ack byte/barrier delta
		pla
.first_block
		jsr .preamble_add_byte	;block_addr_hi

		ldx .blocksize		;set up num of bytes to be transferred
		inx			;increase by one, as it is decreased before receiving first byte on c64
		txa

.preamble_add_byte
		ldx #$0f
		sax .preamble_lo,y	;mask out lower 4 bits
		lsr
		lsr
		lsr
		lsr
		sta .preamble_hi,y	;upper 4 bits
.reset_pre_cmp
		iny
		sty .pre_cmp
		rts

.wait_sync_mark
		;XXX TODO maybe first check can be omitted? But this way we are save to not directly fall through next check if we missed the sync. We would read chunk then and possibly still go through the checksum, as it is just a simple 8 bit eor
;		lda $1c00		;wait for start of sync
;		bpl *-3
		lda $1c00		;wait for end of sync
		bmi *-3
		ldy $1c01		;sync mark -> $ff
		clv
		bvc *
		ldx $1c01		;11111222	must be $55/$52
		clv
		rts

!if >*-1 != >.wait_sync_mark { !error "wait_sync_mark not in one page: ", .wait_sync_mark, " - ", * }

.turn_disc
		jsr .read_dir_sect0	;fetch first dir sector
		lda .filenum
		sec
		sbc .directory + $ff	;compare side info
		bne .turn_disc		;nope, not the requested side
		sta .filenum		;reset filenum
		top
.idle
		inc .filenum		;autoinc always, so thet load_next will also load next file after a load with filenum
.idle_
.drivecode_launch
		;XXX TODO here it would be possible to preload next block
		lda $1c00
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 {
		and #(.VIA2_LED_OFF & .VIA2_MOTOR_OFF)
} else {
		and #.VIA2_LED_OFF
}
		sta $1c00
		jsr .get_byte

		;load file, file number is in A
.load_file
		cmp #BITFIRE_UPLOAD
		bne *+5
		jmp .upload
		cmp #BITFIRE_LOAD_NEXT
		beq .load_next
		sta .filenum		;set new filenum
		bcs .turn_disc
.load_next
		lda .filenum		;get current or autoinced filenum
.load
		ldx #.DIR_SECT+1
		sec
-
		tay			;remmeber previous A
		dex
		sbc #42
		bcs -
+
		sty .temp		;store previous value before underrun -> filenum % 42
		cpx .dirsect		;dirsect changed?
		beq +			;nope, advance

		jsr .read_dir_sect	;fetch next dir_sect
+
		lda .temp		;fetch filenum % 42
					;file number * 6 to get proper index -> (num * 2 + num) * 2
		asl			;*2
		;clc			;can be omitted, not more than 42 files are allowed per dirsect, so carry is always cleared after asl
		adc .temp		;+1 -> * 3
		asl			;*2 -> * 6
		tay

		ldx #$00		;reset block index
		stx .index
!if BITFIRE_DECOMP = 1 {		;no barriers needed with standalone loadraw
;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;		stx .barrier
;}
		stx .last_barr
}
-					;copy over direntry to ZP
		lda .directory,y
		sta .file_descriptor,x
		iny
		inx
		cpx #$06
		bne -
					;a = file_size highbyte, carry set
		adc #$00		;need to add one, as also the last partial block counts as a full block. File size is one too less, so exceptions like $xx00 in length are no problem
		sta .blocks
					;send load-address with first transferred block
		ldy #$00
		lda #$ff
		jsr .preamble_add_byte	;status/ack byte
		lda .ld_addr
		jsr .preamble_add_byte
		lda .ld_addr+1
		jsr .preamble_add_byte

		lda #.VIA2_LED_ON | .VIA2_MOTOR_ON
		jsr .motor_on		;turn on led and motor, if not on already
.load_track
		lda .blocks
		;preload 1. block of next file here?
		beq .idle		;check if all blocks of file are loaded

		jsr .seek		;sets max_sectors and bitrate depending on track
--					;now find corresponding offset for first sector by subtracting until remainder (== offset) remains
!if BITFIRE_CONFIG_INTERLEAVE = 4 {
		lax .sector		;load startsector#/offset of current rev and keep a copy in X
		and #$03
		sta .sector
} else {
		lax .sector		;load startsector#/offset of current rev and keep a copy in X
		sec			;calc modulo of .sector and keep it in .sector, XXX on interleave 4 a and #$03 would suffice, but we offer a variable interleave /o\
-
		sta .sector		;keep old value as new offset
		sbc #BITFIRE_CONFIG_INTERLEAVE	;subtract some #sectors
		bcs -			;until underflow
}

		;we have now the right offset to start at
-
		ldy .index		;get index
		sty .wanted,x		;write index into wantedlist
		inc .index		;advance index
		inc .blocks_on_list	;count number of blocks in list (per track num of blocks)
		dec .blocks		;all blocks loaded?
		beq +			;yep, end
		txa			;go for next block
		sbx #-BITFIRE_CONFIG_INTERLEAVE	;x += INTERLEAVE
		cpx .max_sectors	;wrap around?
		bcc -			;nope
		lda #BITFIRE_CONFIG_INTERLEAVE	;handle next round by increasing offset stored in .sector
		isc .sector		;inc sector offset and compare with INTERLEAVE
		bne --			;on zero, all done, else next round
		;XXX TODO here still blocks need to be loaded
+
		;XXX TODO if we branch to the + label, it is a good point to remember the next block here, as it will be the first block of the next file, and we could then preload it easily before going idle again

.load_wanted_blocks			;read and transfer all blocks on wishlist
		jsr .read_sector	;returns with current blockindex in A
		jsr .preamble		;add blockindex and size to preamble, and handle barrier stuff there
		jsr .send_block		;exits with y = 0 and carry set
		dec .blocks_on_list	;last block on wishlist?
		bne .load_wanted_blocks

.track_finished
		sty .sector		;set start pos for next track, y is always 0 after .send_block
-					;now adjust to_track and take care of skipping track 18
		lda #18
		;sec			;set by send_loop and also set if beq
		isc .to_track
		beq -			;skip dirtrack however
		bne .load_track		;BRA

.seek
		lda #$00		;switch to two Mhz on 1571
		sta $1801
.set_bitrate				;sets bitrate for desired track and also sets value for max. sectors on track
		ldy .to_track

		ldx #$11		;max sectors on track
		lda $1c00
		and #$9f
		cpy #31
		bcs ++
		inx
		cpy #25
		bcs +
		inx
		ora #$40
		cpy #18
		bcs ++
		inx
		inx
+
		ora #$20
++
		stx .max_sectors	;store new max sectors
		sta $1c00		;now set bitrate

		asr #$60		;take four bitrates and shift down
		lsr
		lsr
		lsr
		lsr			;0/1/2/3
		sta .smc1+1		;adapt gcr decode to speedzone by modifying branches
		sta .smc2+1

		tya
		sec
		sbc .track		;how many tracks to go?
		sty .track		;save target track as current track

		ldy #$00		;make stepping positive
		bcs .seek_up		;up or downwards?
.seek_down
		eor #$ff		;down we go, so invert num tracks as it is negative
		adc #$01
		iny			;but make stepping negative
.seek_up
		asl			;counter is twice the number of tracks (halftracks)
		tax
		beq +			;nothing to step, end
.step
		lda #.STEPPING_SPEED
		sta $1c05
.halftrack
		tya
		eor $1c00
		sec
		rol
		and #3
		eor $1c00
		sta $1c00
		bit $1c05
		bmi *-3

		dex
		bne .step
+
		rts

.read_dir_sect0
		ldx #.DIR_SECT
.read_dir_sect
		stx .dirsect
		inc .wanted,x		;mark desired dir sector in wishlist

		lda #$12		;set target track to 18
		sta .to_track
		;sta .file_size + 1	;write any number > 0 to file_size + 1 to trigger a sector_length of 256 bytes

		lda #.VIA2_MOTOR_ON
		jsr .motor_on

!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 1 {
		jsr .seek		;move your ass (this is done on motor on in case we turn motor off after load
}
		jsr .read_sector

		ldy #$00
		;decode dir
-
		ldx .hinibbles,y	;read either from shifted or real table
		lda .gcr2ser,x		;load fitting serial entry from table, this can handle shifted gcr values as we have 2 combined tables in one
		tax
		lda .bin2ser,x		;get corresponding binary value
		asl
		asl
		asl
		asl
		pha

		ldx .lonibbles,y
		lda .gcr2ser,x
		tax
		pla
		ora .bin2ser,x
		sta .directory,y
		dey
		bne -

		rts

.motor_on
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 1 {
		jmp $c11b
} else {
		jsr $c11b
		;seek so that .track is set in any case
		jsr .seek
		;disable wanted check, so any sector is okay
		inc .skip_wcheck
		jsr .read_sector
		jsr .read_sector
		;reenable check
		dec .skip_wcheck
		rts
}

.get_byte
		ldy #BUSY		;signal that no block is ready yet and that we are loading, asap
		lda #$80		;IDLE
		sta $1800		;free all lines

		;fast enough to keep track with send_byte, also does bus lock check for free
.gloop
-
		ldx $1800		;fetch bits from bus
		cpx #$05		;allow 00000101 or 00000001 only as valid values, esle bus lock might be active or protocoll violated otherwise
		beq +
		dex
		bne -
		clc
+
		ror			;shift in value of bit 2
-
		clc
		ldx $1800		;fetch bits from bus
		beq +			;allow 00000000 or 00000100 only
		cpx #$04
		bne -
+
		ror
		bcc .gloop		;still bits to fetch?

		sty $1800
		rts

;		ldy #BUSY		;signal that no block is ready yet and that we are loading, asap
;--
;		lda #$80
;		ldx #$00		;IDLE
;		stx $1800		;free all lines
;-
;		cpx $1800		;did a bit arrive? (bit flip in data in, atn is dropped in same go in first bit)
;		beq *-3
;		ldx $1800		;load register
;		bmi --			;check if bus is available (atn must be low) or if bus lock is active for free $dd00 fiddling
;		cpx #$04		;push bit into carry
;		ror			;shift in
;		bcc -			;do until our counter bit ends up in carry
;		sty $1800		;go busy asap
;		;eor #$ff		;invert, as bits arrive inverted
;		rts

		;receive end_address of code being uploaded
		;maximum is $0110 - $04ae, buffers at $0500,$0600,$0700 can be used
		;if original code is restored, be sure that get_byte is at the same address
.upload
		;receive dest
		jsr .get_byte
		sta .dst
		jsr .get_byte
		sta .dst+1

		;num bytes
		jsr .get_byte
		sta .cmp1
		jsr .get_byte
		sta .cmp2
		;XXX todo: copy part from here on onto zp, so full ram can be used for upload
-
		jsr .get_byte
		ldy #$00
		sta (.dst),y		;lowbyte of dest is always 0

		lda #$ff
		dcp .cmp1
		bne +
		dcp .cmp2
		beq .start
+
		dcp .dst
		bne -
		dec .dst+1
		bcs -
.start
		jmp (.dst)

;.blink
;		sec
;		rol
;		sta .temp
;
;--
;		lda $1c00
;		ora #.VIA2_LED_ON
;		sta $1c00
;
;		ldx #$01
;		ldy #$08
;		bcc .short
;		ldx #$08
;		ldy #$01
;.short
;-
;		lda #$ff
;		sta $1c05
;		lda $1c05
;		bne *-3
;
;		dex
;		bne -
;
;		lda $1c00
;		and #.VIA2_LED_OFF
;		sta $1c00
;
;-
;		lda #$ff
;		sta $1c05
;		lda $1c05
;		bne *-3
;
;		dey
;		bne -
;
;		clc
;		rol .temp
;		bne --
;		beq *
;
;		rts

end
		;register $1800 is not $00 or at least $08 here, this sucks, on second turn it is then $00

;emit warnings only once
!if * > $0500 { !serious "Upload code is ", * - $0500, " bytes too big!" }

!ifdef .drivecode_end {
	!warn $0500 - *, " bytes remaining for drivecode."
}

}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start
