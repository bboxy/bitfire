;--------------------------------------------------------------------------------------------------------------------------
; Much <3 goes to Krill for answering all the questions, giving hints, chats, ideas!!!
;--------------------------------------------------------------------------------------------------------------------------

!src "config.inc"
!convtab pet
!cpu 6510

;XXX TODO add shingle-stepping again, upon last sector to be transmitted?

.STEPPING_SPEED		= $98
.eor_val		= $7f
___			= 0

.drivecode		= $0000
.bootstrap		= $0700
.directory		= $0700
.dir_load_addr		= .directory
.dir_file_size		= .directory + 2
.dir_diskside		= .directory + $ff
.tables			= $0600

.header_checksum	= $107
.header_sector		= $106
.header_track		= $105
.header_id1		= $104
.header_id2		= $103

.DIR_SECT		= 18
.DIR_TRACK		= 18

.BUSY			= $02
.BLOCK_READY		= $08
.IDLE			= $00

.VIA2_LED_OFF		= $f7
.VIA2_LED_ON		= $08

.VIA2_MOTOR_OFF		= $fb
.VIA2_MOTOR_ON		= $04

.1541_RESET		= $eaa0

;replacement for jsr: ldx #<back, ldy #<back, jmp read_sector -> stx $00, sty $01 .... load_sector jmp ($0000) to get back
;		ldx #<back
;		ldy #>back
;		jmp read_sector
;back
;
;
;read_sector
;		stx $00
;		sty $01
;
;		;...do stuff
;
;		jmp ($0000)

!macro encap_jsr .addr {
			pla
			clc
			adc #$01
			sta .addr
			pla
			adc #$00
			sta .addr + 1
}

.bootstrap_start
!pseudopc .bootstrap {
.bootstrap_run
			lda #.DIR_TRACK
			sta $0a
			lda #.DIR_SECT
			sta $0b

			;fetch first dir sect and by that position head at track 18 to have a relyable start point for stepping
			lda #$80
			sta $02
			lda $02
			bmi *-2
			;ends up at $0500?

			;motor and LED is on after that

			sei
			lda $05ff
			sta .fn + 1

			;ldx #<.drivecode_-1
			;txs

						;XXX TODO also possible to use data in/clk in for transmission?
			lda #%01111010		;DDR set bits for drivenumber to 0, ATN out, CLK out and DATA out are outputs
			sta $1802

			lda #%11101110		;CB2 manual output high (read), CB1 low, CA2 manual output high (byte ready), CA1 low (NC)
			sta $1c0c

			lda #%00000001		;PB disable latching, PA enable latching (content for $1c01 is then latched)
			sta $1c0b

			lda #$7f		;disable all interrupts
			sta $180e
			sta $1c0e

			sta $180d		;clear all IRQ flags to ack possibly pending IRQs
			sta $1c0d

			;cli			;now it is save to allow interrupts again, as they won't happen anymore

			ldy #.IDLE

			ldx #.BUSY		;signal that we are ready for transfer
			stx $1800

			;wait for atn coming high
			bit $1800
			bpl *-3

			sty $1800		;clear all lines and set bit 7 as bit counter, to allow data in and clk in to be set/cleared by host
			ldx $1800
.get_block
			lda #$80
-
			cpx $1800		;did a bit arrive? (bit flip in data in, atn is dropped in same go in first bit)
			beq -
			ldx $1800		;load register
			bmi .done
			cpx #$04		;push bit into carry
			ror			;shift in
			bcc -			;do until our counter bit ends up in carry
.block = * + 1
			sta .drivecode,y

			iny
			bne .get_block

			inc .block+1
			bne .get_block
.done
			ldx #.BUSY
			stx $1800

			;wait for atn coming low
			bit $1800
			bmi *-3

.fn			lda #$00
			sta <.filenum
			jsr .set_bitrate
			jmp .drivecode_launch




}
.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

.drivecode_start
!pseudopc .drivecode {
.zp_start

;gcr2bin

.file_descriptor	= .zp_start + $00
.load_addr		= .file_descriptor + 0		;word	-> make this overlap with .preamble? saves up storing? but ned to change layout first in d64write? XXX TODO
.file_size		= .file_descriptor + 2		;word
.max_sectors		= .zp_start + $08		;maximum sectors on current track
.dirsect		= .zp_start + $10
.blocks_on_list		= .zp_start + $11		;blocks tagged on wanted list
.curr_sect		= .zp_start + $18
.blocksize		= .zp_start + $19
.first_block		= .zp_start + $20
.filenum 		= .zp_start + $21
.block_num		= .zp_start + $22
.end_of_file		= .zp_start + $23
.dir_enum		= .zp_start + $28
.dir_entry_num		= .zp_start + $29
.blocks 		= .zp_start + $30		;2 bytes
.wanted			= .zp_start + $3e		;21 bytes
.index			= .zp_start + $54		;current blockindex
.track			= .zp_start + $56		;DT ;current track
.to_track		= .zp_start + $58		;DS
.sector			= .zp_start + $59		;DS
.temp			= .zp_start + $5a
.dir_first_file_track	= .zp_start + $60		;2 bytes ;data for second dirsector is first value
.dir_first_file_sector_index= .zp_start + $62		;2 bytes
.dir_first_block_pos	= .zp_start + $68		;2 bytes

.dir_sector		= .zp_start + $64		;1
.is_loaded		= .zp_start + $66		;255 filenumber of shared sector if loaded, else $ff
.is_loaded_size		= .zp_start + $6a		;startpos within sector
.is_loaded_pos		= .zp_start + $6c		;startpos within sector
.return			= .zp_start + $70		;2 bytes
.return_sect		= .zp_start + $78		;2 bytes

.DT			= 18
.DS			= 18
.pa			= 0
.fd			= 0
.FT			= 1
.wt			= $ff

.tab00005555_hi		= * + $00
.tab00333330_hi		= * + $00
.tab05666660_lo		= * + $01
			;     0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f

                        !byte .fd, .fd, .fd, .fd, $f0, $60, $b0, $20, ___, $40, $80, $00, $e0, $c0, $a0, $80
                        !byte ___, ___, $f0, $1e, $70, $1f, $60, $17, ___, ___, $b0, $1a, $30, $1b, $20, $13
                        !byte ___, ___, ___, ___, $50, $1d, $40, $15, ___, ___, $80, $10, $10, $19, $00, $11
                        !byte ___, ___, $e0, $16, $d0, $1c, $c0, $14, ___, ___, $a0, $12, $90, $18, .wt, .wt
                        !byte .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt, .wt
                        !byte .wt, .wt, .wt, $0e, .DS, $0f, .DT, $07, .DT, ___, ___, $0a, ___, $0b, ___, $03
                        !byte .FT, .FT, ___, ___,   1, $0d, 255, $05, ___, ___, ___, $00, ___, $09, ___, $01
                        !byte ___, ___, ___, $06, ___, $0c, ___, $04, ___, ___, ___, $02, ___, $08
.preamble_data
			!byte $00, $00, $00, $00, $00, $00

							;send_data first if is_loaded matches and pos < $ff, then enter loadloop? after that, -pos must e subtracted from filesize
							;don't send barrier after first run but with second?

;01010 01001 01010 01101 01010 11011 01011 10010
;0     8     0     c     0     b     1     2


;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          1111111111111111111111111111111122222222222222222222222222222222333333333333333333333333333333334444444444444444444444444444444455555555555555555555555555555555
;              1                       ............   2                                      3                                   4             v      5           |-
;1          111111111111111111111111111111222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555
;              1                       ............   2                              3                                   4             v      5           |-
;2          11111111111111111111111111112222222222222222222222222222333333333333333333333333333344444444444444444444444444445555555555555555555555555555
;              1                       ............   2                      3                                   4             v      5           |-
;3          1111111111111111111111111122222222222222222222222222333333333333333333333333334444444444444444444444444455555555555555555555555555
;              1                      .............   2              3                                   4             v      5           |-
; in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the ende and then there's up to 5 cycles delay (branch + fallthrough

			;XXX see if we can use bit 2 from original data, would save space in tables
			;port new tool and make it work with this version (no sbx first of all, remove sbx and use direct access)
			;attention, two sparse bits with 77700077!!!
.read_loop
			;XXX TODO need to save cycles in this block?!?!?!?!?!
			lda $1c01			;22333334
			sax <.threes + 1
			asr #$c1			;lookup? -> 4 cycles
			tax
			lda .tab11111000_hi,y
.twos			eor .tab02200222_lo,x
			tsx
			pha
			beq .gcr_end			;127
;29

.chksum			eor #$00
			eor $0101,x			;can be omitted then XXX TODO can be moved to .fives to eor things there, eor after each lda/adc? then it would work?
			eor $0102,x
			sta <.chksum2 + 1
;13
.gcr_entry						;XXX TODO would be nice if loaded earlier
							;XXX TODO ldx $1c01, 3 cycles earlier, but costs 2 cycles on top later onearlier
			lda $1c01			;44445555	second read
			ldx #$0f			;ldx + sax can be moved after lda + eor?
			sax <.fives + 1
			arr #$f0
			;sta <.fours + 1 to save tay and keep y free? if all tays are saved +0 3 cycles for 3 sta .num + 1
			tay				;44444---		;how's about having 4444---4?
;13
			ldx #$03			;save another 2 cycles and use $0f
.gcr_slow1		lda $1c01			;56666677		third read	;slow down by 6,12,18
			sax <.sevens + 1		;------77		;encode first 7 with sixes and by that shrink 6table? and add it with sevens?
			asr #$fc			;-566666-		;XXX TODO value $fc here, and later on sbx, could be reused? coudl also use -56666-- table
			tax

.threes			lda <.tab00333330_hi		;ZP! XXX TODO This block can also moved above last lda $1c01? but read 3 skews then
			adc .tab44444000_lo,y
			pha
.chksum2		eor #$00
			sta .chksum + 1

.fives			lda <.tab00005555_hi		;ZP!
			adc <.tab05666660_lo,x
			pha
;38
			;bvs here?
			lax $1c01			;77788888	forth read	;slow down by 2, 4, 6
			and #$e0			;ora #$00011111 would also work, and create an offset of $1f? Unfortunatedly the tab then wraps :-(
			tay
							;XXX TODO bit 2 can be used in original form
			lda .tab00088888_lo,x
			ldx #$07			;if we could reuse $07 here too? not as much waste :-(
.sevens			adc .tab77700077_hi,y
			pha
;21
			lda $1c01			;11111222	fifth read
			sax <.twos + 1
			and #$f8			;could shift with asr and compress twos tabe? fits in ZP?
			tay
			ldx #$3e			;XXX TODO $3f? and have only 4x4 afterwards? no asr needed?
;13
-
			bvs .read_loop			;2 cycle jitter only
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			jmp -

			;save here to make next read happen earlier :-( also reuse 7 somehow :-(
;29
.gcr_end
			;Z-Flag = 1 on success, 0 on failure (wrong type)
			jmp .read_sector_back
			jmp .read_header_back

			;XXX reuse X reg and bloat tab77 -> yet too expensive
			;XXX spend a sertab for highbytes

!ifdef .second_pass {
	!warn $0100 - *, " bytes remaining in zeropage."
}

!if >*-1 != >.read_loop { !error "read_sector not in one page: ", .read_loop, " - ", * }

			* = $200
.tab02200222_lo
.gcr_00
			lda ($00,x)
			nop
			nop
.gcr_20
			lda ($00,x)
			nop
.gcr_40
			nop
			lda $1c01
			jmp .gcr_slow1 + 3
.ser2bin
			!byte $00 xor .eor_val
			!byte $08 xor .eor_val
			!byte $02 xor .eor_val
			!byte $0a xor .eor_val
			!byte $04 xor .eor_val
			!byte $0c xor .eor_val
			!byte $06 xor .eor_val
			!byte $0e xor .eor_val
			!byte $01 xor .eor_val
			!byte $09 xor .eor_val
			!byte $03 xor .eor_val
			!byte $0b xor .eor_val
			!byte $05 xor .eor_val
			!byte $0d xor .eor_val
			!byte $07 xor .eor_val
			!byte $0f xor .eor_val

			* = .tab02200222_lo + $22

                        !byte           $0e, $0a, ___, $00, $06, $02
.calc_max_sectors_and_bitrate
			ldx #$11		;max sectors on track
			cpy #31
			bcs ++			;-> bitrate = $00
			inx
			cpy #25
			bcs +			;-> bitrate = $20
			inx
			ora #$40
			cpy #18
			bcs ++			;-> bitrate = $40
			inx			;-> bitrate = $60
			inx
+
			ora #$20
++
			stx <.max_sectors	;store new max sectors
			rts
			;$19 bytes

			* = .tab02200222_lo + $42
                        !byte           $0f, $0b, $0d, $09, $0c, $08

.read_dir_sect_
			jsr .read_sector

			;copy over dir
			ldy #$00
-
			pla
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x
			dey
			sta .directory,y
			bne -
			jmp (.return)

			* = .tab02200222_lo + $62
                        !byte           $07, $03, $05, $01, $04

			;$0d bytes
.send_sector_data
			;switch between preamble and data: nervig -> preamble hat zeit -> jsr calls (meh, nicht möglich, aber calls für einzelbytes?) less code modification, preamble is pain in the ass
			lda #.sendloop - .branch - 2
			sta .branch + 1
			ldy #$ff
			sty .pre_len + 1
			ldy <.blocksize
.sendloop				;send the data block
.datasrc		lda $0100,y
.preamble_entry
			bit $1800
			bmi *-3
			sax $1800
			asl
			ora #$10
			bit $1800
			bpl *-3
			sta $1800
			ror
			arr #$ff - 4
			bit $1800
			bmi *-3
			sta $1800
			ror
			arr #$ff - 4
.pre_len		cpy #$ff
			iny
			bit $1800
			bpl *-3
			sta $1800
.branch			bcc .sendloop
.sendloop_end
			;XXX carry is cleared here, always, might be useful somewhen
			tya
			bne .send_sector_data
			lda #.BUSY
			bit $1800
			bmi *-3
			sta $1800
!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}

;.shared_sect		lda #$00
;			beq +
;			dec .shared_sect + 1
;			lda .file_size
;			sec
;			sbc .is_loaded_size
;			sta .file_size + 1
;			bcs +
;			dec .file_size + 1
			;XXX TODO specialcase: filesize is zero now, nothing else to load? then save actual pos as new pos and filenum++
+
			;check_here if .filenum = .is_loaded and if pos < $ff	-> if so, clear now and correct and jump back to other location
			;same also when alling send_data, see that file_size <= remaining data in sector

			jmp .send_data_back

.send_data
.preamble
			lax <.first_block
			beq +

			inc <.first_block

			ldx <.load_addr
			stx <.preamble_data + 1

			ldx <.load_addr + 1
			stx <.preamble_data + 2

			ldx #$02
+
			and #$fc		;mask out preceeding two bits to signal BLOCK_READY and !BUSY with first transferred byte
			sta <.preamble_data
			inx

			;XXX TODO needs to be calculated differently? distance from start in 16 bit and load_adddress added? -> highbyte is barrier?
!if BITFIRE_DECOMP = 1 {			;no barriers needed with standalone loadraw
			lda <.index		;max index to match against
			ldy #$14		;walk through list of sectors to be loaded
-
			cmp <.wanted,y		;compare
			bcc +			;bigger index, next please
			lda <.wanted,y		;smaller (or same, but can't happen, as index is unique) remember new minimum
+
			dey			;next entry
			bpl -

;!if BITFIRE_CONFIG_IN_ORDER = 1 {
;			sta .barrier		;save new barrier
;}
			clc			;needed as branch above does not always hit in
			adc <.load_addr + 1
			sta <.preamble_data,x
			inx
}
			lda <.block_num
			clc			;can't be omitted, can be set
			adc <.load_addr + 1	;add load_addr hibyte to block number to make the block number an absolute address on c64 side
			sta <.preamble_data,x
			inx
						;XXX TODO need to send lowbyte of current block too
						;can load-adress be left out? Check on current block adress if <= last, assume new load-adress? needed after loading for depacking? reset to ffff upon load?

			lda #$00		;carry is cleared
			sec
			sbc <.blocksize		;set up num of bytes to be transferred
			sta <.preamble_data,x

			stx .pre_len + 1	;set preamble size
-
			lda <.preamble_data,x	;smaller this way, as lda $zp,x can be used now
			and #$0f
			tay
			lda <.preamble_data,x
			and #$f0
			eor .ser2bin,y
			sta <.preamble_data,x
			dex
			bpl -
						;block_available will be signalled at first sent byte
			ldx #$0f
.start_send
			lda #.preloop - .branch - 2
			sta .branch + 1
			ldy #$00
.preloop
			lda <.preamble_data,y
			;adds 3 cycles to send, but preamble is bytewise fetched via jsr calls on resident side, so no timing issues
			jmp .preamble_entry

.drivecode_launch
.turn_disc
			;XXX TODO use load_dir_sector stuff from load_file and make use of it here too
			ldx #.DIR_SECT
			jsr .read_dir_sect	;fetch first dir sector
			lda <.filenum
			eor .dir_diskside	;compare side info
			bne .turn_disc		;nope, not the requested side
			sta <.filenum		;reset filenum
			lda #$3f * 4
			sta <.dir_entry_num
			ldx #$01		;calculate offset for first dir_sector
			stx <.dir_sector
			jsr .find_file_in_dir
						;store values for sector 2
			lda <.to_track
			sta <.dir_first_file_track
			lda <.blocks
			sta <.dir_first_file_sector_index
			lda <.blocks + 1
			sta <.dir_first_block_pos
			top
.idle
			inc <.filenum		;autoinc always, so thet load_next will also load next file after a load with filenum
			dec <.first_block
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
			;cmp #BITFIRE_RESET
			;bne *+5
			;jmp .1541_RESET
			cmp #BITFIRE_LOAD_NEXT
			beq +
			sta <.filenum		;set new filenum
+
			cmp #BITFIRE_REQ_DISC
			;XXX TODO send out preloaded sector here if filenum = expected filenum
			lda #.VIA2_MOTOR_ON
			bcs +
			;no LED during turn disc
			ora #.VIA2_LED_ON
+
			ora $1c00
			sta $1c00
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 {
			inc .skip_wcheck	;disable wanted check ($4c of jmp is transformed into $4d = eor $xxxx), so any sector is okay
			jsr .read_sector
			jsr .read_sector
			jsr .read_sector
			dec .skip_wcheck	;reenable check
}
			lax <.filenum		;load filenum
			cmp #BITFIRE_REQ_DISC
			bcs .drivecode_launch	;turn disc

			ldy #$00		;dir sector 2
			sbx #$3f		;subtract 63 entries
			bcs +			;filenum % 63, branch
			iny			;dir sector 1
			tax			;restore filenum
+
			txa			;shift by 4 to get index into dir
			asl
			asl
			sta <.dir_entry_num	;and save

			cpy <.dir_sector	;dir sector is loaded?
			sty <.dir_sector	;save new dir sector
			beq +			;yes, branch
			tya
			clc
			adc #(.DIR_SECT - 1)	;-> sector 18 (#1) or 17 (#2)
			tax			;requested sector in x
			jsr .read_dir_sect	;load dir sector
+
			ldx <.dir_sector
			jsr .find_file_in_dir
			stx <.index		;reset block index, x = 0 after jsr
						;a = file_size highbyte, carry set
.load_track
			jsr .seek		;sets max_sectors and bitrate depending on track

			lda <.sector		;now crate our wishlist for the current track
.wanted_loop
			tax
			ldy <.index		;get index
			sty <.wanted,x		;write index into wantedlist
			inc <.blocks_on_list	;count number of blocks in list (per track num of blocks)
			cpy <.file_size + 1	;inc index and check if index > file_size + 1 -> EOF
			inc <.index
			bcs .load_wanted_blocks	;yep, EOF, carry is set
			adc #BITFIRE_CONFIG_INTERLEAVE
			cmp <.max_sectors	;wrap around?
			bcc .wanted_loop	;nope

!if BITFIRE_CONFIG_INTERLEAVE = 4 {
			adc #$00		;increase
			and #$03		;modulo 4
} else {
			adc #$00		;increase
-
			sec			;modulo INTERLEAVE
			sbc #BITFIRE_CONFIG_INTERLEAVE	;subtract some #sectors
			bcs -			;until underflow
			adc #BITFIRE_CONFIG_INTERLEAVE
			clc
}
			bne .wanted_loop	;if zero, done
						;carry is 0
						;just track is full
			sta .sector		;start next track with sector = 0
.load_wanted_blocks				;read and transfer all blocks on wishlist
			ror <.end_of_file
-
			jsr .read_sector	;returns with current blockindex in A
			jmp .send_data		;add blockindex and size to preamble, and handle barrier stuff there
.send_data_back
			dec <.blocks_on_list	;last block on wishlist?
			bne -

.track_finished
-					;now adjust to_track and take care of skipping track 18
			lda #18
			sec			;set by send_block and also set if beq
			isc <.to_track
			beq -			;skip dirtrack however

			lda <.end_of_file	;EOF
			bpl .load_track
						;XXX TODO preload 1. block of next file here?
			jmp .idle

.seek
						;sets bitrate for desired track and also sets value for max. sectors on track
			lax <.to_track
			sec
			sbc <.track		;how many tracks to go?
			stx <.track		;save target track as current track
			beq +			;nothing to step, end

			ldy #$00		;make stepping positive
			bcs .seek_up		;up or downwards?
.seek_down
			eor #$ff		;down we go, so invert num tracks as it is negative
			adc #$01
			iny			;but make stepping negative
.seek_up
			asl			;counter is twice the number of tracks (halftracks)
			tax
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
.set_bitrate
			ldy <.track
			lda $1c00
			and #$9f
			jsr .calc_max_sectors_and_bitrate
			sta $1c00		;now set bitrate

			ldy #$4c		;common values toset up for 1, 2, 3
			ldx #>.gcr_40		;should be $02

			and #$60
			beq .bitrate_3		;a bit pity that this needs another bunch of branches :-( TODO
			cmp #$40
			beq .bitrate_1
			bcc .bitrate_2
.bitrate_0
			ldy #$ad
			lda #$01
			ldx #$1c
			top
.bitrate_1
			lda #<.gcr_40
			top
.bitrate_2
			lda #<.gcr_20
			top
.bitrate_3
			lda #<.gcr_00

			sty .gcr_slow1 + 0
			sta .gcr_slow1 + 1
			stx .gcr_slow1 + 2
+
			rts

.read_dir_sect
			;save two bytes on setup by using ZP
			+encap_jsr <(.return)

			stx <.dirsect
			inc <.wanted,x		;mark desired dir sector in wishlist

			lda #.DIR_TRACK		;set target track to 18
			sta <.to_track

			jsr .seek
			jmp .read_dir_sect_
!ifdef .second_pass {
	!warn "size of read_dir_sect: ", * - .read_dir_sect
}


.lock
-
			ldx $1800		;still locked?
			bmi -
.get_byte
			ldy #.BUSY		;enough time for signal to settle
.get_byte_
			lda #.IDLE | $80	;execpt a whole new byte
			sta $1800

			ldx #$00		;start with a free bus
.gloop
-
			cpx $1800		;wait for transition
			beq -
			ldx $1800		;reread register
			bmi .lock		;is the bus locked via ATN? if so, wait
			cpx #$05		;nope, interpret bit
			ror			;and shift in
-
			cpx $1800		;wait for next transition
			beq -
			ldx $1800		;reread register
			bmi .lock		;is the bus locked? XXX TODO can be omitted?
			cpx #$01		;nope, interpret bit
			ror			;and shift in

			bcc .gloop		;more bits to fetch?
			sty $1800		;set busy bit
			rts

.read_sector	;need to read sector header beforehand to compare with our wanted list, if we would seek a certain sector, the kernal routines would do
			;read_header and do checksum, if not okay, do again

			;pull return address from stack and jmp to it later to keep stack clean
			+encap_jsr <(.return_sect)

.read_sector_
			ldx #$07			;bytes to fetch
			ldy #$52			;type (header)
			bne .read_gcr
.read_header_back
			lda .header_id2
			eor <.chksum2 + 1
			bne .read_sector_

			lda .header_sector
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x
			tax
			stx <.curr_sect

			ldy <.wanted,x			;sector on list?
			iny
			bne +
.skip_wcheck		jmp .read_sector_
+
			ldx #$ff			;bytes to fetch
			ldy #$55			;type (sector)
			bne .read_gcr
.read_sector_back
			eor <.chksum + 1		;XXX TODO annoying that last bytes nned to be checksummed here :-(
			eor $0101
			eor $0102
							;cheksum
			ldx $1c01			;44445555
			sta .eor + 1
			txa
			arr #$f0
			tay				;44444---
			ldx <.threes + 1
							;chksum
			lda $00,x
			ora .tab44444000_lo,y
.eor			eor #$00

			tay

			bne .read_sector_		;checksum okay?

			;remove sector from wanted list and determine blocksize in Y (partly or full size)
			ldx <.curr_sect			;current sector number
			lda <.wanted,x			;grab index from list (A with index reused later on after this call)
			dey				;blocksize full sector ($ff)
			sty <.wanted,x			;clear entry in wanted list
			cmp <.file_size + 1		;current block == last block? So we transfer no full block anymore XXX TODO, if current block == 0 -> first block, also transfer partly
			bne +
			ldy <.file_size			;block size of last sector (lowbyte filesize)
+
			sta <.block_num
			tya
			eor #$ff
			sta <.blocksize			;remember for later use
.read_sector_return
			jmp (.return_sect)		;done
.read_gcr
			txs
.reread
			ldx #$00
			top
.still_sync
			ldx #$01
.wait_sync
			bit $1c00			;wait for end of sync
			bmi .still_sync
			txa
			beq .wait_sync			;no loop run taken, so fell through check
			lda $1c01			;sync mark -> $ff
			clv
			lda #.eor_val
			sta <.chksum2 + 1
			bvc *
			cpy #$55			;clc/sec depending on val
			arr #$00			;shift in carry, XXX TODO does it delete v, gotta check?!
			clv
			lsr				;$00/$40
			eor #$0c			;$0c/$4c top/jmp
			sta <.gcr_end
			cpy $1c01			;11111222
			bne .reread
			ldx #$3e
			bvc *
			lda $1c01			;22333334
			sax <.threes + 1
			asr #$c1			;lookup? -> 4 cycles
			jsr .wait
			jsr .wait
			bit $ea
			jmp .gcr_entry

.find_file_in_dir

			lda <.dir_first_file_sector_index,x	;sectorindex of first file in dir (not sector number, but as if written with interleave = 1)
			sta <.blocks + 1
			lda <.dir_first_block_pos,x		;pos in first sector where file starts
			sta <.blocks
			ldy <.dir_first_file_track,x		;track of first file in dir
			jsr .calc_max_sectors_and_bitrate
			ldx #$00
			stx <.dir_enum
.next_dir_entry
			cpx <.dir_entry_num
			beq .found_file
			lda <.blocks
			sec
			adc .dir_file_size,x
			sta <.blocks
			lda <.blocks + 1
			adc .dir_file_size + 1,x
			sta <.blocks + 1

			lda #$00
			sta <.blocks
			inc <.blocks + 1
			;XXX TODO blocks, eor #$ff clc adc #$01 -> size of first block of file
			;written in blocks and blocks + 1
			;yet skip rest of last block
			;lda <.blocks
			;eor #$ff
			;sec
			;adc <.blocks
			;sta <.blocks
			;lda <.blocks + 1
			;adc #$00
			;sta <.blocks + 1
.next_track
			jsr .calc_max_sectors_and_bitrate
			lda <.blocks + 1
			sec
			sbc <.max_sectors
			bcc .no_next_track
			sta <.blocks + 1
-
			;skip dir_track
			iny
			cpy #.DIR_TRACK
			beq -
			bne .next_track
.no_next_track
			lax <.dir_enum
			sbx #-4
			stx <.dir_enum
			bne .next_dir_entry
.wait
			rts
.found_file
			sty <.to_track

			ldy #$00
-
			lda .directory,x
			sta <.file_descriptor,y
			inx
			iny
			cpy #$04
			bne -

			;remaining blocks on track
			lax <.blocks + 1
			beq .found_sector

			lda #$00
.fs_loop
			clc
			adc #BITFIRE_CONFIG_INTERLEAVE
			cmp <.max_sectors
			bcc +
			;next revolutiona XXX interleave 4 that is, other interleaves to be done
!if BITFIRE_CONFIG_INTERLEAVE = 4 {
			adc #$00		;increase
			and #$03		;modulo 4
} else {
			adc #$00		;increase
-
			sec			;modulo INTERLEAVE
			sbc #BITFIRE_CONFIG_INTERLEAVE	;subtract some #sectors
			bcs -			;until underflow
			adc #BITFIRE_CONFIG_INTERLEAVE
}
+
			dex
			bne .fs_loop
.found_sector
			sta <.sector
			rts


.debug_decode_sector
			;for debug, decode sector and turn it upside down for easy hexdiff
;			ldy #$00
;-
;			lda $0100,y
;			ldx #$0f
;			sbx #$00
;			and #$f0
;			eor .ser2bin,x
;			sta $0100,y
;			dey
;			bne -
;
;			ldx #$ff
;			ldy #$00
;-
;			lda $0100,x
;			sta $0700,y
;			dex
;			iny
;			bne -

;XXX TODO count through first sector when initally loaded and remember values for second dirsector (file 64-127)
;also place side infotag elsewhere, so that 64 entry per page are possible
;dirsector: am ende 2 byte für startpos auf disk von erstem file: dann alle filesizes aufrechnen um aktuellen verschnitt zu haben


;dirsector 0:
;first-fileblock-start: $00
;second file: add lowbyte of filesize to get first blocksize
;dirsector 1:
;first-fileblock-start ->nn m
;also on dir track: t/s of first file in dir -> add up files to find starting t/s of file


;4 bytes fileinfo:
;load_addr
;file_size

;63 file je dirsect
;3 bytes free
;1 byte offset in shared sector
;2 bytes t/s of first file on dirsect
;1 byte diskside info


;need to add blocksize to lowbyte of load_adress, can happen always, as add 0 does not hurt at all
;also use remaining blocks on dirtrack? but how to determin?


;add fakeentrys to uploaded code and copy to 0700 for now

;emit warnings only once

;tables with possible offsets
.tab11111000_hi		= .tables + $00
.tab44444000_lo 	= .tables + $04
.tab00088888_lo		= .tables + $00

;table wth no offset
.tab77700077_hi		= .tables + $00

!ifdef .second_pass {
	!if * > .table_start { !serious "Upload code is ", * - .table_start, " bytes too big!" }
}

!ifdef .second_pass {
	!warn .table_start - *, " bytes remaining for drivecode."
}
	 		* = .tables + $21
.table_start
                        !byte      $f0, ___, $e0, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03
                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
                        !byte ___, $70, $50, $d0, ___, ___, ___, ___, $f0, $0e, $0f, $07, $0e, $0a, $0b, $03
                        !byte $70, ___, $0d, $05, $0f, $00, $09, $01, $60, $06, $0c, $04, $07, $02, $08, ___
                        !byte ___, $60, $40, $c0, ___, ___, ___, ___, $b0, $0e, $0f, $07, $0a, $0a, $0b, $03
                        !byte $30, ___, $0d, $05, $0b, $00, $09, $01, $20, $06, $0c, $04, $03, $02, $08, ___
                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___			;XXX TODO place ser2bin here and use free space for dir_sect_load?
                        !byte $50, ___, ___, ___, $0d, ___, ___, ___, $40, ___, ___, ___, $05, ___, ___, ___
                        !byte ___, $b0, $80, $a0, ___, ___, ___, ___, $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, ___
                        !byte ___, $30, $10, $90, ___, ___, ___, ___, $e0, $0e, $0f, $07, $06, $0a, $0b, $03
                        !byte $d0, ___, $0d, $05, $0c, $00, $09, $01, $c0, $06, $0c, $04, $04, $02, $08, ___
                        !byte ___, $20, $00, ___, ___, ___, ___, ___, $a0, $0e, $0f, $07, $02, $0a, $0b, $03
                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start

.second_pass



;	0213
; 3210	0213
;bin2ser
;%0000	0000	$00
;%0001	1000	$08
;%0010	0010	$02
;%0011	1010	$0a
;%0100	0100	$04
;%0101	1100	$0c
;%0110	0110	$06
;%0111	1110	$0e
;%1000	0001	$01
;%1001	1001	$09
;%1010	0011	$03
;%1011	1011	$0b
;%1100	0101	$05
;%1101	1101	$0d
;%1110	0111	$07
;%1111	1111	$0f
;ser2bin
; 3210	0213
;%0000	0000	$00	$00
;%1000	0001	$01	$08
;%0010	0010	$02	$02
;%1010	0011	$03	$0a
;%0100	0100	$04	$04
;%1100	0101	$05	$0c
;%0110	0110	$06	$06
;%1110	0111	$07	$0e
;%0001	1000	$08	$01
;%1001	1001	$09	$09
;%0011	1010	$0a	$03
;%1011	1011	$0b	$0b
;%0101	1100	$0c	$05
;%1101	1101	$0d	$0d
;%0111	1110	$0e	$07
;%1111	1111	$0f	$0f

;think over that 222 22 combination possibilities again
;store tables in serial annotation already
;does this work well with the 566666 table?

;XXX TODO: other tables add a scrambling function taht scrambles a resulting nibble also eor values apply, depending on tranferred bits
;!byte $0f, $07, $0d, $05, $0b, $03, $09, $01, $0e, $06, $0c, $04, $0a, $02, $08, $00
;   place value by bits ---| scrambled or not   atsome points one is added, at some not.  overall adc1 happens aswell, but will change upon scrambling
;3210	1234 5		   v						ser
;0000 	0101 0		-> 0		0	6	adc 1		0f	07	adc 80		0f
;0001 	0101 1		-> 0	adc 1	1	6			07	07			07
;0010 	1001 0		-> 2		2	4	adc 1		0d	05	adc 80		0d
;0011 	1001 1		-> 2	adc 1	3	4			05	05			05
;0100 	0111 0		-> 4		4	2	adc 1		0b	03	adc 80		0b
;0101 	0111 1		-> 4	adc 1	5	2			03	03			03
;0110 	1011 0		-> 6		6	0	adc 1		09	01	adc 80		09
;0111 	1011 1		-> 6	adc 1	7	0			01	01			01
;1000 	0100 1		-> 7	adc 1	8	f			0e	0e			0e
;1001 	1100 1		-> 8	adc 1	9	e			06	06			06
;1010 	1101 0		-> a		a	c	adc 1		0c	04	adc 80		0c
;1011 	1101 1		-> a	adc 1	b	c			04	04			04
;1100 	0110 1		-> b	adc 1	c	b			0a	0a			0a
;1101 	1110 1		-> c	adc 1	d	a			02	02			02
;1110 	1111 0		-> e		e	8	adc 1		08	00	adc 80		08
;1111 	1010 1		-> e	adc 1	f	8			00	00			00


;1000 	0100 1		-> 7	adc 1	8	f			0e	0e			0e
;0001 	0101 1		-> 0	adc 1	1	6			07	07			07
;1100 	0110 1		-> b	adc 1	c	b			0a	0a			0a
;0101 	0111 1		-> 4	adc 1	5	2			03	03			03
;0011 	1001 1		-> 2	adc 1	3	4			05	05			05
;1111 	1010 1		-> e	adc 1	f	8			00	00			00
;0111 	1011 1		-> 6	adc 1	7	0			01	01			01
;1001 	1100 1		-> 8	adc 1	9	e			06	06			06
;1011 	1101 1		-> a	adc 1	b	c			04	04			04
;1101 	1110 1		-> c	adc 1	d	a			02	02			02
;1110 	1111 0		-> e		e	8	adc 1		08	00	adc 80		08



;5 = 0 -> adc $80

;!byte ___, ___, ___, ___, $0f, $06, $0b, $02, ___, $04, $08, $00, $0e, $0c, $0a, $08
;!byte ___, ___, ___, ___, $70, $00, $b0, $40, ___, $20, $e0, $60, $80, $a0, $c0, $e0
;gcr2ser
;!byte ___, ___, ___, ___, $60, $70, $20, $30, ___, $50, $00, $10, $e0, $40, $a0, $00

;!byte ___, ___, ___, ___, $e0, $70, $a0, $30, ___, $50, $00, $10, $60, $40, $20, $00

;;tab5
;	!byte $00	;0	unused
;	!byte $00	;1	unused
;	!byte $00	;2	unused
;	!byte $00	;3	unused
;	!byte $09	;4		$06
;	!byte $00	;5		$07
;	!byte $0d	;6		$02
;	!byte $04	;7		$03
;	!byte $00	;8	unused
;	!byte $02	;9		$05
;	!byte $0e	;a		$00
;	!byte $06	;b		$01
;	!byte $08	;c		$0e
;	!byte $0a	;d		$04
;	!byte $0c	;e		$0a
;	!byte $0e	;f		$00


;	1234 5					ser
;1000 	0100 1		-> 7	adc 1		0e	0e			0e
;0001 	0101 1		-> 0	adc 1		07	07			07
;1100 	0110 1		-> b	adc 1		0a	0a			0a
;0101 	0111 1		-> 4	adc 1		03	03			03
;0011 	1001 1		-> 2	adc 1		05	05			05
;1111 	1010 1		-> e	adc 1		00	00			00
;0111 	1011 1		-> 6	adc 1		01	01			01
;1001 	1100 1		-> 8	adc 1		06	06			06
;1011 	1101 1		-> a	adc 1		04	04			04
;1101 	1110 1		-> c	adc 1		02	02			02




;XXX TODO scramlbe via table could help in adding the bitflips beforehands on off/even bits, depending on




;schnellerer xfer
;jmp ($1800) möglich?

;	lda #$60/sta
;	sta .store
;	 ldy #$ff? (set once to avoid the branch and compare to hit in? + rts? how to fetch single bytes?
;        ldx #$3f
;	 bne .entry
;.loop
;        lsr
;        lsr
;        ora $dd00 - $3f,x
;        stx $dd02
;        lsr
;        lsr
;        cpy #$00	;sec
;	 beq .end
;        ldx #$37
;        ora $dd00
;        stx $dd02
;        ror
;        ror
;        ldx #$3f
;        sax .nibble
;        and $dd00
;        stx $dd02
;.nibble  ora #$00
;.store   sta $beef,y
;.entry
;        lda #$37
;        ora $dd00 - $3f,x
;        sax $dd02
;        dey
;        jmp .loop
;.end



;shift this in to avoid lookup? -> full lookup
;XXX TODO mapping of last bits can also be directly added without lookup?! \o/
;3210	1 2345		   v						ser

;OKAY
;XXX TODO mapping of last bits can also be directly added without lookup?! \o/
;3210	12 345		   v						ser
;1000 	01 001		00	+	00		results in	0
;0000 	01 010		00	+	01				1
;0001 	01 011		00	+	02				2
;1100 	01 101		00	+	03				3
;0100 	01 110		00	+	04				4
;0101 	01 111		00	+	05				5

;0010 	10 010		05	+	01				6
;0011 	10 011		05	+	02				7
;1111 	10 101		05	+	03				8
;0110 	10 110		05	+	04				9
;0111 	10 111		05	+	05				a

;1001 	11 001		0b	+	00				b
;1010 	11 010		0b	+	01				c
;1011 	11 011		0b	+	02				d
;1101 	11 101		0b	+	03				e
;1110 	11 110		0b	+	04				f



;OKAY
;XXX TODO mapping of last bits can also be directly added without lookup?! \o/
;3210	123 45		   v						ser	orig
;1000 	010 01		00	+	00		results in	0	8
;0000 	010 10		00	+	01				1	0
;0001 	010 11		00	+	02				2	1
;1100 	011 01		03	+	00				3	c
;0100 	011 10		03	+	01				4	4
;0101 	011 11		03	+	02				5	5

;0010 	100 10		05	+	01				6	2
;0011 	100 11		05	+	02				7	3
;1111 	101 01		08	+	00				8	f
;0110 	101 10		08	+	01				9	6
;0111 	101 11		08	+	02				a	7

;1001 	110 01		0b	+	00				b	9
;1010 	110 10		0b	+	01				c	a
;1011 	110 11		0b	+	02				d	b
;1101 	111 01		0e	+	00				e	d
;1110 	111 10		0e	+	01				f	e

;OKAY
;XXX TODO mapping of last bits can also be directly added without lookup?! \o/
;3210	123 45		   v						ser	orig
;1000 	010 01		00	+	00		results in	0	8
;0000 	010 10		00	+	01				1	0
;0001 	010 11		00	+	02				2	1
;1100 	011 01		03	+	00				3	c
;0100 	011 10		03	+	01				4	4
;0101 	011 11		03	+	02				5	5

;0010 	100 10		05	+	01				6	2
;0011 	100 11		05	+	02				7	3
;1111 	101 01		08	+	00				8	f
;0110 	101 10		08	+	01				9	6
;0111 	101 11		08	+	02				a	7

;1001 	110 01		0b	+	00				b	9
;1010 	110 10		0b	+	01				c	a
;1011 	110 11		0b	+	02				d	b

;orig2frob
;			!byte $01, $02, $06, $07, $04, $05, $09, $0a
;			!byte $00, $0b, $0c, $0d, $03, $0e, $0f, $08
;frob2orig
;			!byte $08, $00, $01, $0c, $04, $05, $02, $03
;			!byte $0f, $06, $07, $09, $0a, $0b, $0d, $0e
;
;is a single tab anyway, no other nibble associated
;OKAY
;XXX TODO mapping of last bits can also be directly added without lookup?! \o/
;3210	1234 5		   v						ser
;1000 	0100 1		ff	+	01		results in	0	;need to check if overflow is okay, depends on highnibble associated, seems to be zero else als results in zero again also for highnibble
;0000 	0101 0		01	+	00				1
;0001 	0101 1		01	+	01				2
;1100 	0110 1		02	+	01				3
;0100 	0111 0		04	+	00				4
;0101 	0111 1		04	+	01				5

;0010 	1001 0		06	+	00				6
;0011 	1001 1		06	+	01				7
;1111 	1010 1		07	+	01				8
;0110 	1011 0		09	+	00				9
;0111 	1011 1		09	+	01				a

;1001 	1100 1		0a	+	01				b
;1010 	1101 0		0c	+	00				c
;1011 	1101 1		0c	+	01				d
;1101 	1110 1		0d	+	01				e
;1110 	1111 0		0f	+	00				f


;			ldy $1c01		;11111222
;.chksum			eor #$00
;			eor $0101,x
;			eor $0102,x
;			eor $0103,x
;			sta .chksum + 1
;;21
;			bvc *
;			lax $1c01		;22333334
;			asr #$c0		;coudl also be and #$c0
;			sta .twos + 1
;			lda tabAAAAAbbb,y	;AAAAAbbb
;			;if we could mask out bts standalone we could just add bits - 1
;.twos			adc tab0bb00000		;20/40/60 %00000000 %00000101 %00001011		could be ZP
;			pha
;			txa			;a pity that carry can't be fetched over
;			asr #$3f
;			sta three + 1		;00033333 4
;;29
;			lda $1c01		;44445555
;			ldx #$0f
;			sax .fives + 1
;			arr #$f0		;44444000
;			lsr			;04444400 0 -> clc
;			tay
;.three			lda tab000AAAAA		;ZP
;			adc tab0bbbbb00,y	;low		could be shifted once more
;			pha
;;25
;			lax $1c01		;56666677
;			cmp #$80		;c = 5		use rol if 66666000 is used
;			rol .fives + 1
;			and #$7c		;06666600	could be shifted once more eitehr 66666000 or 00666660
;			tay
;;			lda #$00
;;.fives			adc tab00005555		;ZP		;lowest bit added as 0 or 1
;;			lda #$00
;.fives			lda tab000AAAAA		;ZP		;lowest bit added as 0 or 1
;			adc tab0bbbbb00,y	;low
;			pha
;;25
;			ldy $1c01		;77788888
;			lda #$03
;			sax .seven + 1		;00000077	1,2,3
;			;could use sbx, but sets carry
;.seven			lda tab000000AA		;ZP
;			adc tabAAAbbbbb,y	;AAAbbbbb
;			tsx
;			pha
;			bne .loop
;;24
;
;
;
;
;
;.defrob
;			lax $0100
;			ldx #$0f
;			sbx #$00
;			lsr
;			lsr
;			lsr
;			lsr
;			tay
;			lda swap_hi,y		;in addition need to ser2bin
;			ora swap_lo,x
;26 cycles per byte
			;if lucky only an eor thingy or defrob on checksum and current sector


;jmp ($1800) targets $0003 and $0083, send data must happen from here in two steps? unary and ary bitpairs



;with eor
;3210   1234 5
;0000   0101 0          -> 0            0       1       eor 1
;0001   0101 1          -> 0    adc 1   1       1
;0010   1001 0          -> 2            2       3       eor 1
;0011   1001 1          -> 2    adc 1   3       3
;0100   0111 0          -> 4            4       5       eor 1
;0101   0111 1          -> 4    adc 1   5       5
;0110   1011 0          -> 6            6       7       eor 1
;0111   1011 1          -> 6    adc 1   7       7
;1000   0100 1          -> 7    adc 1   8       8
;1001   1100 1          -> 8    adc 1   9       9
;1010   1101 0          -> a            a       b       eor 1
;1011   1101 1          -> a    adc 1   b       b
;1100   0110 1          -> b    adc 1   c       c
;1101   1110 1          -> c    adc 1   d       d
;1110   1111 0          -> e            e       f       eor 1
;1111   1010 1          -> e    adc 1   f       f

;3210   123 45
;0000   010 10          -> 1	-	1
;0001   010 11          -> 1	-	0
;0010   100 10          -> 3	-	1
;0011   100 11          -> 3	-	0
;0100   011 10          -> 5	-	1
;0101   011 11          -> 5	-	0
;0110   101 10          -> 7	-	1
;0111   101 11          -> 7	-	0
;1000   010 01          -> 8	-	
;1001   110 01          -> 8	-
;1010   110 10          -> 8	-	1
;1011   110 11          -> 8	-	0
;1100   011 01          -> 5	-	2
;1101   111 01          -> f	-	2
;1110   111 10          -> f	-	1
;1111   101 01          -> 6	-	8




;as bit 2 is always same as original data, things can be partitioned here too? to 2-x-2?
;3210   12345
;0000   01x10          -> 0
;0001   01x11          -> 1
;0010   10x10          -> 2
;0011   10x11          -> 3
;0100   01x10          -> 0
;0101   01x11          -> 1
;0110   10x10          -> 2
;0111   10x11          -> 3
;1000   01x01          -> 8
;1001   11x01          -> 9
;1010   11x10          -> a
;1011   11x11          -> b
;1100   01x01          -> 8
;1101   11x01          -> 9
;1110   11x10          -> a
;1111   10x01          -> b
