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

;--------------------------------------------------------------------------------------------------------------------------
; Much <3 goes to Krill for answering all the questions, giving very valueable hints, chats, ideas!!!
; Talking about bitorders for serial transfer, and sanity checks helped a lot!
; Also Sparkle from Sparta is a good source of inspiration <3
; Many thanks go out to THCM, sidspieler, dano and doomed for all the testing <3
; Big thanks to Ikwai for more sx64 testing and helping to debug on real hardware with his logic analyzer, giving me a
; chis-like history <3
;--------------------------------------------------------------------------------------------------------------------------

!convtab pet
!cpu 6510
!src "config.inc"
!src "constants.inc"

;config params
.LOAD_IN_ORDER_LIMIT	= $ff ;number of sectors that should be loaded in order (then switch to ooo loading)
.LOAD_IN_ORDER		= 0   ;load all blocks in order to check if depacker runs into yet unloaded memory
.POSTPONED_XFER		= 1   ;postpone xfer of block until first halfstep to cover settle time for head transport, turns out to load slower in the end?
;XXX TODO implement readahead, before going to idle but with eof already internally set, force read of last sector again?
;ldy <.last_block_num
;inc .wanted,y
;set cached flag?
;reenter read_sector
;skip xfer if cached is set? eof + cached = go to idle and finally drop all lines?

;XXX TODO
;load each sector fully, even if header is not what we want yet, we can then decide after loading and stow sector away if it is last sector? so all decisions are aggregated and not spread all over

;XXX TODO directory: move sectorinfo and disksideinfo to the beginning of dirsector, then place empty space to the end of dirsector, we can make use of free space of dirsector + free space and find out min cache size by that
;stow away last sector if coming and if it suits into cache size, no need to force last sector anymore


.IGNORE_ILLEGAL_FILE	= 1 ;on illegal file# halt floppy, turn off motor and light up LED, else just skip load
.FORCE_LAST_BLOCK	= 1 ;load last block of file last, so that shared sector is cached and next file can be loaded faster. works on loadcomp, but slower on loadraw
.SHRYDAR_STEPPING	= 0 ;so far no benefit on loadcompd, and causes more checksum retries on 2 of my floppys, also let's one of the 1541-ii choke at times and load forever when stuck on a half track
.DELAY_SPIN_DOWN	= 1 ;wait for app. 4s until spin down in idle mode
.SANCHECK_HEADER_0F	= 0 ;does never trigger
.SANCHECK_HEADER_ID	= 0 ;does never trigger
.SANCHECK_TRAILING_ZERO = 1 ;check for trailing zeroes after checksum byte
.SANCHECK_TRACK		= 1 ;check if on right track after header is read
.SANCHECK_SECTOR	= 0 ;check if sector # is within range
.INTERLEAVE		= 4
.GCR_125		= 1

;constants
.STEPPING_SPEED		= $18					;98 is too low for some ALPS and Sankyo-Drives
.STEPPING_SPEED_	= $0c
.CHECKSUM_CONST1	= $05					;%00000101 -> 4 times 01010
.CHECKSUM_CONST2	= $29					;%00101001
.CHECKSUM_CONST3	= $4a					;%01001010
.EOR_VAL		= $7f
.HEADER_0F		= $0f xor .EOR_VAL
.DIR_SECT		= 18
.DIR_TRACK		= 18
.BUSY			= $02
.BLOCK_READY		= $08
.LED_OFF		= $f7
.LED_ON			= $08
.MOTOR_OFF		= $fb
.MOTOR_ON		= $04

;adresses
.reset_drive		= $fffc	;eaa0
.drivecode		= $0000
.bootstrap		= $0700
.tables			= $0200

			;XXX TODO allocate this dynamically at end of code and before tables? but bootstrap needs to be in stack then and page needs to be skipped upon upload?
.dir_load_addr		= .directory + 4
.dir_file_size		= .directory + 6
.dir_diskside		= .directory + 3
.dir_first_file_track	= .directory + 0			;starttrack of first file in dir
.dir_first_file_index	= .directory + 1			;how many blocks are used on this track up to the file
.dir_first_block_pos	= .directory + 2			;startposition within block
								;with those three values, the absolute position on disk is represented
.drivecode_start
!pseudopc .drivecode {
.zp_start

;.free			= .zp_start + $00
.max_sectors		= .zp_start + $08			;maximum sectors on current track
.dir_sector		= .zp_start + $10
.blocks_on_list		= .zp_start + $11			;blocks tagged on wanted list
.tempx			= .zp_start + $18
;.free			= .zp_start + $19
.desired_sect		= .zp_start + $20
.ser2bin		= .zp_start + $30			;$30,$31,$38,$39
.blocks 		= .zp_start + $28			;2 bytes
.wanted			= .zp_start + $3e			;21 bytes
.index			= .zp_start + $54			;current blockindex
.track			= .zp_start + $56			;DT ;current track
.to_track		= .zp_start + $58			;DT
.sector			= .zp_start + $59			;DS
.filename		= .zp_start + $5a
.preamble_data		= .zp_start + $60
.track_frob		= .zp_start + $66
.block_size		= .zp_start + $68
.filenum 		= .zp_start + $69
;.free			= .zp_start + $6a
.is_loaded_sector	= .zp_start + $6c
.first_block_size	= .zp_start + $6e
.current_id1		= .zp_start + $70
.current_id2		= .zp_start + $71
.last_block_num		= .zp_start + $72
.last_block_size	= .zp_start + $74
.first_block_pos	= .zp_start + $76
.free			= .zp_start + $78
.block_num		= .zp_start + $79
.dir_entry_num		= .zp_start + $7a
.end_of_file		= .zp_start + $7c

.FS			= 0					;first sector
.DT			= 18					;dir_track
.DS			= 18					;dir_sector
.PA			= $ff					;preamble
.BL			= $ff					;blocks_on_list
.WT			= $ff					;wanted list
___			= $ff
.S0			= $00 xor .EOR_VAL			;ser2bin value 0/9
.S1			= $09 xor .EOR_VAL			;ser2bin value 1/8

.tab00005555_hi		= * + $00
.tab00333330_hi		= * + $00
.tab05666660_lo		= * + $01
.tab00700077_hi		= * + $00				;XXX currently not used

;tab00AAAAA0
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, $f0, ___, $70, ___, $60, ___, ___, ___, $b0, ___, $30, ___, $20, ___
;                        !byte ___, ___, ___, ___, $50, ___, $40, ___, ___, ___, $80, ___, $10, ___, $00, ___
;                        !byte ___, ___, $e0, ___, $d0, ___, $c0, ___, ___, ___, $a0, ___, $90, ___, ___, ___
;tab0000AAAA
;                        !byte ___, ___, ___, ___, $e0, $60, $a0, $20, ___, $40, $80, $00, $e0, $c0, $a0, $80
;tab0Abbbbb0
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, $1e, ___, $1f, ___, $17, ___, ___, ___, $1a, ___, $1b, ___, $13
;                        !byte ___, ___, ___, ___, ___, $1d, ___, $15, ___, ___, ___, $10, ___, $19, ___, $11
;                        !byte ___, ___, ___, $16, ___, $1c, ___, $14, ___, ___, ___, $12, ___, $18, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, $0e, ___, $0f, ___, $07, ___, ___, ___, $0a, ___, $0b, ___, $03
;                        !byte ___, ___, ___, ___, ___, $0d, ___, $05, ___, ___, ___, $00, ___, $09, ___, $01
;                        !byte ___, ___, ___, $06, ___, $0c, ___, $04, ___, ___, ___, $02, ___, $08, ___, ___
;tabAAA000AA
;                        !byte ___, $b0, $80, $a0, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, $20, $00, $80, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___

			;     0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
                        !byte ___, $b0, $80, $a0, $f0, $60, $b0, $20, ___, $40, $80, $00, $e0, $c0, $a0, $80	;00
                        !byte .DS, .BL, $f0, $1e, $70, $1f, $60, $17, ___, ___, $b0, $1a, $30, $1b, $20, $13	;10
                        !byte .FS, $20, $00, $80, $50, $1d, $40, $15, ___, ___, $80, $10, $10, $19, $00, $11	;20
                        !byte .S0, .S1, $e0, $16, $d0, $1c, $c0, $14, .S1, .S0, $a0, $12, $90, $18, .WT, .WT	;30
                        !byte .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT	;40
                        !byte .WT, .WT, .WT, $0e, ___, $0f, .DT, $07, .DT, ___, ___, $0a, ___, $0b, ___, $03	;50
                        !byte .PA, .PA, .PA, .PA, .PA, $0d, ___, $05, ___, ___, ___, $00, ___, $09, ___, $01	;60
                        !byte ___, ___, ___, $06, ___, $0c, ___, $04, ___, ___, ___, $02, ___, $08		;70


			;XXX TODO, move PA to $68, need to fix table afterwards by setting $00? but could the do lda $68,y? but still not possible with 8 bit addressing, but would make setup easier



			;XXX TODO /!\ if making changes to gcr_read_loop also the partly decoding in read_sector should be double-checked, same goes for timing changes
			;XXX see if we can use bit 2 from original data, would save space in tables

;this reflects the perfect timing, it is most likely delayed by 2-3 cycles due to bvs branching before?
;in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the end and then there's up to 5 cycles delay (branch + fallthrough)

;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          2222222222222222222222222222222233333333333333333333333333333333444444444444444444444444444444445555555555555555555555555555555511111111111111111111111111111111
;                2                      ccccccccccc   3                   ggggggggggggggggggggg   4ggg                 ccccccc   5           v      1           bbbbbbbbbbbb
;1          222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555111111111111111111111111111111
;                2                      ccccccccccc   3                   ggggggggggggg   4ggg                 ccccccc   5           v      1           bbbbbbbbbb
;2          22222222222222222222222222223333333333333333333333333333444444444444444444444444444455555555555555555555555555551111111111111111111111111111
;                2                      ccccccccccc   3                   ggggg   4ggg                 ccccccc   5           v      1           bbbbbbbb
;3          2222222222222222222222222233333333333333333333333333444444444444444444444444445555555555555555555555555511111111111111111111111111
;                2                      ccccccccccc   3                      4                 ccccccc   5           v      1           bbbbbb
;b = bvc *
;c = checksum
;v = v-flag clear
;g = gcr slowdown

.read_loop
			ldx #$3e
			lda $1c01				;22333334
			sax <.threes + 1
			asr #$c1
			tax
			lda .tab11111000_hi,y
.twos			ora .tab02200222_lo,x
			tsx
			pha					;$0100
			beq .gcr_end

			eor $0101,x
			eor $0103,x
.gcr_entry
			sta <.chksum2 + 1
			lda $1c01				;44445555		hird read
			ldx #$0f
			sax <.fives + 1

			arr #$f0
			tay					;44444---
!if .GCR_125 = 0 {
			ldx #$03
}
.threes			lda <.tab00333330_hi			;ZP!
			ora .tab44444000_lo,y
			pha					;$0103

.gcr_slow1		lda $1c01				;56666677		fourth read
			sax <.sevens + 1			;----6677
			asr #$fc				;-566666-
			tax

.fives			lda <.tab00005555_hi			;ZP!
			ora <.tab05666660_lo,x			;ZP but index
			pha					;$0102
.chksum			eor #$00				;103,101,100
.chksum2		eor #$00
			sta <.chksum + 1

.gcr_slow2		lax $1c01				;77788888	fifth read
			asr #$40
			tay

			lda .tab7d788888_lo,x
			ldx #$07
!if .GCR_125 = 1 {
.sevens			adc .tab0070dd77_hi,y			;clears v-flag, decodes the remaining bits of quintuple 7, no need to set x to 3, f is enough
} else {
.sevens			adc .tab00700077_hi,y			;ZP! clears v-flag, decodes the remaining bits of quintuple 7
}
			pha					;$0101
			lda $1c01				;11111222	first read
			sax <.twos + 1
			and #$f8
			tay

			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop

			jmp .next_sector
.gcr_end
			;Z-Flag = 1 on success, 0 on failure (wrong type)
			jmp .back_read_sector
			jmp .back_read_header
.slow_table
			!byte <(.slow0 - .slow6) + 2
			!byte <(.slow2 - .slow6) + 2
			!byte <(.slow4 - .slow6) + 2
			!byte <(.slow6 - .slow6) + 2


!ifdef .second_pass {
	!warn $0100 - *, " bytes remaining in zeropage."
}

!if >*-1 != >.read_loop { !error "read_sector not in one page: ", .read_loop, " - ", * }

			* = .tables
.preamble_
			beq +
			ldx #$80
			adc <.first_block_size			;else add first block size as offset, might change carry
+
			sta <.preamble_data + 1			;block address low

			lda .dir_load_addr + 1,y		;add load address highbyte
			;sbc #$00				;subtract one in case of overflow
			;clc
			adc <.block_num				;add block num

			;clc					;should never overrun, or we would wrap @ $ffff?
			sta <.preamble_data + 2			;block address high
			stx <.preamble_data + 3 + CONFIG_DECOMP	;ack/status to set load addr, signal block ready

!if .POSTPONED_XFER = 1 {
			dec <.blocks_on_list			;nope, so check for last block on track (step will happen afterwards)?
			bpl +
			lda <.end_of_file			;eof?
			bmi +

			dec .en_dis_seek			;enable jmp, skip send of data for now
			jmp .en_dis_seek_
+
}
			jmp .start_send

;tables with possible offsets
.tab11111000_hi		= .tables + $00
.tab44444000_lo 	= .tables + $04
.tab7d788888_lo		= .tables + $00

;table wth no offset
.tab02200222_lo		= .tables + $00

	 		* = .tables + $22
.table_start		;combined tables, gaps filled with junk
;                        !byte           $0e, $0a, $24, $00, $06, $02, $28, $4e, $4f, $47, $2c, $4a, $4b, $43
;                        !byte $30, $31, $4d, $45, $34, $40, $49, $41, $38, $46, $4c, $44, $3c, $42, $48, $3f
;                        !byte $40, $41, $0f, $0b, $0d, $09, $0c, $08, $f0, $5e, $5f, $57, $0e, $5a, $5b, $53
;                        !byte $70, $51, $5d, $55, $0f, $50, $59, $51, $60, $56, $5c, $54, $07, $52, $58, $5f
;                        !byte $60, $61, $07, $03, $05, $01, $04, $67, $b0, $4e, $4f, $47, $0a, $4a, $4b, $43
;                        !byte $30, $71, $4d, $45, $0b, $40, $49, $41, $20, $46, $4c, $44, $03, $42, $48, $7f

                        !byte           $0e, $0a, $f0, $00, $06, $02, $e1, $4e, $4f, $47, $d2, $4a, $4b, $43
                        !byte $c3, $b4, $4d, $45, $a5, $40, $49, $41, $96, $46, $4c, $44, $87, $42, $48, $78
                        !byte $69, $5a, $0f, $0b, $0d, $09, $0c, $08, $f0, $5e, $5f, $57, $0e, $5a, $5b, $53
                        !byte $70, $4b, $5d, $55, $0f, $50, $59, $51, $60, $56, $5c, $54, $07, $52, $58, $3c
                        !byte $2d, $1e, $07, $03, $05, $01, $04, $0f, $b0, $4e, $4f, $47, $0a, $4a, $4b, $43
                        !byte $30, $e1, $4d, $45, $0b, $40, $49, $41, $20, $46, $4c, $44, $03, $42, $48, $d3

;                        !byte           $0e, $0a, ___, $00, $06, $02, ___, $4e, $4f, $47, ___, $4a, $4b, $43
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___
;                        !byte ___, ___, $0f, $0b, $0d, $09, $0c, $08, $f0, $5e, $5f, $57, $0e, $5a, $5b, $53
;                        !byte $70, ___, $5d, $55, $0f, $50, $59, $51, $60, $56, $5c, $54, $07, $52, $58, ___
;                        !byte ___, ___, $07, $03, $05, $01, $04, ___, $b0, $4e, $4f, $47, $0a, $4a, $4b, $43
;                        !byte $30, ___, $4d, $45, $0b, $40, $49, $41, $20, $46, $4c, $44, $03, $42, $48, ___

			;----------------------------------------------------------------------------------------------------
			;
			; TURN DISK OR READ IN NEW DIRECTORY BLOCK
			;
			;----------------------------------------------------------------------------------------------------
.td_code_
			beq .td_idle
			jmp .turn_disc				;still wrong diskside
.td_idle
			sta <.filenum				;reset filenum
			jmp .idle_				;right diskside, go idle

.td_lf
			jmp .load_file_				;dir sector changed, try to load file now

			nop
			nop
			nop

			!byte $50
.turn_disc_back
			sty <.blocks_on_list			;clear, as we didn't reach the dec <.blocks_on_list on this code path
			ldx #$0d				;tab value $0d
			dex
			iny
			dop
			!byte $40
			stx .en_dis_td
			!byte $05				;ora $xx
			nop
-
			pla
			ldx #$09
			sbx #$00
			eor <.ser2bin,x				;swap bits 3 and 0
			dey
			bcs +

                        !byte                                         $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, $47, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08
+
			sta .directory,y
			bne -
			lax <.filenum				;just loading a new dir-sector, not requesting turn disk?
			bcs +

                        !byte                                         $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
                        !byte $d0, $38, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18
+
			cmp #BITFIRE_REQ_DISC
			bcc .td_lf
			eor .dir_diskside			;compare side info
			bcs .td_code_

                        !byte                                         $a0, $0e, $0f, $07, $02, $0a, $0b, $03		;9 bytes
                        !byte $90, $29, $0d, $05, $08, $00, $09, $01, $1a, $06, $0c, $04, $da, $02, $08, $f3

;                        !byte $50, ___, ___, ___, $0d, ___, ___, ___, $40, ___, ___, ___, $05, ___, ___, ___		;20 bytes with 3 dops
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $80, $0e, $0f, $07, $00, $0a, $0b, $03
;                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
;                        !byte $d0, ___, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $a0, $0e, $0f, $07, $02, $0a, $0b, $03		;9 bytes
;                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___

;tab000bbbbb
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $4e, $4f, $47, ___, $4a, $4b, $43
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $5e, $5f, $57, ___, $5a, $5b, $53
;                        !byte ___, ___, $5d, $55, ___, $50, $59, $51, ___, $56, $5c, $54, ___, $52, $58, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $4e, $4f, $47, ___, $4a, $4b, $43
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03
;                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $1e, $1f, $17, ___, $1a, $1b, $13
;                        !byte ___, ___, $1d, $15, ___, $10, $19, $11, ___, $16, $1c, $14, ___, $12, $18, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03
;                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
;tabbbbbb000
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, $0e, ___, ___, ___
;                        !byte ___, ___, ___, ___, $0f, ___, ___, ___, ___, ___, ___, ___, $07, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, $0a, ___, ___, ___
;                        !byte ___, ___, ___, ___, $0b, ___, ___, ___, ___, ___, ___, ___, $03, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, $0d, ___, ___, ___, ___, ___, ___, ___, $05, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, $00, ___, ___, ___
;                        !byte ___, ___, ___, ___, $09, ___, ___, ___, ___, ___, ___, ___, $01, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, $06, ___, ___, ___
;                        !byte ___, ___, ___, ___, $0c, ___, ___, ___, ___, ___, ___, ___, $04, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, $02, ___, ___, ___
;                        !byte ___, ___, ___, ___, $08, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;tab0bb00bbb
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, $0e, $0a, ___, $00, $06, $02, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, $0f, $0b, $0d, $09, $0c, $08, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, $07, $03, $05, $01, $04, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;tabAAAAA000
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $f0, ___, ___, ___, ___, ___, ___, ___
;                        !byte $70, ___, ___, ___, ___, ___, ___, ___, $60, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $b0, ___, ___, ___, ___, ___, ___, ___
;                        !byte $30, ___, ___, ___, ___, ___, ___, ___, $20, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte $50, ___, ___, ___, ___, ___, ___, ___, $40, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $80, ___, ___, ___, ___, ___, ___, ___
;                        !byte $10, ___, ___, ___, ___, ___, ___, ___, $00, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $e0, ___, ___, ___, ___, ___, ___, ___
;                        !byte $d0, ___, ___, ___, ___, ___, ___, ___, $c0, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $a0, ___, ___, ___, ___, ___, ___, ___
;                        !byte $90, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___


			* = $300

			;----------------------------------------------------------------------------------------------------
			;
			; CODE TO SLOW DOWN GCR LOOP DEPENDING ON BITRATE
			;
			;----------------------------------------------------------------------------------------------------

.gcr_slow1_00
			lsr $00
			jmp .gcr_slow1_20
!if .GCR_125 = 1 {
.tab0070dd77_hi
                        !byte                          $b0, $80, $a0, ___, $b0, $80, $a0, ___, $b0, $80, $a0
}
.gcr_slow1_20
			lda ($00,x)
			nop
.gcr_slow1_40
			nop
.slow6			lda $1c01
			jmp .gcr_slow1 + 3

.slow0			jmp .gcr_slow1_00
.slow2			jmp .gcr_slow1_20
.slow4			jmp .gcr_slow1_40

			;would also suit at $91, $95, $99
!if .GCR_125 = 1 {
.bitrate
                        !byte                $60, $40, $20, $00, $80, ___, $20, $00, $80, ___, $20, $00, $80
			                     ;|bitrate|bitrate + table combined ->
}

			;----------------------------------------------------------------------------------------------------
			;
			; SEND PREAMBLE AND DATA
			;
			;----------------------------------------------------------------------------------------------------

.send_sector_data_setup
			lda #.sendloop - .branch - 2		;redirect branch to sendloop
			sta .branch + 1				;meh, sta exists twice, but can't be saved
			lda #$68				;place mnemonic pla in highbyte
			sta .sendloop
			ldy <.block_size			;blocksize + 1
			inx					;x = $0b -> indicate second round, does not hurt the sax
			bne .sendloop				;could work with dop here (skip pla), but want to prefer data_entry with less cycles on shift over
.start_send							;entered with c = 0
			ldy #$03 + CONFIG_DECOMP + 1		;num of preamble bytes to xfer. With or without barrier, depending on stand-alone loader or not
-
			lax <.preamble_data - 1,y
			ldx #$09				;masking value
			sbx #$00				;scramble byte while sending, enough time to do so, preamble is called via jsr, so plenty of time between sent bytes
			eor <.ser2bin,x				;swap bits 3 and 0 if they differ, table is 4 bytes only
			sta <.preamble_data - 1,y		;meh, 16 bit
			dey
			bne -

			lda #.preloop - .branch - 2		;be sure branch points to preloop
			sta .branch + 1
			sty .sendloop
			ldx #$0a				;masking value for later sax $1800 and for preamble encoding
			ldy #$03 + CONFIG_DECOMP		;num of preamble bytes to xfer. With or without barrier, depending on stand-alone loader or not
.preloop
.sendloop = * + 2						;send the data block
			lda <.preamble_data,y
								;just pull from stack instead of lda $0100,y, sadly no tsx can be done, due to x being destroyed

								;our possibiloities to send bits:
								;...-0.1.
								;...10.-.

								;...+2.3.
								;...32.-.

								;...-4.5.
								;...54.-.

								;...+6.7.
								;...76.-.

			bit $1800
			bmi *-3
			sax $1800				;76540213	-> ddd-0d1d

			dey
			asl					;6540213. 7
			ora #$10				;654+213. 7	-> ddd+2d3d
			bit $1800
			bpl *-3
			sta $1800

			ror					;7654+213 .
			asr #%11110000				;.7654... .	-> ddd54d-d
			bit $1800
			bmi *-3
			sta $1800

			lsr					;..7654..
			asr #%00110000				;...76...	-> ddd76d-d
			cpy #$ff				;XXX TODO could make loop faster here, by looping alread here on bne? needs a bit of code duplication then
			bit $1800
			bpl *-3
			sta $1800

.branch			bcc .preloop

			cpx #$0a				;keep code here small to not waste much time until busy flag is set after sending
			beq .send_sector_data_setup

			lda #.BUSY				;8 cycles until poll, busy needs to be set asap
			bit $1800
			bmi *-3
			sta $1800				;signal busy after atn drops

!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}
			;----------------------------------------------------------------------------------------------------
			;
			; BLOCK OF TRACK LOADED AND SENT
			;
			;----------------------------------------------------------------------------------------------------

!if .POSTPONED_XFER = 1 {
.en_dis_seek		eor .send_back
}
			lda <.blocks_on_list			;decrease block count, last block on wishlist?
			bpl +
.track_finished
			;XXX TODO make this check easier? only done hre?
			lda <.end_of_file			;EOF
			bmi .idle
.en_dis_seek_
			;set stepping speed to $0c, if we loop once, set it to $18
			;XXX TODO can we always do first halfstep with $0c as timerval? and then switch to $18?
			lda #18
-
!if .POSTPONED_XFER = 1 {
			sec					;set by send_block and also set if beq
}
			isc <.to_track
			beq -					;skip dirtrack however

			jmp .load_track
+
			jmp .next_sector			;nope, continue loading

			;----------------------------------------------------------------------------------------------------
			;
			; ENTRY POINT OF IDLE LOOP
			;
			;----------------------------------------------------------------------------------------------------
.idle
			inc <.filenum				;autoinc always, so thet load_next will also load next file after a load with filenum
.idle_
.skip_load
			lda $1c00				;turn off LED
!if .DELAY_SPIN_DOWN = 0 & CONFIG_MOTOR_ALWAYS_ON = 0 {
			and #.MOTOR_OFF & .LED_OFF
} else {
			and #.LED_OFF
}
			sta $1c00
!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
			and #.MOTOR_OFF
			tax
}
			;----------------------------------------------------------------------------------------------------
			;
			; RECEIVE/WAIT FOR A BYTE FROM HOST
			;
			;----------------------------------------------------------------------------------------------------

.get_byte
			ldy #$80
			sty $1800
.lock
			lda #$80
			sta <.filename
.bitloop
			lda #$04
.wait_bit1
!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
.check_spindown
			;have a free running counter so that we only check on 1c0d?
			;check for underrun of timer here? bit $1c0d, but use 1c05 as timer?
			bit $1c0d
			bpl .end_sd_check
			sty $1c05				;clears irq flag in $1c0d

			bit $1800				;check for clk toggle
			bmi +
			bne .got_bit1
+
			dey
			bne .end_sd_check
			stx $1c00				;turn off motor
.end_sd_check
}
			bit $1800
			bmi .lock
			beq .wait_bit1				;do we have clk == 1?
.got_bit1
			lda $1800				;now read again
			bmi .lock				;check for lock
			lsr
			ror <.filename

			lda #$04
.wait_bit2
			bit $1800
			bmi .lock
			bne .wait_bit2				;do we have clk == 0?
			lda $1800
			bmi .lock
			lsr					;all sane, we can read bit in data
			ror <.filename
			bcc .bitloop				;more bits to fetch?

			lda #.BUSY
			sta $1800				;set busy bit
			lda <.filename
			eor #$ff				;invert bits, saves a byte in resident code, space is more restricted there, so okay

;!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
;			ldy #$00
;			sty $1c05
;			ldy $1c0d
;}

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD FILE / EXECUTE COMMAND, $00..$7f, $ef, $f0..$fe, $ff
			; XXX TODO files up to $ee would be possible in theory, but more dir sectors would be needed
			;
			;----------------------------------------------------------------------------------------------------

			;load file, file number is in A
.load_file
!if .LOAD_IN_ORDER = 1 {
			ldy #$00
			sty .desired_sect
}
			cmp #BITFIRE_RESET
			bne *+5
			jmp (.reset_drive)
			cmp #BITFIRE_LOAD_NEXT
			beq +
			sta <.filenum				;set new filenum
+
			cmp #BITFIRE_REQ_DISC			;sets carry if so, used later on on bcs
			lda #.MOTOR_ON
			bcs +					;no LED during turn disc
			ora #.LED_ON
+
			ora $1c00				;turn on motor (no matter if already on)
			sta $1c00

			bcc +

			;----------------------------------------------------------------------------------------------------
			;
			; TURN DISC / LOAD NEW DIR SECTOR
			;
			;----------------------------------------------------------------------------------------------------
.turn_disc
			ldy #.DIR_SECT				;first dir sector
!if .SANCHECK_HEADER_ID = 1 {
			lda #$90
			sta .en_set_id				;disable id-check, as new disc side can mean, new id
}
.load_dir_sect
			tya
			sta <.dir_sector
			ldx #$4c
			stx .en_dis_td				;enable jump back
			ldx #.DIR_TRACK				;set target track to 18
			stx <.to_track
			ldx #$00
			stx <.last_block_num			;end at index 0
			jmp .turn_disc_entry			;a = sector, x = 0 = index
+
			;----------------------------------------------------------------------------------------------------
			;
			; LOAD A FILE
			;
			;----------------------------------------------------------------------------------------------------

.load_file_
			lax <.filenum				;load filenum
			ldy #.DIR_SECT - 1			;second dir sector
			sbc #$3e				;carry is cleared
			bcs +					;no underflow, filenum is >= $3f
			txa
			iny					;select first dir sector
+
			cpy <.dir_sector			;is this dir sector loaded?
			bne .load_dir_sect

			asl					;shift by 4 to get index into dir
			asl
			sta <.dir_entry_num			;and save

			;----------------------------------------------------------------------------------------------------
			;
			; FIND FILE IN DIRECTORY AND SUM UP FILESIZE TO FIND POSITION ON DISK AND FIRST AND LAST BLOCK SIZE
			;
			;----------------------------------------------------------------------------------------------------

.find_file_in_dir
			lda .dir_first_file_index		;sectorindex of first file in dir (not sector number, but as if written with interleave = 1)
			sta <.blocks + 1
			lda .dir_first_block_pos		;pos in first sector where file starts
			sta <.blocks + 0

			ldy .dir_first_file_track		;track of first file in dir
			ldx #$fc
.next_dir_entry
.no_next_track
			txa
			sbx #-4					;start with x = 0 by this
			;XXX TODO better do sum up all filesizes with 24 bit and then subtract sectors until block + 1 and block + 2 is reached?
			cpx <.dir_entry_num
			beq .next_track				;silly, but need to set max_sectors for later use

			lda <.blocks + 0
			sec
			adc .dir_file_size + 0,x
			sta <.blocks + 0

			;XXX TODO on very huge files, this could fail, as we overflow!!! but then again, files are max $d000 in size due to i/o limitation?
			lda <.blocks + 1
			adc .dir_file_size + 1,x
			sta <.blocks + 1
.next_track
			jmp .set_max_sectors			;setup max_sectors, expects track in Y, returns to .find_file_back, can't jsr here
.find_file_back							;we return from max_sectors, do second check here, as we can fall through for free
			cpx <.dir_entry_num			;recheck again, yuck!
			beq .found_file

			lda <.blocks + 1
			sec
			sbc <.max_sectors			;reduce blockcount track by track
			bcc .no_next_track
			sta <.blocks + 1
-
			;skip dir_track				;advance track
			iny
			cpy #.DIR_TRACK
			beq -
			bne .next_track
.found_file
			;store track
			sty <.to_track

			;remember file index
			;stx .file_index			;same as dir_entry_num

			lda <.blocks + 0
			;calc first block size
			eor #$ff
			sta <.first_block_size

			lda .dir_file_size + 0,x
			cmp <.first_block_size
			ldy .dir_file_size + 1,x		;load file_size + 1
			bne .is_big				;filesize < $100 ? is_big if not
			bcs .is_big				;it is < $0100 but does not fit in remaining space? -> is_big
			sta <.first_block_size			;fits in, correct size, this will cause overflow on next subtraction and last_block_num will be zero, last_block_size will be $ff
			tax
			bne +
			;file not found
!if .IGNORE_ILLEGAL_FILE = 1 {
			jmp .skip_load
} else {
.fucked
			lda $1c00
			and #.MOTOR_OFF
			ora #.LED_ON
			sta $1c00
			jam
}
+
.is_big
			clc
			sbc <.first_block_size
			sta <.last_block_size

			bcc +					;file_size + 1
			iny
+
			sty <.last_block_num

.index_to_sectornum
			;XXX TODO block to index -> can be also done via other means? lookup?
			;remaining blocks on track to find out start sector
			lda #$00				;walk through track from pos 0 on
			ldx <.blocks + 1
			beq .found_sector
.fs_loop
			clc					;XXX could be omitted for interleave = 4?
			adc #.INTERLEAVE
			cmp <.max_sectors
			bcc +
								;next revolutiona
!if .INTERLEAVE = 4 {
			adc #$00				;increase
			and #$03				;modulo 4
} else {
			adc #$00				;increase
-
			sec					;modulo INTERLEAVE
			sbc #.INTERLEAVE			;subtract some #sectors
			bcs -					;until underflow
			adc #.INTERLEAVE
}
+
			dex
			bne .fs_loop
.found_sector
			;jmp .idle_
.turn_disc_entry
			sta <.sector
			stx <.index				;reset block index, x = 0
.load_track
			;----------------------------------------------------------------------------------------------------
			;
			; SEEK
			;
			;----------------------------------------------------------------------------------------------------
.seek
			lax <.to_track
			sec
			sbc <.track				;how many tracks to go?
			stx <.track				;save target track as current track

			ldy #$00				;make stepping positive
			bcs .seek_up				;up or downwards?
.seek_down
			eor #$ff				;down we go, so invert num tracks as it is negative
			adc #$01
			iny					;but make stepping negative
.seek_up
			asl					;counter is twice the number of tracks (halftracks)
			tax
			beq .seek_done				;nothing to step, end

!if .SHRYDAR_STEPPING = 1 {
			lda #.STEPPING_SPEED_
			cpx #$02
			beq .step_
}
.step
			lda #.STEPPING_SPEED
.step_
			sta $1c05				;clears irq flag in $1c0d
			tya
.halftrack
			eor $1c00
			sec
			rol
			and #3
			eor $1c00
			sta $1c00

!if .POSTPONED_XFER = 1 {
			lda .en_dis_seek			;$4c/$4d
			lsr
			bcs .seek_end				;nope, continue

			stx <.tempx				;save x
			jmp .start_send				;send data now
.send_back
			ldx <.tempx
			iny					;continue with seek up (Y = 0)
			inc .en_dis_seek			;disable_jmp
.seek_end
}
			bit $1c0d				;wait for timer to elapse, just in case xfer does not take enough cycles (can be 1-256 bytes)
			bpl *-3
			dex
			bne .step
			dex
			stx .is_loaded_sector			;invalidate sector cache, as we changed track
.seek_done
			ldy <.track				;already part of set_bitrate -> load track

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP BITRATE, MODIFY GCR LOOP IN ZEROPAGE
			;
			;----------------------------------------------------------------------------------------------------

.set_bitrate
!if .SANCHECK_TRACK = 1  & .SANCHECK_HEADER_ID = 1 {
			tya
			ldx #$09
			sbx #$00
			eor <.ser2bin,x
			sta <.track_frob			;needs to be precacled here
			ldx #$ff
} else {
			dex
}
.set_max_sectors
			lda #16
			cpy #25
			adc #0					;add 0/1 depending on zone
			cpy #31
			adc #0					;a is $12, $11, $10 ...
			eor #3					;a is $11, $12, $13 now
								;XXX TODO this is the value that coul be directly fed into the shifting below :-(
			cpy #18
			bcs +					;
			adc #2					;or $15 if < track 18
+
			sta <.max_sectors
			sbc #$11				;carry still set depending on cpy #18 -> 0, 1, 2, 3

			cpx #$fe
			bcs +
			jmp .find_file_back			;can only happen if we come from .set_bitrate code-path, not via .set_max_sectors, as x is a multiple of 4 there, extend range by doin two hops, cheaper than long branch XXX TODO, returned to long branch, as there is no fitting gap for second bne :-(
+
			tay
			lda $1c00
			ora #$60
			eor .bitrate,y
			sta $1c00

			ldx <.slow_table,y
			ldy #$02
-
			lda .slow6,x
			sta <.gcr_slow1,y
			dex
			dey
			bpl -

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD ALL NEEDED SECTORS OF A TRACK AS MARKED ON WANTED-LIST
			;
			;----------------------------------------------------------------------------------------------------


			lda <.sector				;now create our wishlist for the current track
.wanted_loop
			tax
			ldy <.index				;get index
			sty <.wanted,x				;write index into wantedlist
			inc <.blocks_on_list			;count number of blocks in list (per track num of blocks)
			cpy <.last_block_num			;inc index and check if index > file_size + 1 -> EOF
			bcs .load_wanted_blocks			;yep, EOF, carry is set
			inc <.index				;keep index as low as possible, so that literal blob gets loaded with lz_next_page in any case and over more than one page
			adc #.INTERLEAVE
			cmp <.max_sectors			;wrap around?
			bcc .wanted_loop			;nope

!if .INTERLEAVE = 4 {
			adc #$00				;increase
			and #$03				;modulo 4
} else {
			adc #$00				;increase
-
			sec					;modulo INTERLEAVE
			sbc #.INTERLEAVE			;subtract some #sectors
			bcs -					;until underflow
			adc #.INTERLEAVE
			clc					;XXX TODO should not happen and not needed?
}
			bne .wanted_loop			;if zero, done
								;carry is 0
								;just track is full
			sta .sector				;start next track with sector = 0
.load_wanted_blocks						;read and transfer all blocks on wishlist
			;lda #$30
			;ror
			;lsr
			;sta .eof				;-> $4c / $0c
			ror <.end_of_file			;shift in carry for later check, easiest way to preserve eof-state, if carry is set, we reached EOF

			;----------------------------------------------------------------------------------------------------
			;
			; READ A SECTOR WITH HEADER AND DO VARIOUS SANITY CHECKS ON IT
			;
			;----------------------------------------------------------------------------------------------------

			jmp .new_or_cached_sector

.back_read_header
								;XXX TODO could call stuff via jsr here, as stack is only filled with 8 bytes
!if .SANCHECK_HEADER_0F = 0 {
			ldx #$02
}
			txs					;saves 2 cycles compared to pla
			pla
!if .SANCHECK_HEADER_0F = 1 {
			cmp #.HEADER_0F				;header_0f_2
			bne .retry_no_count
			pla
			cmp #.HEADER_0F				;header_0f_1
			bne .retry_no_count
			pla
}
!if .SANCHECK_HEADER_ID = 1 {
			tay					;$0103
}
			eor <.chksum + 1
			eor #.HEADER_0F
			bne .retry_no_count			;header checksum check failed? reread
			pla					;header_id1
!if .SANCHECK_HEADER_ID = 1 {
.en_set_id		bcs .no_set_id				;will be changed to bcc/bcs to allow/skip id check, carry is always set due to preceeding cmp
			sty <.current_id2
			sta <.current_id1			;fall through is no problem, tests will succeed
			lda #$b0
			sta .en_set_id
			bne +
.no_set_id
			cpy <.current_id2
			bne .retry_no_count
			cmp <.current_id1
			bne .retry_no_count
+
}
			pla					;.header_track
			;XXX TODO is there any way of decoding 2 bytes in a loop in a smaller way?
!if .SANCHECK_TRACK = 1 {
	!if .SANCHECK_HEADER_ID = 1  {
			eor <.track_frob			;needs to be precalced, else we run out of time
	} else {
			ldx #$09
			sbx #$00
			eor <.ser2bin,x
			eor <.track
	}
			bne .retry_no_count
}
			;XXX TODO, can only be $1x or 0x
			pla					;header_sector
			ldx #$09
			sbx #$00
			eor <.ser2bin,x
!if .SANCHECK_SECTOR = 1 {
			cmp <.max_sectors
			bcs .retry_no_count
}
			sta <.is_loaded_sector			;gap still not passed by here, so still some code possible here

			;----------------------------------------------------------------------------------------------------
			;
			; HEADER DONE, NOW READ SECTOR DATA
			;
			;----------------------------------------------------------------------------------------------------

			ldx #$ff				;bytes to fetch
			ldy #$55				;type (sector)
			lda #$4c
			bne .read_gcr
.next_sector
.read_gcr_header						;read_header and do checksum, if not okay, do again
			ldx #$07				;bytes to fetch
			ldy #$52				;type (header)
			lda #$0c
.read_gcr
			txs
-
			ldx $1c00				;wait for start of sync
			bpl -
-
			ldx $1c00				;wait for end of sync
			bmi -

			ldx $1c01				;sync mark -> $ff
			clv

			bvc *

			clv
			cpy $1c01				;11111222
			bne .retry_no_count			;start over with a new header again, do not wait for a sectorheadertype to arrive
			;lsr
			;ror
			;lsr
			;eor #$06
			sta <.gcr_end				;setup return jump

			bvc *

			eor #$2c
			sta <.chksum + 1			;$20 or $60 depending if header or sector, just the right values we need there
			lda $1c01				;22333334
			ldx #$3e
			sax <.threes + 1
			asr #$c1
			eor <.chksum + 1
			bne .retry_no_count			;start over with a new header again, do not wait for a sectorheadertype to arrive
			sta <.chksum + 1

			pha
			pla
			lda #.EOR_VAL
			jmp .gcr_entry				;32 cycles until entry
.retry_no_count
			jmp .next_sector			;will be sbc (xx),y if disabled
.back_read_sector
			;7 cycles need to pass

			eor $0101
			sta <.chksum2 + 1			;checksum

			lax $1c01				;44445555

			arr #$f0
			tay					;44444---

!if .SANCHECK_TRAILING_ZERO = 1 {
			lda #$0f
			sbx #.CHECKSUM_CONST1			;4 bits of a trailing zero after checksum
			bne .retry_no_count			;check remaining nibble if it is $05
;			ldx $1c01
;			clv					;after arr, as it influences v-flag
;!if <* == $ff {
;			!error "bvc * crosses page!"
;}
;			bvc *
;			cpx #.CHECKSUM_CONST2			;0 01010 01 - more traiing zeroes
;			bne .retry_no_count
;			lda $1c01
;			and #$e0
;			cmp #.CHECKSUM_CONST3 & $e0		;010 xxxxx - and more trailing zeroes, last nibble varies on real hardware
;			bne .retry_no_count
}
			ldx <.threes + 1
			lda <.tab00333330_hi,x			;sector checksum
			ora .tab44444000_lo,y
			eor $0103
			eor <.chksum2 + 1
			eor <.chksum + 1			;XXX TODO annoying that last bytes nned to be checksummed here :-(
			bne .retry_no_count			;checksum okay? Nope, take two hops to get to the beginning of code again
.new_or_cached_sector
			ldx <.is_loaded_sector
			bmi .retry_no_count			;invalid, so skip
			lda <.wanted,x				;grab index from list (A with index reused later on after this call)
			cmp #$ff
			beq .retry_no_count			;if block index is $ff, we reread, as block is not wanted then
!if .FORCE_LAST_BLOCK = 1 {
			cmp <.last_block_num			;current block is last block on list?
			bne +					;nope continue
			ldy <.blocks_on_list			;yes, it is last block of file, only one block remaining to load?
			bne .retry_no_count			;reread
+
}
!if .LOAD_IN_ORDER = 1 {
			ldy <.desired_sect
			cpy #.LOAD_IN_ORDER_LIMIT
			bcs +
			cmp <.desired_sect
			bne .retry_no_count
+
}
			ldy #$ff				;blocksize full sector ($ff) /!\ reused later on for calculations!
			sty <.wanted,x				;clear entry in wanted list
.en_dis_td		top .turn_disc_back			;can be disabled and we continue with send_data, else we are done here already

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP SEND LOOP, AND DECIDE BLOCK SIZES FIRST
			;
			;----------------------------------------------------------------------------------------------------
.setup_send
!if .LOAD_IN_ORDER = 1 {
			inc .desired_sect
}
			cmp <.last_block_num			;compare once when code-path is still common, carry is not tainted until needed
			sta <.block_num
			tax					;is needed then however to restore flags, but cheaper
			bne .is_not_first_block
.is_first_block
			ldy <.first_block_size
			bcc .first_block_big			;bcs = last_block, so a small file that starts and ends in same sector, bcc = big_file, all done
			tya
			clc
			adc <.blocks + 0
			bcc .first_block_small			;XXX TODO bcc would also suffice? Can this really overflow if we end in same block?
.is_not_first_block
			bcc .full_block				;a is !0
.last_block
			ldy <.last_block_size
.full_block
			tya
.first_block_small
			eor #$ff
.set_positions
			tax
.first_block_big
			dex					;a bit anoying, but SP is SP--/++SP on push/pull
			txs					;set up position in stack, from where we transfer data
			;sta .send_start
			;sty <.send_end

			;full block:
			;send_start = 0					-> $ff ^ $ff
			;send_end = full_block_size			-> $ff

			;last block
			;send_start = 0 - 1 - last_block_size		-> last_block_size ^ $ff
			;send_end = last_block_size			-> last_block_size

			;first block till end of block
			;send_start = 0					-> 0
			;send_end = first_block_size			-> first_block_size

			;first block small
			;sned_start = block_pos - 1 - first_block_size	-> (first_block_size + block_pos) ^ $ff
			;send_end = first_block_size			-> first_block_size	(is position of end within block then)

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP HEADERDATA FOR BLOCK TO BE SENT
			;
			;----------------------------------------------------------------------------------------------------
.preamble							;y = blocksize
			sty <.block_size
			iny					;set up num of bytes to be transferred
			sty <.preamble_data + 0			;used also as send_end on data_send by being decremented again

			ldy <.dir_entry_num

!if CONFIG_DECOMP = 1 {						;no barriers needed with standalone loadraw
			ldx #$14				;walk through list of sectors to be loaded
			lda <.index
.min_loop
			cmp <.wanted,x				;compare
			bcc .is_bigger				;bigger index, next please
			lda <.wanted,x				;smaller (or same, but can't happen, as index is unique) remember new minimum
			beq .barr_zero
.is_bigger
			dex					;next entry
			bpl .min_loop

			;tay
			;dey
			;tya
								;we need to at least wait with setting barrier until first block is loaded, as load-address comes with this block, barrier check on resident side must fail until then by letting barrier set to 0
			clc
			adc .dir_load_addr + 1,y		;add load address highbyte to lowest blockindex
.barr_zero
			sta <.preamble_data + 3			;barrier, zero until set for first time, maybe rearrange and put to end?
}
			lda .dir_load_addr + 0,y		;fetch load address lowbyte
			sec					;XXX TODO could be saved then? Nope, crashes on cebit'18 bootloader

			ldx <.block_num				;first block? -> send load address, neutralize sbc later on, carry is set
			jmp .preamble_

.directory

!ifdef .second_pass {
	!warn $0800 - *, " bytes remaining for drivecode and directory."
	!warn "directory @ ", *
}
;
;!if * > .tables {
;	!set .junk_start = *
;} else {
;	!set .junk_start = .tables
;}
;
;
;			* = .junk_start
;			;use remaining space with code or fill up with junk
;			!for .x, 0, $20 - <.junk_start {
;				!byte ((.x & $f) << 4) + (.x & $f xor $f)
;			}

}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start

;XXX TODO merge high and lownibbles in one table and separate by and #$0f, possible if we save data due to that and also cycles

;11111000 table fits into zp if compressed with asr #$f8, preamble then starts @ $89, zero bytes free then, but fits

;halfstep, send_data, timer elapsed? else wait rest, halfstep, wait
;XXX TODO
;XXX TODO change interleave depending on track (via set max_sectors?)

;XXX TODO optimize code size on stepping
;XXX TODO optimze eof detection?

;turn disc, make it send a single byte to ack? -> wait block ready, receive a byte and then floppy goes idle?




;11111222

;22333334

;44445555

;56666677
;77788888
.bootstrap_start
!pseudopc .bootstrap {
.bootstrap_run
			;this bootstrap will upload code from $0000-$06ff, and the bootstrap @ $0700 will be overwritten when dir-sector is read later on
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
			lda $0503
			sta .fn + 1				;remember diskside#

			lda #%01111010				;DDR set bits for drivenumber to 0, ATN out, CLK out and DATA out are outputs
			sta $1802

			;ACR
			lda #%00000001				;shift-register disabled, PB disable latching, PA enable latching (content for $1c01 is then latched)
			sta $1c0b

			;PCR					 111                            0        111                                  0
			lda #%11101110				;CB2 manual output high (read), CB1 low, CA2 manual output high (byte ready), CA1 low (NC)
			sta $1c0c

			lda #$00				;clear lower part of counter
			sta $1c08
			sta $1c04

			lda #$7f				;disable all interrupts
			sta $180e
			sta $1c0e
			lda $180d				;clear all IRQ flags to ack possibly pending IRQs
			lda $1c0d

			lda #$c0				;enable timer 1 flag
			sta $1c0e

			;cli					;now it is save to allow interrupts again, as they won't happen anymore, okay, it is a lie, timer irqs would happen, but we keep sei

			ldy #$00

			ldx #.BUSY				;signal that we are ready for transfer
			stx $1800

			bit $1800
			bpl *-3

			sty $1800				;clear all lines and set bit 7 as bit counter, to allow data in and clk in to be set/cleared by host
			ldx $1800
.get_block
			lda #$80
-
			cpx $1800				;did a bit arrive? (bit flip in data in, atn is dropped in same go in first bit)
			beq -
			ldx $1800				;load register
			bmi .done
			cpx #$04				;push bit into carry
			ror					;shift in
			bcc -					;do until our counter bit ends up in carry
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
			bit $1800				;no need to, check is done by get_byte
			bmi *-3

.fn			lda #$00				;load directory
			jmp .load_file
}

.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

!ifdef .second_pass {
	!warn "bootstrap size: ", .bootstrap_size
}

.second_pass
