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

;XXX TODO implement readahead, before going to idle but with eof already internally set, force read of last sector again?
;XXX TODO store track as track * 2, so we can detect with a simple lsr if we are on a half track? Do check before sending? if so do halfstep? make all this postponed xfer simpler?

;config params
.SANCHECK_TRAILING_ZERO = 1   ;check if 4 bits of 0 follow up the checksum. This might fail or lead into partially hanging floppy due to massive rereads.
.BOGUS_READS		= 0   ;XXX TODO reset bogus counter only, when motor spins down, so set on init? And on spin down? but do not miss incoming bits!1! number of discarded successfully read sectors on spinup
.POSTPONED_XFER		= 0   ;postpone xfer of block until first halfstep to cover settle time for head transport, turns out to load slower in the end?
.DELAY_SPIN_DOWN	= 1   ;wait for app. 4s until spin down in idle mode
.INTERLEAVE		= 4
.GCR_125		= 1

;constants
.STEPPING_SPEED		= $18
.STEPPING_SETTLE	= $08
.CHECKSUM_CONST1	= $05					;%00000101 -> 4 times 01010
.CHECKSUM_CONST2	= $29					;%00101001
.CHECKSUM_CONST3	= $4a					;%01001010
.EOR_VAL		= $7f
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
.zeropage		= $0000
.drivecode		= $0200
.bootstrap		= $0700
.cache			= $0700

			;XXX TODO allocate this dynamically at end of code and before tables? but bootstrap needs to be in stack then and page needs to be skipped upon upload?
.dir_load_addr		= .directory + 4
.dir_file_size		= .directory + 6
.dir_first_file_track	= .directory + 0			;starttrack of first file in dir
.dir_first_file_index	= .directory + 1			;how many blocks are used on this track up to the file
.dir_first_block_pos	= .directory + 2			;startposition within block
.dir_diskside		= .directory + 3
								;with those three values, the absolute position on disk is represented
.drivecode_start
!pseudopc .zeropage {
.zp_start

;free			= .zp_start + $00
.max_sectors		= .zp_start + $08			;maximum sectors on current track
.dir_sector		= .zp_start + $10
.blocks_on_list		= .zp_start + $11			;blocks tagged on wanted list
.filenum		= .zp_start + $18			;needs to be $18 for all means, as $18 is used as opcode clc /!\
.en_dis_seek		= .zp_start + $19
.ser2bin		= .zp_start + $30			;$30,$31,$38,$39
.blocks 		= .zp_start + $28			;2 bytes
.wanted			= .zp_start + $3e			;21 bytes
.index			= .zp_start + $54			;current blockindex
.track			= .zp_start + $56			;DT ;current track
.val07ff		= .zp_start + $57			;DT ;current track
.to_track		= .zp_start + $58			;DT
.sector			= .zp_start + $59			;DS
.valff			= .zp_start + $5a			;DT
.preamble_data		= .zp_start + $60
!if .BOGUS_READS > 0 {
.bogus_reads		= .zp_start + $66
}
.block_size		= .zp_start + $68
.cache_limit		= .zp_start + $69
.is_cached_sector	= .zp_start + $6a
.is_loaded_sector	= .zp_start + $6c
.first_block_size	= .zp_start + $6e
.val58			= .zp_start + $70
.filename		= .zp_start + $71
.last_block_num		= .zp_start + $72
.last_block_size	= .zp_start + $74
.val0c4c		= .zp_start + $75			;and $78!
.first_block_pos	= .zp_start + $76
.block_num		= .zp_start + $79
.dir_entry_num		= .zp_start + $7a
.last_track_of_file	= .zp_start + $7c

.DT			= 18					;dir_track
.DS			= 18					;dir_sector
___			= $ff
.S0			= $00 xor .EOR_VAL			;ser2bin value 0/9
.S1			= $09 xor .EOR_VAL			;ser2bin value 1/8

.tab00005555_hi		= .zp_start + $00
.tab00333330_hi		= .zp_start + $00
.tab05666660_lo		= .zp_start + $01
.tab00700077_hi		= .zp_start + $00			;XXX currently not used

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
                        !byte .DS, ___, $f0, $1e, $70, $1f, $60, $17, ___, $80, $b0, $1a, $30, $1b, $20, $13	;10
                        !byte ___, $20, $00, $80, $50, $1d, $40, $15, ___, ___, $80, $10, $10, $19, $00, $11	;20
                        !byte .S0, .S1, $e0, $16, $d0, $1c, $c0, $14, .S1, .S0, $a0, $12, $90, $18, ___, ___	;30
                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___	;40
                        !byte ___, ___, ___, $0e, ___, $0f, .DT, $07, .DT, ___, $ff, $0a, ___, $0b, ___, $03	;50
                        !byte ___, ___, ___, ___, ___, $0d, ___, $05, ___, ___, ___, $00, ___, $09, ___, $01	;60
                        !byte $58, ___, ___, $06, ___, $0c, ___, $04, $4c, ___, ___, $02, ___, $08		;70


			;XXX TODO, move PA to $68, need to fix table afterwards by setting $00? but could the do lda $68,y? but still not possible with 8 bit addressing, but would make setup easier

			;XXX TODO /!\ if making changes to gcr_read_loop also the partly decoding in read_sector should be double-checked, same goes for timing changes
			;XXX see if we can use bit 2 from original data, would save space in tables

;this reflects the perfect timing, it is most likely delayed by 2-3 cycles due to bvs branching before?
;in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the end and then there's up to 5 cycles delay (branch + fallthrough)

;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          2222222222222222222222222222222233333333333333333333333333333333444444444444444444444444444444445555555555555555555555555555555511111111111111111111111111111111
;              2                       ccccccccccc   3                   ggggggggggggggggggg   4ggggg                   ccccccc   5             v      1         bbbbbbbbbbb
;1          222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555111111111111111111111111111111
;              2                       ccccccccccc   3                   ggggggggggg   4ggggg                   ccccccc   5             v      1         bbbbbbbbb
;2          22222222222222222222222222223333333333333333333333333333444444444444444444444444444455555555555555555555555555551111111111111111111111111111
;              2                       ccccccccccc   3                   ggg   4ggggg                   ccccccc   5             v      1         bbbbbbb
;3          2222222222222222222222222233333333333333333333333333444444444444444444444444445555555555555555555555555511111111111111111111111111
;              2                       ccccccccccc   3                      4                   ccccccc   5             v      1         bbbbb
;b = bvc *
;c = checksum
;v = v-flag clear
;g = gcr slowdown

.read_loop
			eor $0101,x
			eor $0103,x
.gcr_entry
			sta <.chksum2 + 1
			lda $1c01				;44445555		third read
			ldx #$0f
			sax <.fives + 1

			arr #$f0
			tay					;44444---
!if .GCR_125 = 0 {
			ldx #$03
}
.threes			lda <.tab00333330_hi			;ZP!
			eor .tab44444000_lo,y
			pha					;$0103

.gcr_slow1		lda $1c01				;56666677		fourth read
			sax <.sevens + 1			;----6677
			asr #$fc				;-566666-
			tax

.fives			lda <.tab00005555_hi			;ZP!
			adc <.tab05666660_lo,x			;ZP but index
			pha					;$0102
.chksum			eor #$00				;103,101,100
.chksum2		eor #$00
			sta <.chksum + 1

			lax $1c01				;77788888	fifth read
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
.val3e = * + 1
			ldx #$3e

			bvc *

			lda $1c01				;22333334
			sax <.threes + 1
			asr #$c1
			tax
			lda .tab11111000_hi,y
.twos			eor .tab02200222_lo,x
			tsx
			pha					;$0100
			bne .read_loop
.gcr_end
			;Z-Flag = 1 on success, 0 on failure (wrong type)
			eor $0103				;do remaining checksum, yet same for header or sector
			eor <.chksum + 1
.gcr_h_or_s		jmp .back_read_sector			;will either happen or disabled for fall through
			bne .read_header			;header checksum check failed? reread

			ldy #$01
-
			sta <.is_loaded_sector			;cleared on first round, but correct value will be set on next round
			lda $0105,y				;read in sector and track from header
			ldx #$09
			sbx #$00
			eor <.ser2bin,x
			dey					;Y = 0 or 1
			bpl -					;first byte being processed?
			ldy #$55				;type (sector) (SP = $ff already)
			eor <.track				;second byte is track
			beq +
.read_header
			jmp .next_sector			;type (header)
+
			jmp .read_sector

!if .GCR_125 = 1 {
.slow_table
			!byte <(.slow0 - .slow6) + 2
			!byte <(.slow2 - .slow6) + 2
			!byte <(.slow4 - .slow6) + 2
			!byte <(.slow6 - .slow6) + 2
}

!ifdef .second_pass {
	!warn $0100 - *, " bytes remaining in zeropage."
}

!if >*-1 != >.read_loop { !error "read_sector not in one page: ", .read_loop, " - ", * }

!align 255,0
}
!pseudopc .drivecode {
.tables

;tables with possible offsets
.tab11111000_hi		= .tables + $00
.tab44444000_lo 	= .tables + $04
.tab7d788888_lo		= .tables + $00

;table wth no offset
.tab02200222_lo		= .tables + $00

!if CONFIG_DECOMP = 0 {
			!fill 21,$ea
}								;no barriers needed with standalone loadraw
.preamble_
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
			clc
			lda <.wanted,x				;smaller (or same, but can't happen, as index is unique) remember new minimum
			beq .barr_zero
.is_bigger
			dex					;next entry
			bpl .min_loop
								;we need to at least wait with setting barrier until first block is loaded, as load-address comes with this block, barrier check on resident side must fail until then by letting barrier set to 0
			adc .dir_load_addr + 1,y		;add load address highbyte to lowest blockindex
.barr_zero
			sta <.preamble_data + 3			;barrier, zero until set for first time, maybe rearrange and put to end?
}
			;sbc #$00				;subtract one in case of overflow
			;clc
			lda .dir_load_addr + 0,y		;fetch load address lowbyte
			sec
			bcs .preamble__

	 		* = .tables + $22
.table_start		;combined tables, gaps filled with junk
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

.turn_disc_back
			iny
-
			pla
			ldx #$09
			sbx #$00
			eor <.ser2bin,x				;swap bits 3 and 0
			dey
			sta .directory,y
			bne -
			nop
			dop
			!byte $50
			dec <.blocks_on_list
			dop
			!byte $0d
			lax <.filenum				;just loading a new dir-sector, not requesting turn disk?
			dop
			!byte $40
			inc .en_dis_td
			!byte $05				;ora $0b, does not harm, it is a ora $00
			!byte $0b
			;XXX TODO would also work with dey, needs tya on and + ldx #$fc, would save tsx and end up the same way
			jmp .drivecode_entry
.preamble__
			ldx <.block_num				;first block? -> send load address, neutralize sbc later on, carry is set
			beq ++
			bne +
			nop

                        !byte                                         $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, $47, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08

+
			adc <.first_block_size			;else add first block size as offset, might change carry
			ldx #$80
++
			sta <.preamble_data + 1			;block address low
			jmp +

                        !byte                                         $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
                        !byte $d0, $38, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18
+
			lda <.block_num				;add block num
			adc .dir_load_addr + 1,y		;add load address highbyte
			sta <.preamble_data + 2			;block address high
			bcc .preamble___

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
			lda ($13),y
			jmp .gcr_slow1_20
!if .GCR_125 = 1 {
.tab0070dd77_hi
                        !byte                          $b0, $80, $a0, ___, $b0, $80, $a0, ___, $b0, $80, $a0
}
.gcr_slow1_20
			lda ($04,x)			;lda ($13) = $701f
			nop
.gcr_slow1_40
.slow6			lda $1c01
			nop
			jmp .gcr_slow1 + 3

.slow0			jmp .gcr_slow1_00
.slow2			jmp .gcr_slow1_20
.slow4			jmp .gcr_slow1_40

			;would also suit at $91, $95, $99
!if .GCR_125 = 1 {
.bitrate
                        !byte                $60, $40, $20, $00, $80, ___, $20, $00, $80, ___, $20, $00, $80
			                     ;|bitrate|bitrate + table combined ->
} else {
.bitrate		!byte $60, $40, $20, $00
.slow_table
			!byte <(.slow0 - .slow6) + 2
			!byte <(.slow2 - .slow6) + 2
			!byte <(.slow4 - .slow6) + 2
			!byte <(.slow6 - .slow6) + 2
}
			;----------------------------------------------------------------------------------------------------
			;
			; SEND PREAMBLE AND DATA
			;
			;----------------------------------------------------------------------------------------------------

.preamble___
			stx <.preamble_data + 3 + CONFIG_DECOMP	;ack/status to set load addr, signal block ready
			;clc					;should never overrun, or we would wrap @ $ffff?
!if .POSTPONED_XFER = 1 {
			;ldy <.blocks_on_list
			dec <.blocks_on_list			;nope, so check for last block on track (step will happen afterwards)?
			bpl .start_send
			bit <.last_track_of_file		;eof?
			bpl .postpone
}
.start_send
			ldy #$03 + CONFIG_DECOMP + 1		;num of preamble bytes to xfer. With or without barrier, depending on stand-alone loader or not
-
			lax <.preamble_data - 1,y
			ldx #$09				;masking value
			sbx #$00				;scramble byte while sending, enough time to do so, preamble is called via jsr, so plenty of time between sent bytes
			eor <.ser2bin,x				;swap bits 3 and 0 if they differ, table is 4 bytes only
			sta <.preamble_data - 1,y		;meh, 16 bit
			dey
			bne -

			ldx #$09				;XXX TODO meh, just the value we loaded before :-( masking value for later sax $1800 and for preamble encoding
			lda #.preloop - .branch - 2		;be sure branch points to preloop
			top
.send_sector_data_setup
			ldy #$68				;place mnemonic pla in highbyte
			sta .branch + 1
			sty .sendloop
			ldy <.block_size			;blocksize + 1
			inx
			bcc .sendloop				;send data or preamble?

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

!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}
!if >*-1 != >.sendloop {
	!error "sendloop-size changed, .sendloop - .branch - 2 is != $d1: ", .sendloop - .branch - 2
}
								;keep code here small to not waste much time until busy flag is set after sending
			lda #.sendloop - .branch - 2		;redirect branch to sendloop (A = $d1)
			cpx #$0b				;check on second round, clear carry by that
			bcc .send_sector_data_setup		;second round, send sector data now

			;asl					;a == $d1 -> ATNA out is set as busy flag, same as data out, all other bits are on input bits
			;lda #.BUSY				;8 cycles until poll, busy needs to be set asap
			bit $1800
			bmi *-3
			sta $1800				;signal busy after atn drops

			ldx #$ff
			txs
			;----------------------------------------------------------------------------------------------------
			;
			; BLOCK OF TRACK LOADED AND SENT
			;
			;----------------------------------------------------------------------------------------------------

!if .POSTPONED_XFER = 1 {
			bit <.en_dis_seek
			bmi +
			inx					;we return with X = $ff from send -> X = 0
			inc <.en_dis_seek			;disable_jmp
			jmp .send_back
+
			lda <.blocks_on_list			;just read again, was decreased before
} else {
			dec <.blocks_on_list			;decrease block count, last block on wishlist?
}
			bmi .track_finished
			;XXX TODO, exists twice, and so expensive, as a branch would suffice :-(
			jmp .next_sector			;nope, continue loading
!if .POSTPONED_XFER = 1 {
.postpone
			dec <.en_dis_seek			;enable jmp, skip send of data for now
}
.en_dis_seek_							;XXX TODO if entered here, Y != $ff :-(
			;set stepping speed to $0c, if we loop once, set it to $18
			;XXX TODO can we always do first halfstep with $0c as timerval? and then switch to $18?
			lda #.DIR_TRACK
-
;!if .POSTPONED_XFER = 1 {
			sec					;set by send_block and also set if beq
;}
			isc <.to_track
			beq -					;skip dirtrack however

			jmp .load_track
.track_finished
			;XXX TODO make this check easier? only done here?
			bit <.last_track_of_file		;EOF
			bpl .en_dis_seek_

			inc <.filenum				;autoinc always, so thet load_next will also load next file after a load with filenum
			top

			;----------------------------------------------------------------------------------------------------
			;
			; ENTRY POINT OF IDLE LOOP
			;
			;----------------------------------------------------------------------------------------------------
.idle_
			sta <.filenum				;filenum will be $ff and autoinced later to be zero
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
			bpl +
			sty $1c05				;clears irq flag in $1c0d
			dey
			bne +
			stx $1c00				;turn off motor
+
.rechk
			bit $1800				;check for clk toggle
			bmi .wait_bit1
			beq .wait_bit1
}
.got_bit1
			lda $1800				;now read again
			;bmi .lock				;check for lock
			lsr
			ror <.filename

			lda #$04
.wait_bit2
			bit $1800
			bmi .lock
			bne .wait_bit2				;do we have clk == 0?
			lsr $1800				;XXX TODO can do lsr $1800 here
			;bmi .lock
			;lsr					;all sane, we can read bit in data
			ror <.filename
			bcc .bitloop				;more bits to fetch?

			lsr					;lda #.BUSY
			sta $1800				;set busy bit
			lda <.filename

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD FILE / EXECUTE COMMAND, $00..$7f, $ef, $f0..$fe, $ff
			; XXX TODO files up to $ee would be possible in theory, but more dir sectors would be needed
			;
			;----------------------------------------------------------------------------------------------------

			beq .reset				;if filename is $00, we reset, as we need to eor #$ff the filename anyway, we can check prior to eor $ff
			eor #$ff				;invert bits, saves a byte in resident code, space is more restricted there, so okay
.drivecode_entry
			;top
			;lda <.filenum
			cmp #BITFIRE_LOAD_NEXT			;carry clear = load normal file, carry set = request disk
			beq .clc				;clear carry and skip sta <.filenum by that, filenum = $18 == clc
.clc = * + 1
			sta <.filenum				;set new filenum
			lda #.MOTOR_ON				;always turn motor on
			ora $1c00
			bcs +
			ora #.LED_ON				;only turn LED on if file is loaded
+
			sta $1c00
			lax <.filenum				;load filenum
			bcc .load_file

			;----------------------------------------------------------------------------------------------------
			;
			; TURN DISC / LOAD NEW DIR SECTOR
			;
			;----------------------------------------------------------------------------------------------------
.turn_disc
			eor .dir_diskside			;compare side info
			beq .idle_
			ldy #.DIR_SECT				;first dir sector
.load_dir_sect
			ldx #.DIR_TRACK				;set target track to 18
			stx <.to_track
			tya
			sta <.dir_sector
			dec .en_dis_td				;enable jump back
			ldy #$00
			sty <.blocks + 1
			sty <.is_cached_sector			;invalidate cached sector
			beq .turn_disc_entry			;BRA a = sector, x = 0 = index
.reset			jmp (.reset_drive)

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD A FILE
			;
			;----------------------------------------------------------------------------------------------------
.load_file
!if .BOGUS_READS > 0 {
			ldy #.BOGUS_READS
			sty .bogus_reads
}
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
			;iny
			;iny
			;iny
			;iny
			txa
			sbx #-4					;start with x = 0 by this
			;XXX TODO better do sum up all filesizes with 24 bit and then subtract sectors until block + 1 and block + 2 is reached?
			lda <.blocks + 0
			cpx <.dir_entry_num
			beq .found_file

			sec
			adc .dir_file_size + 0,x
			sta <.blocks + 0

			;XXX TODO on very huge files, this could fail, as we overflow!!! but then again, files are max $d000 in size due to i/o limitation?
			lda <.blocks + 1
			adc .dir_file_size + 1,x
.next_track
			sta <.blocks + 1
			jsr .set_max_sectors			;setup max_sectors, expects track in Y, returns with carry set
			lda <.blocks + 1

			sbc <.max_sectors			;reduce blockcount track by track
			bcc .no_next_track
-
			;skip dir_track				;advance track
			iny
			cpy #.DIR_TRACK
			beq -
			bne .next_track
.found_file
			;store track
			sty <.to_track

			;calc first block size
			eor #$ff
			sta <.first_block_size

			;set max_sectors for track in Y
			jsr .set_max_sectors			;setup max_sectors, expects track in Y, max_sectors might be unset for current to_track

			lda .dir_file_size + 0,x
			cmp <.first_block_size
			ldy .dir_file_size + 1,x		;load file_size + 1
			bne .is_big				;filesize < $100 ? is_big if not
			bcs .is_big				;it is < $0100 but does not fit in remaining space? -> is_big
			sta <.first_block_size			;fits in, correct size, this will cause overflow on next subtraction and last_block_num will be zero, last_block_size will be $ff
.is_big
			clc
			sbc <.first_block_size
			sta <.last_block_size

			bcc +					;file_size + 1
			iny
+
			lxa #$00
.turn_disc_entry
			sta <.sector
			sty <.last_block_num
			stx <.index				;reset block index, x = 0 (if done via turn_disc, it doesn't matter, as no block is transferred, it will just be set to 18 or 17)
.load_track
			;----------------------------------------------------------------------------------------------------
			;
			; SEEK
			;
			;----------------------------------------------------------------------------------------------------
.seek
			ldy #$00				;make stepping positive
			lax <.to_track
			sec
			sbc <.track				;how many tracks to go?
			stx <.track				;save target track as current track

			bcs .seek_up				;up or downwards?
.seek_down
			eor #$ff				;down we go, so invert num tracks as it is negative
			adc #$01
			iny					;but make stepping negative
.seek_up
			asl					;counter is twice the number of tracks (halftracks)
			tax
			bpl .seek_check				;this is a BRA
.step
			lda #.STEPPING_SPEED
.step_
			sta $1c05				;clears irq flag in $1c0d
			tya
.halftrack
			eor $1c00
			and #$01				;same as and #3 afterwards + clc after rol
			sec
			rol
			eor $1c00
			sta $1c00

			;XXX TODO postpone after second halfstep, then while waiting
!if .POSTPONED_XFER = 1 {
			txa
			adc <.en_dis_seek			;$7f/$80
			bmi .send_back				;nope, continue
			jmp .start_send				;send data now
.send_back
}
			lda $1c0d				;wait for timer to elapse, just in case xfer does not take enough cycles (can be 1-256 bytes)
			bpl *-3
			sta <.is_cached_sector
.seek_check
			dex
			bpl .step
.seek_done
			ldy <.track				;already part of set_bitrate -> load track

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP BITRATE, MODIFY GCR LOOP IN ZEROPAGE
			;
			;----------------------------------------------------------------------------------------------------
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

			inx
			beq .set_bitrate			;check on X == $ff? and preserve carry
			dex
			rts
.set_bitrate
			tax
			lda $1c00
			ora #$60
			eor .bitrate,x
			sta $1c00

			ldy .slow_table,x
			ldx #$02
-
			lda .slow6,y
			sta <.gcr_slow1,x
			dey
			txa					;A = 0 after exit of loop
			dex
			bpl -

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD ALL NEEDED SECTORS OF A TRACK AS MARKED ON WANTED-LIST
			;
			;----------------------------------------------------------------------------------------------------

			;A = 0
			clc
								;XXX TODO could also use anc #0 here
.wanted_loop
			ldy <.blocks + 1
			bne +
			lax <.sector
			ldy <.index				;get index
			sty <.wanted,x				;write index into wantedlist
			inc <.blocks_on_list			;count number of blocks in list (per track num of blocks)
			cpy <.last_block_num			;inc index and check if index > file_size + 1 -> EOF
			bcs .load_wanted_blocks			;yep, EOF, carry is set
			inc <.index				;keep index as low as possible, so that literal blob gets loaded with lz_next_page in any case and over more than one page
			top
+
			dec <.blocks + 1
			adc #.INTERLEAVE
			cmp <.max_sectors			;wrap around?
			bcc +					;nope, store sector and do a BRA, bcc will always be bne
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
+
			sta <.sector				;start next track with sector = 0
			bne .wanted_loop			;if zero, done
								;carry is 0
								;just track is full
.load_wanted_blocks						;read and transfer all blocks on wishlist
			ror <.last_track_of_file		;shift in carry for later check, easiest way to preserve eof-state, if carry is set, we reached EOF
			ldx <.is_cached_sector
			bmi .next_sector			;nothing cached yet
			;maybe not necessary, but with random access loading?
			;ldy <.wanted,x				;grab index from list (A with index reused later on after this call)
			;iny					;is it part of our yet loaded file?
			;beq .next_sector			;something went wrong, seems like we loaded another file
.restore
			stx <.is_loaded_sector
-
			tsx
			lda .cache,x
			pha
			txa
			bne -
.new_sector
			ldx <.is_loaded_sector			;initially $ff
			;bmi .next_sector			;initial call on a new track? Load content first
			ldy <.wanted,x				;grab index from list
			sty <.block_num
			cpy <.last_block_num			;current block is last block on list? comparision sets carry and is needed later on on setup_send
			bne .no_caching				;nope, do not cache this sector
.stow
-
			pla
			tsx
			sta .cache,x
			inx
			bne -
			ldx <.is_loaded_sector
			stx <.is_cached_sector
.no_caching
			;cpy <.last_block_num			;compare once when code-path is still common, carry is not tainted until needed
			iny
			beq .next_sector			;if block index is $ff, we reread, as block is not wanted then

			ldy #$ff				;blocksize full sector ($ff) /!\ reused later on for calculations!
			sty <.wanted,x				;clear entry in wanted list

.en_dis_td		eor .turn_disc_back			;can be disabled and we continue with send_data, else we are done here already

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP SEND LOOP, AND DECIDE BLOCK SIZES FIRST
			;
			;----------------------------------------------------------------------------------------------------
.setup_send
			ldx <.block_num
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
			tax
.first_block_big
			dex					;a bit anoying, but SP is SP--/++SP on push/pull
			txs
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
			jmp .preamble_

			;----------------------------------------------------------------------------------------------------
			;
			; READ A SECTOR WITH HEADER AND DO VARIOUS SANITY CHECKS ON IT (HEADER ALREADY ON STACK)
			;
			;----------------------------------------------------------------------------------------------------
.back_read_sector
			;read happens earlier than needed, 2 cycles over all! as branch is not taken and eor is only 3 cycles
			nop
			ldx $1c01				;44445555
			eor $0101
			sta <.chksum2 + 1			;checksum 2 bytes from last round
			txa

			arr #$f0
			tay					;44444---

!if .SANCHECK_TRAILING_ZERO = 1 {
			lda #$0f
			sbx #.CHECKSUM_CONST1			;4 bits of a trailing zero after checksum
			bne .next_sector			;check remaining nibble if it is $05
;			ldx $1c01
;			clv					;after arr, as it influences v-flag
;			bvc *
;			cpx #.CHECKSUM_CONST2			;0 01010 01 - more traiing zeroes
;			bne .next_sector
;			lda $1c01
;			and #$e0
;			cmp #.CHECKSUM_CONST3 & $e0		;010 xxxxx - and more trailing zeroes, last nibble varies on real hardware
;			bne .next_sector
}
			ldx <.threes + 1
			lda <.tab00333330_hi,x			;sector checksum
			eor .tab44444000_lo,y
			eor <.chksum2 + 1
!if .BOGUS_READS > 0 {
			bne .next_sector
			lda .bogus_reads
			beq .new_sector
			dec .bogus_reads
} else {
			beq .new_sector
}
			;----------------------------------------------------------------------------------------------------
			;
			; HEADER DONE, NOW READ SECTOR DATA
			;
			;----------------------------------------------------------------------------------------------------
.next_sector
			ldy #$52				;type (header)
			;jmp .read_header
.read_sector
			ldx <.val07ff - $52,y
			txs					;bytes to read
			lax <.val0c4c - $52,y			;setup A ($0c/$4c)
-
			bit $1c00				;wait for start of sync
			bpl -
-
			bit $1c00				;wait for end of sync
			bmi -

			bit $1c01				;sync mark -> $ff
			clv

			bvc *

			clv
			cpy $1c01				;11111222
			;lsr
			;ror
			;lsr
			;eor #$06

			bvc *
			bne .next_sector			;start over with a new header again as check against first bits of headertype already fails
			sta <.gcr_h_or_s			;setup return jump
			asl
			eor $1c01				;read 22333334 - 2 most significant bits should be zero now
			ldx <.val3e				;set x to $3e and waste a cycle
			eor <.val58 - $3e,x			;eor #$58 to compensate for the unsued bits of eor #$4c * 2 and waste 2 cycles
			sax <.threes + 1
			asr #$c1				;shift out LSB and mask two most significant bits (should be zero)
			bne .next_sector			;start over with a new header again as teh check for header type failed in all bits
			sta <.chksum + 1 - $3e,x		;waste a cycle
			lda <.ser2bin; - $3e,x			;lda #.EOR_VAL and waste 1 cycle
			jmp .gcr_entry				;36 cycles until entry

.directory

!ifdef .second_pass {
	!warn $0800 - *, " bytes remaining for drivecode, cache and directory."
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
!if .DIR_SECT != .DIR_TRACK {
			lda #.DIR_SECT
}
			sta $0b

			;fetch first dir sect and by that position head at track 18 to have a relyable start point for stepping
			ldx #$80
			stx $02
.poll_job		bit $02
			bmi .poll_job
			;ends up at $0500?

			;motor and LED is on after that

			sei
			lda $0503
			pha

			;$180e .. 1800
			;$1c0e .. 1800

			lda #%01111010				;DDR set bits for drivenumber to 0, ATN out, CLK out and DATA out are outputs
			sta $1802

			;PCR					 111                            0        111                                  0
			lda #%11101110				;CB2 manual output high (read), CB1 low, CA2 manual output high (byte ready), CA1 low (NC)
			sta $1c0c

			;ACR
			lda #%00000001				;shift-register disabled, PB disable latching, PA enable latching (content for $1c01 is then latched)
			sta $1c0b

			ldy #$00				;clear lower part of counter
			sty $1c08
			sty $1c04

			dex					;disable all interrupts
			stx $180e
			stx $1c0e
			ldx $180d				;clear all IRQ flags to ack possibly pending IRQs
			ldx $1c0d

			ldx #$c0				;enable timer 1 flag
			stx $1c0e

			;cli					;now it is save to allow interrupts again, as they won't happen anymore, okay, it is a lie, timer irqs would happen, but we keep sei

			;ldy #$00

			asl;ldx #.BUSY				;signal that we are ready for transfer
			sta $1800

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
			sta .zeropage,y

			iny
			bne .get_block

			lda #$01				;skip stack area
-
			isc .block + 1
			beq -
			bne .get_block
.done
			ldx #.BUSY
			stx $1800

			;wait for atn coming low
			bit $1800				;no need to, check is done by get_byte
			bmi *-3

			pla
			jmp .drivecode_entry
}

.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

!ifdef .second_pass {
	!warn "bootstrap size: ", .bootstrap_size
}

.second_pass
