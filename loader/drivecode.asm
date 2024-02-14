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
.SANCHECK_CYCLES        = 1   ;start a timer when reading sync + whole sector, if this took too short, discard sector, as we are prone to false positives then.
!if CONFIG_MOTOR_ALWAYS_ON = 0 {
.BOGUS_READS		= 0
}
;.POSTPONED_XFER	= 1   ;postpone xfer of block until first halfstep to cover settle time for head transport, turns out to load slower in the end?
.DELAY_SPIN_DOWN	= 0   ;wait for app. 4s until spin down in idle mode
.INTERLEAVE		= 4

;constants
.STEPPING_SPEED		= $18
.EOR_VAL		= $7f
.DIR_SECT		= 18
.DIR_TRACK		= 18
.BUSY			= $02
.LED_OFF		= $f7
.LED_ON			= $08
.MOTOR_OFF		= $fb
.MOTOR_ON		= $04

;adresses
.reset_drive		= $fffc	;eaa0
.zeropage		= $0000
.drivecode		= $0200
.directory		= $0600
.cache			= $0700
.bootstrap		= $0700

.dir_load_addr_lo	= .directory + 4 + (0 * $3f)
.dir_load_addr_hi	= .directory + 4 + (1 * $3f)
.dir_file_size_lo	= .directory + 4 + (2 * $3f)
.dir_file_size_hi	= .directory + 4 + (3 * $3f)
								;with those three values, the absolute position on disk is represented
.dir_first_file_track	= .directory + 0			;starttrack of first file in dir
.dir_first_file_index	= .directory + 1			;how many blocks are used on this track up to the file
.dir_first_block_pos	= .directory + 2			;startposition within block
.dir_diskside		= .directory + 3
.drivecode_start
!pseudopc .zeropage {
.zp_start

.dirinfo		= .zp_start + $00
.to_track		= .zp_start + $00			;DT
.blocks_hi 		= .zp_start + $01
.blocks_lo 		= .zp_start + $02
;.free			= .zp_start + $03
.max_sectors		= .zp_start + $08			;maximum sectors on current track
.dir_sector		= .zp_start + $10
.blocks_on_list		= .zp_start + $11			;blocks tagged on wanted list
.filenum		= .zp_start + $18			;needs to be $18 for all means, as $18 is used as opcode clc /!\
.en_dis_seek		= .zp_start + $19
.next_header__		= .zp_start + $20
.preamble_data_		= .zp_start + $22
.ser2bin		= .zp_start + $30			;$30,$31,$38,$39
;free	 		= .zp_start + $28
;free	 		= .zp_start + $29
.wanted			= .zp_start + $3e			;21 bytes
.index			= .zp_start + $54			;current blockindex
.track			= .zp_start + $56			;DT ;current track
.val07ff		= .zp_start + $57			;DT ;current track
.sector			= .zp_start + $58			;DS
;free	 		= .zp_start + $59
;free	 		= .zp_start + $5c
.preamble_data		= .zp_start + $60
!if .BOGUS_READS != 0 {
.bogus_reads		= .zp_start + $66
}
.block_size		= .zp_start + $68
;free			= .zp_start + $69
.is_cached_sector	= .zp_start + $6a
.is_loaded_sector	= .zp_start + $6c
.first_block_size	= .zp_start + $6e
.filename		= .zp_start + $70
.last_block_num		= .zp_start + $71
.last_block_size	= .zp_start + $72
.first_block_pos	= .zp_start + $74
;free			= .zp_start + $75
.val80f0		= .zp_start + $0f
.block_num		= .zp_start + $79
.dir_entry_num		= .zp_start + $7a
.last_track_of_file	= .zp_start + $7c
.val01			= .zp_start + $6f

.DT			= 18					;dir_track
.DS			= 18					;dir_sector
___			= $ff
.S0			= $00 xor .EOR_VAL			;ser2bin value 0/9
.S1			= $09 xor .EOR_VAL			;ser2bin value 1/8

.P1			= <(.preamble_data - 1)
.P2			= >(.preamble_data - 1)

.N1			= <.next_header
.N2			= >.next_header

.tab00005555_hi		= .zp_start + $00
.tab00333330_hi		= .zp_start + $00
.tab05666660_lo		= .zp_start + $01
;.tab00700077_hi		= .zp_start + $00			;XXX currently not used

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

			;     0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
                        !byte ___, ___, ___, ___, $f0, $60, $b0, $20, ___, $40, $80, $00, $e0, $c0, $a0, $80	;00
                        !byte .DS, ___, $f0, $1e, $70, $1f, $60, $17, ___, $80, $b0, $1a, $30, $1b, $20, $13	;10
                        !byte .N1, .N2, .P1, .P2, $50, $1d, $40, $15, ___, ___, $80, $10, $10, $19, $00, $11	;20
                        !byte .S0, .S1, $e0, $16, $d0, $1c, $c0, $14, .S1, .S0, $a0, $12, $90, $18, ___, ___	;30
                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___	;40
                        !byte ___, ___, ___, $0e, ___, $0f, .DT, $07, ___, ___, $ff, $0a, ___, $0b, ___, $03	;50
                        !byte ___, ___, ___, ___, ___, $0d, $02, $05, ___, ___, ___, $00, ___, $09, ___, $01	;60
                        !byte ___, ___, ___, $06, ___, $0c, ___, $04, ___, ___, ___, $02, ___, $08		;70

			;XXX TODO /!\ if making changes to gcr_read_loop also the partly decoding in read_sector should be double-checked, same goes for timing changes

;this reflects the perfect timing, it is most likely delayed by 2-3 cycles due to bvs branching before?
;in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the end and then there's up to 5 cycles delay (branch + fallthrough)

;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          2222222222222222222222222222222233333333333333333333333333333333444444444444444444444444444444445555555555555555555555555555555511111111111111111111111111111111
;           bbbgggggg   ccccccc   1              2                      ccccccccccc   3                   ggggggggggggggg   4ggg                    5       v    bbbbbbbbbbb
;1          222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555111111111111111111111111111111
;           bbbgggg   ccccccc   1              2                      ccccccccccc   3                   ggggggggg   4ggg                    5       v    bbbbbbbbb
;2          22222222222222222222222222223333333333333333333333333333444444444444444444444444444455555555555555555555555555551111111111111111111111111111
;           bbbgg   ccccccc   1              2                      ccccccccccc   3                   ggg   4ggg                    5       v    bbbbbbb
;3          2222222222222222222222222233333333333333333333333333444444444444444444444444445555555555555555555555555511111111111111111111111111
;           bbb   ccccccc   1              2                      ccccccccccc   3                      4                    5       v    bbbbb
;b = bvc *
;c = checksum
;v = v-flag clear
;g = gcr slowdown

			;22333334 44445555 56666677 77788888 11111222
			;33000000 00001111 11111122 22222222 33333333

			nop
			nop
			nop
.read_loop
			pha					;$0101
.chksum			eor #$00
.chksum2		eor #$00
			sta .chksum + 1

			lda $1c01				;11111222		first read
			ldx #$07
			sax <.twos + 1
			;sbc <.twos + 1 is possible
			and #$f8
			tay
.val3e = * + 1
			ldx #$3e
.v1c01 = * + 1
			lda $1c01				;22333334		second read
			sax <.threes + 1
			asr #$c1
			tax
			lda .tab11111000_hi,y			;offset 0
.twos			eor .tab02200222_lo,x			;offset 0
			tsx
			pha					;$0100
			beq .gcr_end

.v0102			eor $0102,x
.v0103			eor $0103,x
.gcr_entry
			sta <.chksum2 + 1
			lda $1c01				;44445555		third read
			ldx #$0f
			sax <.fives + 1

			arr #$f0
			tay					;44444---
.threes			lda <.tab00333330_hi			;offset 0 - ZP!
.fours			eor .tab44444000_lo,y			;offset 4
			pha					;$0103
.gcr_slow1
			lda $1c01				;56666677		fourth read
			sax <.sevens + 1			;----6677
			asr #$fc				;-566666-
			tax

.fives			lda <.tab00005555_hi			;offset 0 - ZP!
			adc <.tab05666660_lo,x			;offset 1 - ZP but index
			pha					;$0102

			lax $1c01				;77788888	fifth read
			asr #$40
			tay					;XXX TODO can be omitted? can this value be combensated by dd77?
			adc .tab7d788888_lo,x			;offset 0
.sevens			eor .tab0070dd77_hi,y			;offset $20 - clears v-flag, decodes the remaining bits of quintuple 7
.bvs
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
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			jmp .read_loop
.gcr_end
			eor <.chksum + 1,x
			tax
			jmp .back_gcr

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

!if CONFIG_LOADER_ONLY = 1 {
			!fill 21,$ea
}								;no barriers needed with standalone loadraw
.preamble
			sty <.block_size
			iny					;set up num of bytes to be transferred
			sty <.preamble_data + 0			;used also as send_end on data_send by being decremented again

			ldy <.dir_entry_num

!if CONFIG_LOADER_ONLY = 0 {					;no barriers needed with standalone loadraw
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
								;we need to at least wait with setting barrier until first block is loaded, as load-address comes with this block, barrier check on resident side must fail until then by letting barrier set to 0
			clc
			adc .dir_load_addr_hi,y			;add load address highbyte to lowest blockindex
.barr_zero
			sta <.preamble_data + 3			;barrier, zero until set for first time, maybe rearrange and put to end?
}
			lda .dir_load_addr_lo,y			;fetch load address lowbyte
			sec
			bcs .preamble__

	 		* = .tables + $22

.G1			= <.gcr_slow1 + 3
.G2			= >.gcr_slow1 + 3

.table_start		;combined tables, gaps filled with junk
			!byte           $0e, $0a, $f0, $00, $06, $02, $e1, $4e, $4f, $47, $d2, $4a, $4b, $43
.gcr_slow_back		!byte .G1, .G2, $4d, $45, $a5, $40, $49, $41, $96, $46, $4c, $44, $87, $42, $48
.turn_disc_back
			iny
			beq +

			!byte           $0f, $0b, $0d, $09, $0c, $08, $f0, $3e, $3f, $37, $0e, $3a, $3b, $33
			!byte $70, $78, $3d, $35, $0f, $30, $39, $31, $60, $36, $3c, $34, $07, $32, $38
.slow_tab
			!byte (<.gcr_slow1_00) << 1
			!byte (<.gcr_slow1_20) << 1
			!byte (<.gcr_slow1_40) << 1

			!byte           $07, $03, $05, $01, $04, $2e, $b0, $2e, $2f, $27, $0a, $2a, $2b, $23
			!byte $30, $e1, $2d, $25, $0b, $20, $29, $21, $20, $26, $2c, $24, $03, $22, $28

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
+
-
			pla
			ldx #$09
			sbx #$00
			eor <.ser2bin,x				;swap bits 3 and 0
			dey
			sta .directory,y
			bne -
			inc .en_dis_td
			top
			!byte $50				;bvc * + 2, will never be taken?
			!byte $02
			top
			nop
			!byte $0d				;does not harm, ora $eaea
			lax <.filenum
			dop
			!byte $40
			dec <.blocks_on_list
			dop
			!byte $05				;ora $0b, does not harm, it is a ora $00
			;XXX TODO would also work with dey, needs tya on and + ldx #$fc, would save tsx and end up the same way
			jmp .drivecode_entry
.preamble__
			ldx <.block_num				;first block? -> send load address, neutralize sbc later on, carry is set
			beq ++
			ldx #$80
			bne +

                        !byte                                         $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, $47, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08

+
			adc <.first_block_size			;else add first block size as offset, might change carry
++
			sta <.preamble_data + 1			;block address low
			stx <.preamble_data + 4 - CONFIG_LOADER_ONLY	;ack/status to set load addr, signal block ready
			jmp +

                        !byte                                         $e0, $fe, $ff, $f7, $06, $fa, $fb, $f3		;9 bytes
                        !byte $d0, $38, $fd, $f5, $0c, $f0, $f9, $f1, $c0, $f6, $fc, $f4, $04, $f2, $f8
+
			lda <.block_num				;add block num
			adc .dir_load_addr_hi,y			;add load address highbyte
			sta <.preamble_data + 2			;block address high
			;clc					;should never overrun, or we would wrap @ $ffff?
			bcc +

                        !byte                                         $a0, $ee, $ef, $e7, $02, $ea, $eb, $e3		;9 bytes
                        !byte $90, $29, $ed, $e5, $08, $e0, $e9, $e1, $1a, $e6, $ec, $e4, $da, $e2, $e8

;                        !byte $50, ___, ___, ___, $0d, ___, ___, ___, $40, ___, ___, ___, $05, ___, ___, ___		;20 bytes with 3 dops
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $80, $0e, $0f, $07, $00, $0a, $0b, $03
;                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
;                        !byte $d0, ___, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $a0, $0e, $0f, $07, $02, $0a, $0b, $03		;9 bytes
;                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___

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
;tab000bbbbb
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $4e, $4f, $47, ___, $4a, $4b, $43
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $5e, $5f, $57, ___, $5a, $5b, $53 x
;                        !byte ___, ___, $5d, $55, ___, $50, $59, $51, ___, $56, $5c, $54, ___, $52, $58, ___ x
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $4e, $4f, $47, ___, $4a, $4b, $43 x
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___ x
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03
;                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $1e, $1f, $17, ___, $1a, $1b, $13 x
;                        !byte ___, ___, $1d, $15, ___, $10, $19, $11, ___, $16, $1c, $14, ___, $12, $18, ___ x
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03 x
;                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___ x
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

+
.start_send
			ldy #$06 - CONFIG_LOADER_ONLY		;num of preamble bytes to xfer. With or without barrier, depending on stand-alone loader or not

			;----------------------------------------------------------------------------------------------------
			;
			; CODE TO SLOW DOWN GCR LOOP DEPENDING ON BITRATE
			;
			;----------------------------------------------------------------------------------------------------

			bne .start_send_
			nop
.gcr_slow1_00
.gcr_slow1_20	= * + 6
.tab0070dd77_hi                                  ;dop #$b0  dop #$a0  dop #$b0  dop #$a0  dop #$b0  dop #$a0
                        !byte                     $80, $b0, $80, $a0, $80, $b0, $80, $a0, $80, $b0, $80, $a0
.gcr_slow1_40
			lda $1c01
			jmp (.gcr_slow_back)			;waste 5 cycles
-
			;XXX TODO could use lda/sta ($xx),y to save bytes, this way we use 16 bit addresses despite preamble being in ZP
			lda (.preamble_data_),y
			sbx #$00
			eor <.ser2bin,x				;swap bits 3 and 0 if they differ, table is 4 bytes only
			sta (.preamble_data_),y
.start_send_
			ldx #$09				;greatness, just the value we need for masking with sax $1800 and for preamble encoding \o/
			dey
			bne -
			beq +

			;would also suit at $91, $95, $99
.next_header_ = * + 7
                        !byte                          $20, $00, $80, ___, $20, $00, $80, $6c, $20, $00, $80

.bitrate
			!byte $60, $40, $20, $00

			;----------------------------------------------------------------------------------------------------
			;
			; SEND PREAMBLE AND DATA
			;
			;----------------------------------------------------------------------------------------------------
;!if .POSTPONED_XFER = 1 {
;			;ldy <.blocks_on_list
;			dec <.blocks_on_list			;nope, so check for last block on track (step will happen afterwards)?
;			bpl +
;			bit <.last_track_of_file		;eof?
;			bpl .postpone
;}
+

			lda #.preloop - .branch - 2		;be sure branch points to preloop
.send_sector_data_setup
			sta .branch + 1
			sty .sendloop
			ldy <.block_size			;blocksize + 1
			inx					;increase counter, as we go through sendloop twice, for preamble and for data
			bcc .sendloop				;send data or preamble?

			ldy #$04 - CONFIG_LOADER_ONLY		;num of preamble bytes to xfer. With or without barrier, depending on stand-alone loader or not
.preloop
.sendloop = * + 2						;send the data block
			lda <.preamble_data,y
								;just pull from stack instead of lda $0100,y, sadly no tsx can be done, due to x being destroyed

								;our possibilities to send bits:
								;...-0.1.
								;...10.-.

								;...+2.3.
								;...32.-.

								;...-4.5.
								;...54.-.

								;...+6.7.
								;...76.-.

			;XXX TODO also use $1802 to send data by setting to input?
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
			ldy #$68				;place mnemonic pla in highbyte
			lda #.sendloop - .branch - 2		;redirect branch to sendloop (A = $d1)
			cpx #$0b				;check on second round, clear carry by that
			bcc .send_sector_data_setup		;second round, send sector data now

			;asl					;a == $d1 -> ATNA out is set as busy flag, same as data out, all other bits are on input bits
			;lda #.BUSY				;8 cycles until poll, busy needs to be set asap
			bit $1800
			bmi *-3
			sta $1800				;signal busy after atn drops
;-
;			bit $1c0d
;			bpl -

			;----------------------------------------------------------------------------------------------------
			;
			; BLOCK OF TRACK LOADED AND SENT
			;
			;----------------------------------------------------------------------------------------------------

;!if .POSTPONED_XFER = 1 {
;			bit <.en_dis_seek
;			bmi +
;			ldx #$00				;we return with X = $ff from send -> X = 0
;			inc <.en_dis_seek			;disable_jmp
;			jmp .send_back
;+
;			lda <.blocks_on_list			;just read again, was decreased before
;} else {
			dec <.blocks_on_list			;decrease block count, last block on wishlist?
;}
			bpl .next_header_
;!if .POSTPONED_XFER = 1 {
;.postpone
;			dec <.en_dis_seek			;enable jmp, skip send of data for now
;			jmp .next_track
;}
			;XXX TODO make this check easier? only done here?
			bit <.last_track_of_file		;EOF
!if .BOGUS_READS = 1 & .DELAY_SPIN_DOWN = 1 {
			bmi *+5
			jmp .next_track
} else {
			bpl .next_track
}

			inc <.filenum				;autoinc always, so thet load_next will also load next file after a load with filenum
			top

			;----------------------------------------------------------------------------------------------------
			;
			; ENTRY POINT OF IDLE LOOP
			;
			;----------------------------------------------------------------------------------------------------
.idle_
			sta <.filenum
.skip_load
			lax $1c00
!if .DELAY_SPIN_DOWN = 0 & CONFIG_MOTOR_ALWAYS_ON = 0 {
			and #.MOTOR_OFF & .LED_OFF
} else {
			and #.LED_OFF				;for now only turn off LED
}
!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
			sbx #.MOTOR_ON				;prepare value to turn off motor
}
			sta $1c00

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
			lda #$04
.bitloop
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
}
			bit $1800				;check for clk toggle
			bmi .lock
			beq .wait_bit1
.got_bit1
			ldy $1800				;now read again
			cpy #5					;won't destroy A
			ror <.filename
.wait_bit2
			bit $1800
			bmi .lock
			bne .wait_bit2				;do we have clk == 0?

			lsr $1800				;XXX TODO can do lsr $1800 here
			ror <.filename
			bcc .bitloop				;more bits to fetch?

			lsr					;lda #.BUSY
			sta $1800				;set busy bit

!if .BOGUS_READS != 0 {
			;XXX TODO A = 2 -> num reads bis $ff laufen lassen
			;XXX TODO A = 4 -> same val as MOTOR_ON
			asl
			bit $1c00
			bne +
			lsr
			sta <.bogus_reads
+
}
			lda <.filename

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD FILE / EXECUTE COMMAND, $00..$7f, $ef, $f0..$fe, $ff
			; XXX TODO files up to $ee would be possible in theory, but more dir sectors would be needed
			;
			;----------------------------------------------------------------------------------------------------

			beq .reset				;if filename is $00, we reset, as we need to eor #$ff the filename anyway, we can check prior to eor $ff
			eor #$ff				;invert bits, saves a byte in resident code, and makes reset detection easier
.drivecode_entry
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

			;lda #$80
			;sta $1c0

			ldy #.DIR_SECT				;second dir sector
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
.load_dir_sect
			ldx #.DIR_TRACK				;set target track to 18
			stx <.to_track
			sty <.dir_sector
			tya
			dec .en_dis_td				;enable jump back
			ldy #$00
			sty <.blocks_hi
			sty <.is_cached_sector			;invalidate cached sector (must be != DIR_SECT)
			beq .turn_disc_entry			;BRA a = sector, y = 0 = index

			;----------------------------------------------------------------------------------------------------
			;
			; INCREMENT TRACK
			;
			;----------------------------------------------------------------------------------------------------


			;XXX TODO two ways to increment track, can we make this code common?
.next_track
			lda #.DIR_TRACK
;!if .POSTPONED_XFER = 1 {
;			sec					;set by send_block and also set if beq
;}
-
			isc <.to_track
			beq -					;skip dirtrack however
			bne .load_track

.reset			jmp (.reset_drive)

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD A FILE
			;
			;----------------------------------------------------------------------------------------------------
.load_file
			sbx #$3f				;check if filnum >= $3f
			bcc +					;underflow, filenum is < $3f
			txa
			dey					;select second dir sector
+
			cpy <.dir_sector			;is this dir sector loaded?
			bne .load_dir_sect

			sta <.dir_entry_num			;and save

			;----------------------------------------------------------------------------------------------------
			;
			; FIND FILE IN DIRECTORY AND SUM UP FILESIZE TO FIND POSITION ON DISK AND FIRST AND LAST BLOCK SIZE
			;
			;----------------------------------------------------------------------------------------------------

.find_file_in_dir
			ldx #$03
-
			ldy .directory - 1,x			;copy over dir info
			sty <.dirinfo - 1,x			;to track is in Y after copy
			dex
			bne -
.next_dir_entry
			;XXX TODO better do sum up all filesizes with 24 bit and then subtract sectors until block + 1 and block + 2 is reached?
			lda <.blocks_lo
			cpx <.dir_entry_num
			beq .found_file

			sec
			adc .dir_file_size_lo,x
			sta <.blocks_lo

			;XXX TODO on very huge files, this could fail, as we overflow!!! but then again, files are max $d000 in size due to i/o limitation?
			lda <.blocks_hi
			adc .dir_file_size_hi,x

			inx
.inc_track
			sta <.blocks_hi
			jsr .set_max_sectors			;setup max_sectors, expects track in Y, returns with carry set
			lda <.blocks_hi

			sbc <.max_sectors			;reduce blockcount track by track
			bcc .next_dir_entry
-
			;skip dir_track				;advance track
			iny
			cpy #.DIR_TRACK
			beq -
			bne .inc_track
.found_file
			;store track
			sty <.to_track

			;calc first block size
			eor #$ff
			sta <.first_block_size

			lda .dir_file_size_lo,x
			cmp <.first_block_size
			ldy .dir_file_size_hi,x			;load file_size + 1
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

			bcs .seek_up				;up or downwards?
.seek_down
			eor #$ff				;down we go, so invert num tracks as it is negative
			adc #$01
			iny					;but make stepping negative
.seek_up
			stx <.track				;save target track as current track
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
;!if .POSTPONED_XFER = 1 {
;			txa
;			adc <.en_dis_seek			;$7f/$80
;			bmi .send_back				;nope, continue
;			jmp .start_send				;send data now
;.send_back
;}
			lda $1c0d				;wait for timer to elapse, just in case xfer does not take enough cycles (can be 1-256 bytes)
			bpl *-3
			sta <.is_cached_sector			;invalidate chached sector upon track change
.seek_check
			dex
			bpl .step
.seek_done
			txs					;reset stack pointer

			ldy <.track				;already part of set_bitrate -> load track

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP BITRATE, DETERMIN #SECTORS ON TRACK, MODIFY GCR LOOP IN ZEROPAGE
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
.const			= <(.read_loop - .bvs - 3 - 3 - $f*2)

			tay
			sta .bra_slo + 1
			adc #.const
			ldx #$0f*2
-
			sta <.bvs + 1,x
			adc #2
			dex
			dex
			bpl -

			lda $1c00
			ora #$60
			;eor .bitrate - .const - $14,y in case of tay after adc #.const
			eor .bitrate,y
			sta $1c00

			lda .slow_tab,y
			ldy #$4c
			ldx #>.gcr_slow1_00
			lsr
			bcc +					;if carry = 1, A = 1
			lsr					;a = 1
			ldy #$ad
			ldx #$1c
+
			sty <.gcr_slow1 + 0
			sta <.gcr_slow1 + 1
			stx <.gcr_slow1 + 2

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD ALL NEEDED SECTORS OF A TRACK AS MARKED ON WANTED-LIST
			;
			;----------------------------------------------------------------------------------------------------

			;A = 0
			asr #$00
			;clc
								;XXX TODO could also use anc #0 here, but might fail on 1541 U1
.wanted_loop
			ldy <.blocks_hi
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
			dec <.blocks_hi
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
			bmi .next_header			;nothing cached yet
			;maybe not necessary, but with random access loading?
			;ldy <.wanted,x				;grab index from list (A with index reused later on after this call)
			;iny					;is it part of our yet loaded file?
			;beq .next_header			;something went wrong, seems like we loaded another file
.restore
			ldy #$00
-
			lda .cache,y
			pha
			iny
			bne -
			top
.new_sector
			ldx <.is_loaded_sector			;initially $ff
			;bmi .next_header			;initial call on a new track? Load content first
			ldy <.wanted,x				;grab index from list
			sty <.block_num
			cpy <.last_block_num			;current block is last block on list? comparision sets carry and is needed later on on setup_send
			bne .no_caching				;nope, do not cache this sector
.stow
			stx <.is_cached_sector

			ldy #$00
-
			pla
			dey
			sta .cache,y
			bne -

			;ldy <.last_block_num			;restore Y, can be omitted, as it falls through anyway on next beq
.no_caching
			;cpy <.last_block_num			;compare once when code-path is still common, carry is not tainted until needed
			iny
			beq .next_header			;if block index is $ff, we reread, as block is not wanted then

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
			adc <.blocks_lo
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

			jmp .preamble

			;----------------------------------------------------------------------------------------------------
			;
			; READ A SECTOR WITH HEADER AND DO VARIOUS SANITY CHECKS ON IT (HEADER ALREADY ON STACK)
			;
			;----------------------------------------------------------------------------------------------------
.back_gcr
			lda $1c01				;xxxx0101 least significant 4 bits are CONST1/trailing zero in case of sector checksum
!if .SANCHECK_TRAILING_ZERO = 1 {
;			ror					;xxxxx010 1
;			bcc .next_header
;			tay
;			lda .tab4444000_lo - 2,y
			ror					;xxxxx010 1
			bcc .next_header			;check bit 0 of CONST1
			sbc #2					;clear out constant (incl. carry) if anything goes wrong, offset into table will be off and checksum will fail?
			tay
			and #$07
			bne .next_header
} else {
			arr #$f0
			tay
}

			txa
			eor (.fours + 1),y
			eor $0102
			eor $0103
			ldx <.threes + 1
			eor <.tab00333330_hi,x
			bne .next_header			;checksum check failed? reread
.gcr_h_or_s		beq .sector_done			;this is either $80 or $f0 -> dop for header oder beq for sector

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
			beq .read_sector			;always falls through if we come from sector read (negtive is alsways != 0), else it decides if we continue with sector payload or start with a new header
.next_header
			ldy #$52				;type (header)
.read_sector
!if .SANCHECK_CYCLES = 1 {
			ldx .bra_slo + 1
}
-
			bit $1c00				;wait for start of sync
			bmi -

			lda $1c01				;adc $1c01 would work, if we could be sure we return with A < $80 from checksum check or lda $1c00 would be < $7f, but can be $7f if write protect is on
			clv
!if .SANCHECK_CYCLES = 1 {
			;XXX TODO setup 2 vals at once? ldy #$7e inx iny bpl?
                        lda .time_lo_s,x                        ;setup safeguard timers for sane range of rotation speeds
                        sta $1c04
                        lda .time_hi_s,x
                        sta $1c05
                        lda .time_lo_f,x
                        sta $1804
                        lda .time_hi_f,x
                        sta $1805
}
			bvc *					;wait for first byte after sync

			cpy $1c01				;11111222 compare type
			bne .next_header			;start over with a new header again as check against first bits of headertype already fails
			;XXX TODO 90/f0 would result in bcc and check carry as well
			lax <.val80f0 - $52,y			;setup A ($0c/$4c) (lax allows for 8 bit address)
			stx .gcr_h_or_s				;setup return jump either $0c or $4c
			sbc #$30
			and #$c0
			ldx #$3e
.bra_slo		bne +
+
			nop
			nop
			nop
			nop
			eor $1c01
			sax <.threes + 1
			asr #$c1				;shift out LSB and mask two most significant bits (should be zero now depending on type)
			bne .next_header			;start over with a new header again as the check for header type failed in all bits
			sta <.chksum + 1 - $3e,x 		;init checksum, waste 1 cycle
			lda ($03 - $3e,x)
			lda <.ser2bin - $3e,x			;$7f, waste 2 cycles
			ldx <.val07ff - $52,y			;will we receive 8 or 256 bytes
			txs					;bytes to read
			jmp .gcr_entry				;35 cycles until entry, 36 would be better
.sector_done
        !if .SANCHECK_CYCLES = 1 {
                        lda $180d
                        bpl .next_header                        ;read too fast?
                        lda $1c0d				;XXX TODO we could eor here and check eored result
                        bmi .next_header                        ;read too slow?
			jmp .new_sector
        }

!if .SANCHECK_CYCLES = 1 {
.t0_f                   = $ffff - $d7cc ;2833
.t1_f                   = $ffff - $da50 ;25af
.t2_f                   = $ffff - $dcc8 ;2337
.t3_f                   = $ffff - $df49 ;20b6

.t0_s                   = $ffff - $d603 ;29fc (50 / 160 * 160)  ;32*2
.t1_s                   = $ffff - $d8a8 ;2757 (50 / 160 * 150)  ;30*2
.t2_s                   = $ffff - $db3b ;24c4 (50 / 160 * 140)  ;28*2
.t3_s                   = $ffff - $ddd7 ;2228 (50 / 160 * 130)  ;26*2

.time_lo_f
                        !byte <.t0_f, <.t1_f, <.t2_f, <.t3_f
.time_hi_f
                        !byte >.t0_f, >.t1_f, >.t2_f, >.t3_f

.time_lo_s
                        !byte <.t0_s, <.t1_s, <.t2_s, <.t3_s
.time_hi_s
                        !byte >.t0_s, >.t1_s, >.t2_s, >.t3_s
}


;c73a

;			clv
;			bvc *
;			;$40 ;$c0
;			;
;			;asl
;			;
;			ldx #$3e
;			sbc #$30
;			and #$c0
;;reduced by 3 cycles
;			eor (.v1c01 - $3e,x) 			;read 22333334 - 2 most significant bits should be zero now, waste 2 cycles
;			sax <.threes + 1
;			asr #$c1				;shift out LSB and mask two most significant bits (should be zero now depending on type)
;			bne .next_header			;start over with a new header again as the check for header type failed in all bits
;			sta <.chksum + 1			;init checksum
;			lda #.EOR_VAL
;			ldx <.val07ff - $52,y			;will we receive 8 or 256 bytes
;			txs					;bytes to read
;			jmp .gcr_entry				;33 cycles until entry

;!if .BOGUS_READS != 0 {
;			bne .next_header
;			lda <.bogus_reads
;			bmi .new_sector
;			dec <.bogus_reads
;			top
;} else {
;			beq .new_sector
;			top
;}

;			adc #$13
;			asl
;			ldx <.val3e				;set x to $3e and waste 1 cycle
;.bra_slo		bne +
;+
;			nop
;			nop
;			nop
;			eor (.v1c01 - $3e, x)
;			sax <.threes + 1
;			asr #$c1				;shift out LSB and mask two most significant bits (should be zero now depending on type)
;			bne .next_header			;start over with a new header again as the check for header type failed in all bits
;			sta <.chksum + 1 - $3e,x 		;init checksum, waste 1 cycle
;			lda ($03 - $3e,x)
;			lda <.ser2bin - $3e,x			;$7f, waste 2 cycles
;			ldx <.val07ff - $52,y			;will we receive 8 or 256 bytes
;			txs					;bytes to read
;			jmp .gcr_entry				;35 cycles until entry, 36 would be better


;.C:1948  A0 52       LDY #$52
;.C:194a  2C 00 1C    BIT $1C00
;.C:194d  30 FB       BMI $194A
;.C:194f  6D 01 1C    ADC $1C01
;.C:1952  50 FE       BVC $1952
;.C:1954  CC 01 1C    CPY $1C01
;.C:1957  D0 EF       BNE $1948
;.C:1959  B7 23       LAX $23,Y
;.C:195b  86 FA       STX $FA
;.C:195d  CB EC       SBX #$EC
;.C:195f  8A          TXA
;.C:1960  0A          ASL A
;.C:1961  A6 93       LDX $93
;.C:1963  D0 03       BNE $1968
;.C:1965  EA          NOP
;.C:1966  EA          NOP
;.C:1967  EA          NOP
;.C:1968  4D 01 1C    EOR $1C01
;.C:196b  87 B9       SAX $B9
;.C:196d  4B C1       ASR #$C1
;.C:196f  D0 D7       BNE $1948
;.C:1971  85 83       STA $83
;.C:1973  A1 00       LDA ($00,X)
;.C:1975  A5 30       LDA $30
;.C:1977  B6 05       LDX $05,Y
;.C:1979  9A          TXS
;.C:197a  EA          NOP
;.C:197b  4C AC 00    JMP $00AC


!ifdef .second_pass {
	!warn $0800 - *, " bytes remaining for drivecode, cache and directory."
}

}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start

;XXX TODO merge high and lownibbles in one table and separate by and #$0f, possible if we save data due to that and also cycles

;11111000 table fits into zp if compressed with asr #$f8, preamble then starts @ $89, zero bytes free then, but fits

;XXX TODO
;XXX TODO change interleave depending on track (via set max_sectors?)

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

			sax $1c08				;clear counters
			sax $1c04

			dex					;disable all interrupts
			stx $180e
			stx $1c0e
			ldx $180d				;clear all IRQ flags to ack possibly pending IRQs
			ldx $1c0d

			ldx #$c0				;enable timer 1 flag
			stx $1c0e
			stx $180e

			;cli					;now it is save to allow interrupts again, as they won't happen anymore, okay, it is a lie, timer irqs would happen, but we keep sei

			asl;ldx #.BUSY				;signal that we are ready for transfer
			sta $1800

			bit $1800
			bpl *-3

			ldy #$00				;clear lower part of counter
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

;table_start
;>C:15d2  0e 0a f0 00  06 02 e1 4e  4f 47 d2 4a  4b 43 c3 b4   .......NOG.JKC..
;>C:15e2  4d 45 a5 40  49 41 96 46  4c 44 87 42  48 78 69 5a   ME.@IA.FLD.BHxiZ
;>C:15f2  0f 0b 0d 09  0c 08 f0 3e  3f 37 0e 3a  3b 33 70 4b   .......>?7.:;3pK
;>C:1602  3d 35 0f 30  39 31 60 36  3c 34 07 32  38 3c 2d 1e   =5.091`6<4.28<-.
;>C:1612  07 03 05 01  04 0f b0 2e  2f 27 0a 2a  2b 23 30 e1   ......../'.*+#0.
;>C:1622  2d 25 0b 20  29 21 20 26  2c 24 03 22  28

;.C:152e  EA          NOP
;.C:152f  EA          NOP
;.C:1530  EA          NOP
;.C:1531  48          PHA
;.C:1532  49 00       EOR #$00
;.C:1534  49 00       EOR #$00
;.C:1536  85 83       STA $83
;.C:1538  AD 01 1C    LDA $1C01
;.C:153b  A2 07       LDX #$07
;.C:153d  87 A0       SAX $A0
;.C:153f  29 F8       AND #$F8
;.C:1541  A8          TAY
;.C:1542  A2 3E       LDX #$3E
;.C:1544  AD 01 1C    LDA $1C01
;.C:1547  87 B9       SAX $B9
;.C:1549  4B C1       ASR #$C1
;.C:154b  AA          TAX
;.C:154c  B9 00 02    LDA $0200,Y
;.C:154f  5D 00 02    EOR $0200,X
;.C:1552  BA          TSX
;.C:1553  48          PHA
;.C:1554  F0 54       BEQ $15AA
;.C:1556  5D 02 01    EOR $0102,X
;.C:1559  5D 03 01    EOR $0103,X
;.C:155c  85 85       STA $85
;.C:155e  AD 01 1C    LDA $1C01
;.C:1561  A2 0F       LDX #$0F
;.C:1563  87 C7       SAX $C7
;.C:1565  6B F0       ARR #$F0
;.C:1567  A8          TAY
;.C:1568  A5 00       LDA $00
;.C:156a  59 04 02    EOR $0204,Y
;.C:156d  48          PHA
;.C:156e  AD 01 1C    LDA $1C01
;.C:1571  87 D5       SAX $D5
;.C:1573  4B FC       ASR #$FC
;.C:1575  AA          TAX
;.C:1576  A5 00       LDA $00
;.C:1578  75 01       ADC $01,X
;.C:157a  48          PHA
;.C:157b  AF 01 1C    LAX $1C01
;.C:157e  4B 40       ASR #$40
;.C:1580  A8          TAY
;.C:1581  7D 00 02    ADC $0200,X
;.C:1584  59 05 03    EOR $0305,Y
;.C:1587  70 A8       BVS $1531
;.C:1589  70 A6       BVS $1531
;.C:158b  70 A4       BVS $1531
;.C:158d  70 A2       BVS $1531
;.C:158f  70 A0       BVS $1531
;.C:1591  70 9E       BVS $1531
;.C:1593  70 9C       BVS $1531
;.C:1595  70 9A       BVS $1531
;.C:1597  70 98       BVS $1531
;.C:1599  70 96       BVS $1531
;.C:159b  70 94       BVS $1531
;.C:159d  70 92       BVS $1531
;.C:159f  70 90       BVS $1531
;.C:15a1  70 8E       BVS $1531
;.C:15a3  70 8C       BVS $1531
;.C:15a5  70 8A       BVS $1531
;.C:15a7  4C 81 00    JMP $0081
;.C:15aa  4C 77 05    JMP $0577
;.C:15ad  4C CE 05    JMP $05CE


;.C:1948  A0 52       LDY #$52
;.C:194a  2C 00 1C    BIT $1C00
;.C:194d  30 FB       BMI $194A
;.C:194f  6D 01 1C    ADC $1C01
;.C:1952  50 FE       BVC $1952
;.C:1954  CC 01 1C    CPY $1C01
;.C:1957  D0 EF       BNE $1948
;.C:1959  B7 23       LAX $23,Y
;.C:195b  86 FA       STX $FA
;.C:195d  CB EC       SBX #$EC
;.C:195f  8A          TXA
;.C:1960  0A          ASL A
;.C:1961  A6 93       LDX $93
;.C:1963  D0 03       BNE $1968
;.C:1965  EA          NOP
;.C:1966  EA          NOP
;.C:1967  EA          NOP
;.C:1968  4D 01 1C    EOR $1C01
;.C:196b  87 B9       SAX $B9
;.C:196d  4B C1       ASR #$C1
;.C:196f  D0 D7       BNE $1948
;.C:1971  85 83       STA $83
;.C:1973  A1 00       LDA ($00,X)
;.C:1975  A5 30       LDA $30
;.C:1977  B6 05       LDX $05,Y
;.C:1979  9A          TXS
;.C:197a  EA          NOP
;.C:197b  4C AC 00    JMP $00AC

