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
!src "../config.inc"
!src "constants.inc"

;config params
.CACHED_SECTOR		= 1
.FORCE_LAST_BLOCK	= 1
;.BOGUS_READS		= 0;2
.SHRYDAR_STEPPING	= 0 ;so far no benefit on loadcompd
.DELAY_SPIN_DOWN	= 1
.SANCHECK_BVS_LOOP	= 1
.SANCHECK_FULL_SYNC	= 1
.SANCHECK_HEADER_0F	= 1
.SANCHECK_HEADER_ID	= 1
.SANCHECK_TRAILING_ZERO = 1
.SANCHECK_TRACK		= 1
.SANCHECK_SECTOR	= 1
.INTERLEAVE		= 4

;constants
.STEPPING_SPEED		= $98
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
.reset_drive		= $eaa0
.drivecode		= $0000
.bootstrap		= $0700
.tables			= $0600

.directory		= $0700
.dir_load_addr		= .directory
.dir_file_size		= .directory + 2
.dir_diskside		= .directory + $ff
.dir_first_file_track	= .directory + $fc			;starttrack of first file in dir
.dir_first_file_sector_index= .directory + $fd			;how many blocks are used on this track up to the file
.dir_first_block_pos	= .directory + $fe			;startposition within block
								;with those three values, the absolute position on disk is represented
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
			lda $05ff
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

			lda #$7f				;disable all interrupts
			sta $180e
			sta $1c0e
			sta $180d				;clear all IRQ flags to ack possibly pending IRQs
			sta $1c0d

			;cli					;now it is save to allow interrupts again, as they won't happen anymore

			ldy #$00

			ldx #.BUSY				;signal that we are ready for transfer
			stx $1800

			;wait for atn coming high
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

.drivecode_start
!pseudopc .drivecode {
.zp_start

;.free			= .zp_start + $00
;.free			= .zp_start + $01
;.free			= .zp_start + $02
;.free			= .zp_start + $03
.max_sectors		= .zp_start + $08			;maximum sectors on current track
.dir_sector		= .zp_start + $10
.blocks_on_list		= .zp_start + $11			;blocks tagged on wanted list
.spin_count		= .zp_start + $18
.spin_up		= .zp_start + $19
.timer			= .zp_start + $20
;.free			= .zp_start + $21
;.free			= .zp_start + $22
;.free			= .zp_start + $23
.byte			= .zp_start + $28
.dir_entry_num		= .zp_start + $29
.blocks 		= .zp_start + $30			;2 bytes
.wanted			= .zp_start + $3e			;21 bytes
.index			= .zp_start + $54			;current blockindex
.track			= .zp_start + $56			;DT ;current track
.to_track		= .zp_start + $58			;DT
.sector			= .zp_start + $59			;DS
.temp			= .zp_start + $5a
.preamble_data		= .zp_start + $60
.track_frob		= .zp_start + $66
;free			= .zp_start + $68
.filenum 		= .zp_start + $69
.is_loaded_track	= .zp_start + $6a
.is_loaded_sector	= .zp_start + $6c
.current_id1		= .zp_start + $70
.current_id2		= .zp_start + $71
.bogus_reads		= .zp_start + $78
.first_block_size	= .zp_start + $6e
.last_block_num		= .zp_start + $72
.last_block_size	= .zp_start + $74
.first_block_pos	= .zp_start + $76
.block_num		= .zp_start + $79
;.free			= .zp_start + $7a
.end_of_file		= .zp_start + $7c

.DT			= 18					;dir_track
.DS			= 18					;dir_sector
.PA			= $ff					;preamble
.BL			= 0					;blocks_on_list
.WT			= $ff					;wanted list
___			= $ff

.tab00005555_hi		= * + $00
.tab00333330_hi		= * + $00
.tab05666660_lo		= * + $01
.tab00700077_hi		= * + $00

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
                        !byte ___, $20, $00, $80, $50, $1d, $40, $15, ___, ___, $80, $10, $10, $19, $00, $11	;20
                        !byte ___, ___, $e0, $16, $d0, $1c, $c0, $14, ___, ___, $a0, $12, $90, $18, .WT, .WT	;30
                        !byte .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT	;40
                        !byte .WT, .WT, .WT, $0e, ___, $0f, .DT, $07, .DT, ___, ___, $0a, ___, $0b, ___, $03	;50
                        !byte .PA, .PA, .PA, .PA, .PA, $0d, ___, $05, ___, ___, ___, $00, ___, $09, ___, $01	;60
                        !byte ___, ___, ___, $06, ___, $0c, ___, $04, ___, ___, ___, $02, ___, $08		;70


;01010 01001 01010 01101 01010 11011 01011 10010
;0     8     0     c     0     b     1     2

;this reflects the perfect timing, it is most likely delayed by 2-3 cycles due to bvs branching before?

;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          1111111111111111111111111111111122222222222222222222222222222222333333333333333333333333333333334444444444444444444444444444444455555555555555555555555555555555	;after 10
;              1                      .............   2                                    3                                   4             v      5           |-
;1          111111111111111111111111111111222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555	;after 8
;              1                      .............   2                            3                                   4             v      5           |-
;2          11111111111111111111111111112222222222222222222222222222333333333333333333333333333344444444444444444444444444445555555555555555555555555555	;misses after 6 cycles
;              1                      .............   2                    3                                   4             v      5           |-
;3          1111111111111111111111111122222222222222222222222222333333333333333333333333334444444444444444444444444455555555555555555555555555		;misses bvs when delayed by 4 cycles
;              1                      .............   2            3                                   4             v      5           |-
; in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the end and then there's up to 5 cycles delay (branch + fallthrough

			;XXX TODO /!\ if making changes to gcr_read_loop also the partly decoding in read_sector should be double-checked, same goes for timing changes
			;XXX see if we can use bit 2 from original data, would save space in tables
.read_loop
			lda $1c01				;22333334
			sax <.threes + 1
			asr #$c1				;lookup? -> 4 cycles
			tax
			lda .tab11111000_hi,y			;8 cycles to mask and lookup
.twos			eor .tab02200222_lo,x			;13 cycles to mask ad lookup
			tsx
			pha
			beq .gcr_end				;127

.chksum			eor #$00
			eor $0101,x
			eor $0102,x
.gcr_entry
			sta <.chksum2 + 1
;39
			lda $1c01				;44445555	second read
			ldx #$0f
			sax <.fives + 1
			arr #$f0
								;sta <.fours + 1 to save tay and keep y free? if all tays are saved +0 3 cycles for 3 sta .num + 1
			tay					;44444---		;how's about having 4444---4?
			;ldx #$03				;save another 2 cycles and use $0f, XXX TODO would mean, $0f bytes max in $00 and $40? $20? $80 would be perfect, but how to achieve?
								;XXX TODO could reuse $0f and bloat table with 7th, means another table and interleaved code
;13
.gcr_slow1		lda $1c01				;56666677		third read	;slow down by 6,12,18
			sax <.sevens + 1			;------77		;encode first 7 with sixes and by that shrink 6table? and add it with sevens?
			asr #$fc				;-566666-		;can be shifted, but and would suffice?
			tax

.threes			lda <.tab00333330_hi			;ZP!
			adc .tab44444000_lo,y
			pha
.chksum2		eor #$00				;adds one cycle to checksum this way, but timing seems to be safer
			sta <.chksum + 1			;XXX TODO move this after .sevens?!

.fives			lda <.tab00005555_hi			;ZP!
			adc <.tab05666660_lo,x			;XXX TODO shifted by 1 this would be same as 7d788888 table?
			pha
;36
			;bvs here?
			lax $1c01				;77788888	forth read	;slow down by 2, 4, 6
			asr #$40				;ora #$11011111 would also work, and create an offset of $1f? Unfortunatedly the tab then wraps  but okay when in ZP :-(
								;will asr help here?
			tay
			lda .tab7d788888_lo,x			;this table decodes bit 0 and bit 2 of quintuple 7 and quintuple 8
			ldx #$07				;delay upcoming adc as long as possible, as it clears the v flag
.sevens			adc .tab0070dd77_hi,y			;clears v-flag, decodes the remaining bits of quintuple 7, no need to set x to 3, f is enough
;.sevens		adc .tab00700077_hi,y			;ZP! clears v-flag, decodes the remaining bits of quintuple 7
;19 -> clv?
			pha
;21
			lda $1c01				;11111222	fifth read
			sax <.twos + 1
			and #$f8				;XXX TODO could shift with asr and compress ones table?
								;XXX TODO with shift, bit 2 of twos is in carry and could be added as +0 +4?
			tay
			ldx #$3e
;13
			;let's check out, on real hardware slower speedzones more and more miss reads if only 4 loop runs are given
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop				;and use 2 bvs as safety zone? Would be pretty much save that it misses however, so need to find out
			bvs .read_loop				;2 cycle jitter only
			bvs .read_loop
								;bail out here on zone 0
			;those loop runs will be reduced one  per speedzone, so the loop times out and we restart the whol procedure
.bvs_01			bvs .read_loop
								;here on zone 1
.bvs_02			bvs .read_loop
								;zone 2
.bvs_03			bvs .read_loop
								;zone 3
			;10, 8, 6, 4
			;if we run out of this loop, we have timed out and better reread, maybe the disc spins too slow yet?
-
			jmp .read_sector			;took too long, retry
;29
.gcr_end
			;Z-Flag = 1 on success, 0 on failure (wrong type)
			jmp .read_sector_back
.read_header_back
			txs					;saves 2 cycles copared to pla
			pla					;header_0f_2
!if .SANCHECK_HEADER_0F = 1 {
			cmp #.HEADER_0F
			bne -
}
			pla					;header_0f_1
!if .SANCHECK_HEADER_0F = 1 {
			cmp #.HEADER_0F
			bne -
}
			pla					;header_id2
			tay
			eor <.chksum2 + 1
			bne -					;header checksum check failed? reread
			jmp .read_header_back_


!ifdef .second_pass {
;	!warn $0100 - *, " bytes remaining in zeropage."
}

!if >*-1 != >.read_loop { !error "read_sector not in one page: ", .read_loop, " - ", * }

			* = $200

			;----------------------------------------------------------------------------------------------------
			;
			; CODE TO SLOW DOWN GCR LOOP DEPENDING ON BITRATE
			;
			;----------------------------------------------------------------------------------------------------

.gcr_00
			lda ($00,x)				;8, x = 3 -> lda ($03) = lda $f0a0
			nop					;XXX TODO add a nop here for thcms floppy?
.gcr_20
			lda ($00,x)				;8
			nop
.gcr_40
			nop					;2 + 3 + 3 (nop, jmp here, jmp back) = 8 cycles on top
			lda $1c01
			jmp .gcr_slow1 + 3

			;----------------------------------------------------------------------------------------------------
			;
			; SEND PREAMBLE AND DATA
			;
			;----------------------------------------------------------------------------------------------------
.start_send							;entered with c = 0
			lda #.preloop - .branch - 2		;setup branch to point to preloop first
			ldy #$03 + CONFIG_DECOMP		;with or without barrier, depending on stand-alone loader or not
			sta .branch + 1
.preloop
			lax <.preamble_data,y			;ilda would be 16 bit, lax works with 8 bit \o/
			ldx #$0f				;scramble byte while sending, enough time to do so, preamble is called via jsr, so plenty of time between sent bytes
			sbx #$00
			and #$f0
			eor .ser2bin,x				;swap bits 3 and 0
			ldx #$0a				;masking value for later sax $1800
			bne .preamble_entry			;could work with dop here, but want to prefer data_entry with less cycles on shift over
.send_sector_data
			sta .branch + 1				;do not save code here (could do so by bcc .pre_send and reuse sty/sta, but timing get's very tight then when shifting over from preamble to data
			ldy <.preamble_data + 0			;blocksize + 1, could store that val in an extra ZP-addr, waste 1 byte compared to this solution and save 2 cycles on the dey
			dey
.sendloop							;send the data block
			pla					;just pull from stack instead of lda $0100,y, sadly no tsx can be done, due to x being destroyed
.preamble_entry
			bit $1800
			bmi *-3
			sax $1800				;76540213	-> dddd0d1d
			asl					;6540213. 7
			ora #$10				;654X213. 7	-> dddX2d3d
			bit $1800
			bpl *-3
			sta $1800
			ror					;7654X213 x
			asr #%11110000				;.7654... x	-> ddd54d.d
			bit $1800
			bmi *-3
			sta $1800
			lsr					;..7654..
			asr #%00110000				;...76...	-> ddd76d.d
			dey
			cpy #$ff				;XXX TODO could make loop faster here, by looping alread here on bne? needs a bit of code duplication then
			bit $1800
			bpl *-3
			sta $1800
.branch			bcc .sendloop
			;XXX carry is set here, always, might be useful somewhen
.sendloop_end
			lda #.sendloop - .branch - 2		;do we need to send data as well, or all done?
			cmp .branch + 1
			bne .send_sector_data			;not yet done, send data, y = fitting value for branch, c = 1, always
			lda #.BUSY
			bit $1800
			bmi *-3
			sta $1800
!if >*-1 != >.sendloop {
	!error "sendloop not in one page! Overlapping bytes: ", * & 255
}
			;----------------------------------------------------------------------------------------------------
			;
			; BLOCK OF TRACK LOADED AND SENT
			;
			;----------------------------------------------------------------------------------------------------

			dec <.blocks_on_list			;last block on wishlist?
			beq .track_finished
			jmp .cont_track
.track_finished
			;set stepping speed to $0c, if we loop once, set it to $18
			;XXX TODO can we always do first halfstep with $0c as timerval? and then switch to $18?
			lda #18
!if .SHRYDAR_STEPPING = 1 {
			ldx #$8c
			top
-
			ldx #$98
} else {
-
}
			;sec					;set by send_block and also set if beq
			isc <.to_track
			beq -					;skip dirtrack however

!if .SHRYDAR_STEPPING = 1 {
			stx .stepping_speed + 1
}

			;XXX TODO make this check easier? only done hre?
			lda <.end_of_file			;EOF
			bmi .idle
			jmp .load_track

			;----------------------------------------------------------------------------------------------------
			;
			; TURN DISK OR READ IN NEW DIRECTORY BLOCK
			;
			;----------------------------------------------------------------------------------------------------
.turn_disc_back
			lda #$0c
			sta .en_dis_td
			;copy over dir
			ldy #$00
			sty <.blocks_on_list			;clear, as we didn't reach the dec <.blocks_on_list on this code path
-
			pla
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x				;swap bits 3 and 0
			dey
			sta .directory,y
			bne -

			lax <.filenum				;just loading a new dir-sector, not requesting turn disk?
			cmp #BITFIRE_REQ_DISC
			bcs +
			jmp .load_file_				;XXX TODO bcc .load_file but bus_lock gets in our way, we should jmp there to keep distances short
+
			eor .dir_diskside			;compare side info
			beq +
			jmp .turn_disc				;XXX TODO two times out of range :-( still wrong side
+
			sta <.filenum				;reset filenum
			top

			;----------------------------------------------------------------------------------------------------
			;
			; ENTRY POINT OF IDLE LOOP
			;
			;----------------------------------------------------------------------------------------------------
.idle
			inc <.filenum				;autoinc always, so thet load_next will also load next file after a load with filenum
			lda $1c00				;turn off LED
!if .DELAY_SPIN_DOWN = 0 & CONFIG_MOTOR_ALWAYS_ON = 0 {
			and #.MOTOR_OFF & .LED_OFF
} else {
			and #.LED_OFF
}
			sta $1c00
!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
			and #.MOTOR_OFF
			sta .spinval + 1
}
			;----------------------------------------------------------------------------------------------------
			;
			; RECEIVE/WAIT FOR A BYTE FROM HOST
			;
			;----------------------------------------------------------------------------------------------------
.get_byte
			ldy #.BUSY

!if CONFIG_MOTOR_ALWAYS_ON = 0 & .DELAY_SPIN_DOWN = 1 {
			lda #$80				;expect a whole new byte and start with a free bus
			;sta $1c09				;this will spin down the motor after ~ 4s
			sta <.timer
			sta $1800				;clear lines

			ldx $1800
			bne *-3
-
			cpx $1800				;check for a new filename-bit on bus
			bne .wait_bit1				;go to wait bit to waste some more cycles, atn responder might take some time

			bit $1c09				;7 cycles
			bpl -

			;XXX ommit
			;cpx $1800				;check for a new filename-bit on bus
			;bne .wait_bit1

			sta $1c09				;reset timer 7 cycles

			cpx $1800				;check for a new filename-bit on bus
			bne .wait_bit1

			dec <.timer				;count down rounds
			bne -					;8 cycles

			cpx $1800				;check for a new filename-bit on bus
			bne .wait_bit1
.spinval
			lda #$00
			sta $1c00
			lda #$80
			bne .wait_bit1
.lock
;			lda .spinval + 1			;XXX TODO comment out
;			sta $1c00				;spin down finally

			lda #$80				;expect a whole new byte and start with a free bus
			ldx $1800				;wait for buslock to end
			bmi *-3
			ldx $1800
			bne *-3

} else {
.lock
			lda #$80				;expect a whole new byte and start with a free bus
			sta $1800				;clear lines
			ldx $1800				;wait for buslock to end
			bmi *-3
			ldx $1800				;wait for buslock to end
			bne *-3
}
			;XXX TODO wait on turn disc for $1800 to be 0? ($dd02 == $3f) But we do so already on entyr of this func?
.wait_bit1
			cpx $1800
			beq .wait_bit1
			ldx $1800
			bmi .lock
			cpx #$04
			ror
			bcc .wait_bit1				;more bits to fetch?
			sty $1800				;set busy bit

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD FILE / EXECUTE COMMAND, $00..$7f, $ef, $f0..$fe, $ff
			; XXX TODO files up to $ee would be possible in theory, but more dir sectors would be needed
			;
			;----------------------------------------------------------------------------------------------------

			;load file, file number is in A
.load_file
			cmp #BITFIRE_RESET
			bne *+5
			jmp .reset_drive
			cmp #BITFIRE_LOAD_NEXT
			beq +
			sta <.filenum				;set new filenum
+
			cmp #BITFIRE_REQ_DISC			;sets carry if so, used later on on bcs
			lda #.MOTOR_ON
;!if CONFIG_MOTOR_ALWAYS_ON = 0 {
;!if .BOGUS_READS > 0 {
;			ldx #.BOGUS_READS
;			stx <.bogus_reads
;}
;}
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
!if .CACHED_SECTOR = 1 {
			stx .is_loaded_track			;throw away preloaded sector, to avoid skip of read_sector upon turn disc, as we would repeatedly read the same sector and skip the read, as it is still cached
}
			stx <.last_block_num			;end at index 0
			jmp .turn_disc_entry			;a = sector, x = 0 = index
+
			;----------------------------------------------------------------------------------------------------
			;
			; LOAD A FILE
			;
			;----------------------------------------------------------------------------------------------------

			lax <.filenum				;load filenum
.load_file_
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
			lda .dir_first_file_sector_index	;sectorindex of first file in dir (not sector number, but as if written with interleave = 1)
			sta <.blocks + 1
			lda .dir_first_block_pos		;pos in first sector where file starts
			sta <.blocks + 0

			ldy .dir_first_file_track		;track of first file in dir
			ldx #$00
.next_dir_entry
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
			jmp .set_max_sectors			;setup max_sectors, expects track in Y, returns to .find_file_back
.find_file_back
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
.no_next_track
			txa
			sbx #-4
			bne .next_dir_entry			;XXX TODO this should always branch
.find_file_back_						;we return from max_sectors, do second check here, as we can fall through for free
			cpx <.dir_entry_num			;recheck again, yuck!
			bne .find_file_back
.found_file
			;store track
			sty <.to_track

			;remember file index
			;stx .file_index			;same as dir_entry_num

			lda .dir_file_size + 0,x
			tay
			ora .dir_file_size + 1,x		;enough to check for zero filesize?
			bne +
			;file not found
			lda $1c00
			and #.MOTOR_OFF
			ora #.LED_ON
			sta $1c00
			jam
			;jmp .idle
+
			lda <.blocks + 0
			;calc first block size
			eor #$ff
			sta <.first_block_size

			;lda <.first_block_size
			cmp .dir_file_size + 0,x		;compare with file_size

			tya					;lda .dir_file_size + 0,x
			ldy .dir_file_size + 1,x		;load file_size + 1
			bne .is_big				;filesize < $100 ? is_big if not
			bcc .is_big				;it is < $0100 but does not fit in remaining space? -> is_big
			sta <.first_block_size			;fits in, correct size, this will cause overflow on next subtraction and last_block_num will be zero
.is_big
			clc
			sbc <.first_block_size
			sta <.last_block_size

			tya					;file_size + 1
			adc #$00
			sta <.last_block_num

			;remaining blocks on track to find out start sector
			lax <.blocks + 1
			beq .found_sector

			lda #$00				;walk through track from pos 0 on
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
			beq +					;nothing to step, end

			ldy #$00				;make stepping positive
			bcs .seek_up				;up or downwards?
.seek_down
			eor #$ff				;down we go, so invert num tracks as it is negative
			adc #$01
			iny					;but make stepping negative
.seek_up
			asl					;counter is twice the number of tracks (halftracks)
			tax
.step
.stepping_speed		lda #.STEPPING_SPEED
			sta $1c09
			tya
.halftrack
			eor $1c00
			sec
			rol
			and #3
			eor $1c00
			jmp .skiptab
!align 255,5
.tab0070dd77_hi
                        !byte                          $b0, $80, $a0, ___, $b0, $80, $a0, ___, $b0, $80, $a0
.skiptab
			sta $1c00
			bit $1c09
			bmi *-3

			dex
			bne .step
+
			lda <.to_track				;already part of set_bitrate -> load track
			jmp +

                        !byte ___, ___, ___, ___, ___, $20, $00, $80, ___, $20, $00, $80, ___, $20, $00, $80
+
			;----------------------------------------------------------------------------------------------------
			;
			; SET UP BITRATE, MODIFY GCR LOOP IN ZEROPAGE
			;
			;----------------------------------------------------------------------------------------------------

.set_bitrate
!if .SHRYDAR_STEPPING = 1 {
			ldx #$98
			stx .stepping_speed + 1
}
			tay
!if .SANCHECK_TRACK = 1 {
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x
			sta <.track_frob			;needs to be precacled here
}
			ldx #$fe				;continue after max_sectors
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
			beq +
			jmp .find_file_back_			;can only happen if we come from .set_bitrate code-path, not via .set_max_sectors, as x is a multiple of 4 there, extend range by doin two hops, cheaper than long branch
+

			rol					;00000xx1
!if .SANCHECK_BVS_LOOP = 1 {
			sax .br0 + 1				;$00,$02,$04,$06
}
			asl					;shift to right position
			asl
			asl
			asl					;0xx10000 -> $10, $30, $50, $70
			sbx #$01				;preserve led, motor and stepper-bits -> $0f, $2f, $4f, $6f -> 0xx01111
			lda $1c00
			ora #$60				;x11xxxxx new bitrate bits to be set
			sax $1c00				;merge

!if .SANCHECK_BVS_LOOP = 1 {
			ldy #$a9				;restore 3 LDAs
			top
-
			ldy #$70
.br0			bpl *					;now create up to 3 BVSs, depending on speedzone
			sty <.bvs_03
			sty <.bvs_02
			sty <.bvs_01
			bmi -					;first round? then LDA was set 3 times, now set right amount of BVS on a second round, after that, we fall through this check
}
			txa

			ldy #$4c				;same as $98 >> 1, used with set_bitrate this value. common values to set up for 1, 2, 3
			ldx #>.gcr_40				;should be $02

			and #$60
			beq .bitrate_3				;a = $00		;a bit pity that this needs another bunch of branches :-( TODO
			cmp #$40
			beq .bitrate_1				;a = $40
			lda #<.gcr_20				;03
			bcc .bitrate_2				;a = $20
								;a = $60
.bitrate_0
			ldy #$ad
			lda #$01
			ldx #$1c
			top
.bitrate_1
			lda #<.gcr_40				;06	XXX TODO 0,3,6 -> derivate from bitrate? -> asl + self?
.bitrate_2
.bitrate_3
			sty <.gcr_slow1 + 0			;modify single point in gcr_loop for speed adaptioon, lda $1c01 or branch out with a jmp to slow down things
			sta <.gcr_slow1 + 1
			stx <.gcr_slow1 + 2

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD ALL NEEDED SECTORS OF A TRACK AS MARKED ON WANTED-LIST
			;
			;----------------------------------------------------------------------------------------------------

			lda <.sector				;now crate our wishlist for the current track
.wanted_loop
			tax
			ldy <.index				;get index
			sty <.wanted,x				;write index into wantedlist
			inc <.blocks_on_list			;count number of blocks in list (per track num of blocks)
			cpy <.last_block_num			;inc index and check if index > file_size + 1 -> EOF
			inc <.index
			bcs .load_wanted_blocks			;yep, EOF, carry is set
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
.cont_track
!if .CACHED_SECTOR = 1 {
			lda <.is_loaded_track			;is the sector we hold in ram the one we need?
			cmp <.track
			bne .rs_cont				;nope

			ldx <.is_loaded_sector			;track is okay, sector too?
								;XXX TODO, why restricting this to index 0? Could in theory also be any other valid block, but saves code this way and as this block is forced, other cases should not happen
			ldy <.wanted,x				;check with wanted list if it is the first sector in our chain
			bne .rs_cont				;not requested, load new sector
			jmp .wipe_from_wanted			;skip loading of any data and directly start sending
.rs_cont
}
			;----------------------------------------------------------------------------------------------------
			;
			; READ A SECTOR WITH HEADER AND DO VARIOUS SANITY CHECKS ON IT
			;
			;----------------------------------------------------------------------------------------------------
.read_sector
.read_gcr_header						;read_header and do checksum, if not okay, do again
			ldx #$07				;bytes to fetch
			ldy #$52				;type (header)
			lda #$0c
			jmp .read_gcr
.read_header_back_
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
			bne .read_sector
			cmp <.current_id1
			bne .read_sector
+
}
			pla					;.header_track
!if .SANCHECK_TRACK = 1 {
			cmp <.track_frob			;needs to be precalced, else we run out of time
			bne .read_sector
}
			;XXX TODO, can only be $1x or 0x
			pla					;header_sector
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x
			tax
!if .SANCHECK_SECTOR = 1 {
			cpx <.max_sectors
			bcs .rs_retry2
}
			stx <.is_loaded_sector
								;96 cycles
;!if CONFIG_MOTOR_ALWAYS_ON = 0 {
;!if .BOGUS_READS > 0 {
;			lda <.bogus_reads
;			beq +
;			dec <.bogus_reads
;			bne .read_sector
;+
;}
;}
			ldy <.wanted,x				;sector on list?
!if .FORCE_LAST_BLOCK = 1 {
			cpy <.last_block_num			;current block is last block on list?
			bne .not_last				;nope continue
			ldx <.blocks_on_list			;yes, it is last block of file, only one block remaining to load?
			dex
			beq .last				;yes, so finally load last blovk
			bne .rs_retry2				;reread
}
.not_last
			iny
			beq .rs_retry2				;if block index is $ff, we reread, as block is not wanted then
								;max 111/112 cycles passed, so still header_gap bytes flying by and we finish in time
.last
			;----------------------------------------------------------------------------------------------------
			;
			; HEADER DONE, NOW READ SECTOR DATA
			;
			;----------------------------------------------------------------------------------------------------

			ldx #$ff				;bytes to fetch
			ldy #$55				;type (sector)
			lda #$4c
.read_gcr
			sta <.gcr_end				;setup return jump
			eor #$2c
			sta .header_t2 + 1			;$20 or $60 depending if header or sector, just the right values we need there
			txs
!if .SANCHECK_FULL_SYNC = 1 {
;			ldx #$00
;			top
;.still_sync
;			ldx #$01
;.wait_sync
;			bit $1c00				;wait for end of sync
;			bmi .still_sync
;			txa
;			beq .wait_sync				;no loop run taken, so fell through check

			bit $1c00				;wait for end of sync
			bpl *-3

			bit $1c00				;wait for end of sync
			bmi *-3
} else {
			bit $1c00				;wait for end of sync
			bmi *-3
}
			lda $1c01				;sync mark -> $ff
			clv
			bvc *
			clv
			cpy $1c01				;11111222
			bne .read_sector			;start over with a new header again, do not wait for a sectorheadertype to arrive
			bvc *
			lda $1c01				;22333334
			ldx #$3e
			sax <.threes + 1
			asr #$c1				;lookup? -> 4 cycles
.header_t2		eor #$c0
			bne .read_sector			;start over with a new header again, do not wait for a sectorheadertype to arrive
			nop
			pla
			pha
			pla
			pha
			lda #.EOR_VAL
			jmp .gcr_entry				;36 cycles until entry
.rs_retry2
			jmp .read_sector			;will be sbc (xx),y if disabled
.read_sector_back
			;6 cycles of 15 passed, another 9 can pass?
			clv
			eor $0101
			sta <.chksum2 + 1
								;checksum
			lax $1c01				;44445555
			bvc *
			arr #$f0
			clv					;after arr, as it influences v-flag
			tay					;44444---

!if .SANCHECK_TRAILING_ZERO = 1 {
			lda #$0f
			sbx #.CHECKSUM_CONST1			;4 bits of a trailing zero after checksum
			bne .rs_retry2				;check remaining nibble if it is $05
}
			ldx $1c01
			bvc *
!if .SANCHECK_TRAILING_ZERO = 1 {
			cpx #.CHECKSUM_CONST2			;0 01010 01 - more traiing zeroes
			bne .rs_retry2
}
			lda $1c01
!if .SANCHECK_TRAILING_ZERO = 1 {
			and #$e0
			cmp #.CHECKSUM_CONST3 & $e0		;010 xxxxx - and more trailing zeroes, last nibble varies on real hardware
			bne .rs_retry2
}
			ldx <.threes + 1
			lda <.tab00333330_hi,x			;sector checksum
			ora .tab44444000_lo,y
			eor $0102
			eor <.chksum2 + 1
			eor <.chksum + 1			;XXX TODO annoying that last bytes nned to be checksummed here :-(
			bne .rs_retry2				;checksum okay? Nope, take two hops to get to the beginning of code again
								;counter dd db d8 d6
			;XXX TODO -> set this once anayway per track? but after send?
!if .CACHED_SECTOR = 1 {
			ldx <.track				;remember T/S for later check if sector is cached
			stx <.is_loaded_track
}
			;XXX TODO a lot of wanted reads and decisions made, can they be aggregated?!?!?!?!
			ldx <.is_loaded_sector			;XXX TODO is check already above, can we remember result?
.wipe_from_wanted
			ldy #$ff				;blocksize full sector ($ff)
			lda <.wanted,x				;grab index from list (A with index reused later on after this call), also a is loaded last this way and we can directly check flags afterwards
			sty <.wanted,x				;clear entry in wanted list
.send_data
.en_dis_td		top .turn_disc_back			;can be disabled and we continue with send_data, else we are done here already

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP SEND LOOP, AND DECIDE BLOCK SIZES FIRST
			;
			;----------------------------------------------------------------------------------------------------

			cmp <.last_block_num			;compare once when code-path is still common, carry is not tainted until needed
			sta <.block_num
			tax					;is needed then however to restore flags, but cheaper
			bne .is_not_first_block
.is_first_block
			ldy <.first_block_size
			;lda #$00				;a is 0 already
			;cmp <.last_block_num			;do compare beforehand for both cases
			bcc .first_block_big			;bcs = last_block, so a small file that starts and ends in same sector, bcc = big_file, all done
			tya
			clc
			adc <.blocks + 0
			;eor #$ff
			bcc .first_block_small			;XXX TODO bcc would also suffice? Can this really overflow if we end in same block?
.is_not_first_block
			;ldy #$ff				;y is $ff already
			;cmp <.last_block_num			;done in the beginning
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
			iny					;set up num of bytes to be transferred
			sty <.preamble_data + 0			;used also as send_end on data_send by being decremented again

!if CONFIG_DECOMP = 1 {						;no barriers needed with standalone loadraw
			lda <.index				;max index to match against
			ldx #$14				;walk through list of sectors to be loaded
.min_loop
			cmp <.wanted,x				;compare
			bcc .is_bigger				;bigger index, next please
			lda <.wanted,x				;smaller (or same, but can't happen, as index is unique) remember new minimum
.is_bigger
			dex					;next entry
			bpl .min_loop

			;barrier, if new, last block-addr = new barrier? nope can even be higher :-(
			;else we could remember values and just copy them instead of doing new calc :-(

			ldx <.dir_entry_num
								;we need to at least wait with setting barrier until first block is loaded, as load-address comes with this block, barrier check on resident side must fail until then by letting barrier set to 0
			tay
			beq .barr_zero				;zero, so still not loaded
			dey

;			lda .dir_load_addr + 0,x
;			sec					;filesize/first_block_size is stored with -1, add here again
;			adc <.first_block_size
;			;bcs +
;			dey
;+
			tya
			clc

			adc .dir_load_addr + 1,x		;add load address highbyte to lowest blockindex
.barr_zero
			sta <.preamble_data + 3			;barrier, zero until set for first time, maybe rearrange and put to end?
}

			;loadaddr	blocksize	data till		;lz-readpos okay
			;65c0		c0		-> 6680			addr not yet set
			;6680		100		-> 6780			65c0-66c0
			;6780		100		-> 6880			66c0-67c0
			;6880		100		-> 6980			67c0-68c0


			;loadaddr	blocksize	data till		;lz-readpos okay
			;6510		20		-> 6530			addr not yet set
			;6530		100		-> 6630			6510-6610
			;6630		100		-> 6730			6610-6710
			;6730		100		-> 6830			6710-6810

;			      first block ends in new page
;			                  |-barrier
;			       000|1111111 2222222|3333333
;depackpos/barrier	| $1000 | $1100 | $1200 | $1300 |
;                        000|1111111 2222222|3333333
;			            |-barrier
;			first block ends in same page

			lda .dir_load_addr + 0,x		;fetch load address lowbyte
			sec					;XXX TODO could be saved then? Nope, crashes on cebit'18 bootloader

			ldy <.block_num				;first block? -> send load address, neutralize sbc later on, carry is set
			beq +
			ldy #$80
			adc <.first_block_size			;else add first block size as offset, might change carry
+
			sta <.preamble_data + 1			;block address low

			lda .dir_load_addr + 1,x		;add load address highbyte
			sbc #$00				;subtract one in case of overflow
			clc
			adc <.block_num				;add block num

			;clc					;should never overrun, or we would wrap @ $ffff?
			sty <.preamble_data + 3 + CONFIG_DECOMP	;ack/status to set load addr, signal block ready
			sta <.preamble_data + 2			;block address high
			jmp .start_send

.debug_decode_sector
			;for debug, decode sector and turn it upside down for easy hexdiff
;			ldy #$ff
;-
;			pla
;			ldx #$0f
;			sbx #$00
;			and #$f0
;			eor .ser2bin,x
;			sta $0700,y
;			dey
;			tsx
;			bne -


;tables with possible offsets
.tab11111000_hi		= .tables + $00
.tab44444000_lo 	= .tables + $04
.tab7d788888_lo		= .tables + $00

;table wth no offset
.tab02200222_lo		= .tables + $00

!ifdef .second_pass {
	!if * > .table_start { !serious "Upload code is ", * - .table_start, " bytes too big!" }
}

!ifdef .second_pass {
	!warn .table_start - *, " bytes remaining for drivecode."
}

!if * > .tables {
	!set .junk_start = *
} else {
	!set .junk_start = .tables
}


			* = .junk_start
			;use remaining space with code or fill up with junk
			!for .x, 0, $20 - <.junk_start {
				!byte $ff - .x
			}

	 		* = .tables + $22
.table_start
                        !byte           $0e, $0a, $24, $00, $06, $02, $28, $4e, $4f, $47, $2c, $4a, $4b, $43
                        !byte $30, $31, $4d, $45, $34, $40, $49, $41, $38, $46, $4c, $44, $3c, $42, $48, $3f
                        !byte $40, $41, $0f, $0b, $0d, $09, $0c, $08, $f0, $5e, $5f, $57, $0e, $5a, $5b, $53
                        !byte $70, $51, $5d, $55, $0f, $50, $59, $51, $60, $56, $5c, $54, $07, $52, $58, $5f
                        !byte $60, $61, $07, $03, $05, $01, $04, $67, $b0, $4e, $4f, $47, $0a, $4a, $4b, $43
                        !byte $30, $71, $4d, $45, $0b, $40, $49, $41, $20, $46, $4c, $44, $03, $42, $48, $7f

;                        !byte           $0e, $0a, ___, $00, $06, $02, ___, $4e, $4f, $47, ___, $4a, $4b, $43
;                        !byte ___, ___, $4d, $45, ___, $40, $49, $41, ___, $46, $4c, $44, ___, $42, $48, ___
;                        !byte ___, ___, $0f, $0b, $0d, $09, $0c, $08, $f0, $5e, $5f, $57, $0e, $5a, $5b, $53
;                        !byte $70, ___, $5d, $55, $0f, $50, $59, $51, $60, $56, $5c, $54, $07, $52, $58, ___
;                        !byte ___, ___, $07, $03, $05, $01, $04, ___, $b0, $4e, $4f, $47, $0a, $4a, $4b, $43
;                        !byte $30, ___, $4d, $45, $0b, $40, $49, $41, $20, $46, $4c, $44, $03, $42, $48, ___
.ser2bin
			!byte $00 xor .EOR_VAL
			!byte $08 xor .EOR_VAL
			!byte $02 xor .EOR_VAL
			!byte $0a xor .EOR_VAL
			!byte $04 xor .EOR_VAL
			!byte $0c xor .EOR_VAL
			!byte $06 xor .EOR_VAL
			!byte $0e xor .EOR_VAL

			!byte $01 xor .EOR_VAL	;same as above but upside down and xor $0f? or same as above but + 1 -> bit 8 adds 1
			!byte $09 xor .EOR_VAL
			!byte $03 xor .EOR_VAL
			!byte $0b xor .EOR_VAL
			!byte $05 xor .EOR_VAL
			!byte $0d xor .EOR_VAL
			!byte $07 xor .EOR_VAL
			!byte $0f xor .EOR_VAL

                        !byte $50, $91, $92, $93, $0d, $95, $96, $97, $40, $99, $9a, $9b, $05, $9d, $9e, $9f
                        !byte $a0, $a1, $a2, $a3, $a4, $a5, $a6, $a7, $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, $b1, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, $bf
                        !byte $c0, $c1, $c2, $c3, $c4, $c5, $c6, $c7, $e0, $1e, $1f, $17, $06, $1a, $1b, $13
                        !byte $d0, $d1, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18, $df
                        !byte $e0, $e1, $e2, $e3, $e4, $e5, $e6, $e7, $a0, $0e, $0f, $07, $02, $0a, $0b, $03
                        !byte $90, $f1, $0d, $05, $08, $00, $09, $01, $f8, $06, $0c, $04, $fc, $02, $08, $ff

;			!byte $50
;.scramble_preamble
;			sty <.preamble_data + 0			;ack/status to set load addr, signal block ready
;			dop
;			!byte $0d
;			sta <.preamble_data + 1 + CONFIG_DECOMP	;block address high
;			dop
;			!byte $40
;			ldx #$03 + CONFIG_DECOMP		;with or without barrier, depending on stand-alone loader or not
;			dop
;			!byte $05
;			stx .pre_len + 1			;set preamble size
;			ldy #.preloop - .branch - 2		;setup branch to point to preloop first
;			jmp .start_send
;			nop
;			nop
;			nop
;
;                        !byte                                         $80, $0e, $0f, $07, $00, $0a, $0b, $03
;                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08
;
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;
;                        !byte                                         $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
;                        !byte $d0, ___, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18
;
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;			nop
;
;                        !byte                                         $a0, $0e, $0f, $07, $02, $0a, $0b, $03		;9 bytes
;                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___

;                        !byte $50, ___, ___, ___, $0d, ___, ___, ___, $40, ___, ___, ___, $05, ___, ___, ___		;20 bytes with 3 dops
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $80, $0e, $0f, $07, $00, $0a, $0b, $03
;                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $e0, $1e, $1f, $17, $06, $1a, $1b, $13		;9 bytes
;                        !byte $d0, ___, $1d, $15, $0c, $10, $19, $11, $c0, $16, $1c, $14, $04, $12, $18, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, $a0, $0e, $0f, $07, $02, $0a, $0b, $03		;9 bytes
;                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___

;tab0070dd77
;                        !byte ___, ___, ___, ___, ___, $b0, $80, $a0, ___, $b0, $80, $a0, ___, $b0, $80, $a0
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte ___, ___, ___, ___, ___, $20, $00, $80, ___, $20, $00, $80, ___, $20, $00, $80

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


}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start

.second_pass

;XXX TODO merge high and lownibbles in one table and separate by and #$0f, possible if we save data due to that and also cycles

;11111000 table fits into zp if compressed with asr #$f8, preamble then starts @ $89, zero bytes free then, but fits

;halfstep, send_data, timer elapsed? else wait rest, halfstep, wait
;XXX TODO
;XXX TODO change interleave depending on track (via set max_sectors?)

;XXX TODO optimize code size on stepping
;XXX TODO optimze eof detection?



;check in loader read of $0700 after turn disc?
;klappt laden und entpacken von eigentlichem file? ZP adessen konflikt?
;watch exec 0100 -> step through
;else revert changes in drivecode and check again, only happens with spin up/down?
;does it kill 1c00 vals with double and?
;turn disc, make it send a single byte to ack? -> wait block ready, receive a byte and then floppy goes idle?


;1 0010	8	eor	a		2
;1 0011	9	eor	a		3
;1 0101	5	eor	a		f
;1 0110	c	eor	a		6
;1 0111	d	eor	a		7
;0 1001	8	eor	0		8
;1 1001	3x	eor	a		9
;1 1010	0	eor	a		a
;0 1010	0	eor	0		0
;1 1011	1	eor	a		b
;0 1011	1	eor	0		1
;0 1110	4	eor	0		4
;1 1110	4	eor	a		e
;0 1101	c	eor	0		c
;1 1101	7x	eor	a		d
;0 1111	5	eor	0		5
