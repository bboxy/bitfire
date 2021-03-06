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
; Many thanks go out to THCM, Dano and doomed for all the testing <3
;--------------------------------------------------------------------------------------------------------------------------

!src "config.inc"
!convtab pet
!cpu 6510

;XXX TODO add shingle-stepping again, upon last sector to be transmitted?

;config params
.BOGUS_READS		= 0;2
.FORCE_LAST_BLOCK	= 1
.SANCHECK_BVS_LOOP	= 1
.SANCHECK_FULL_SYNC	= 1
.SANCHECK_HEADER_0F	= 1
.SANCHECK_HEADER_ID	= 1
.SANCHECK_TRAILING_ZERO = 1
.SANCHECK_TRACK		= 1
.SANCHECK_SECTOR	= 1

;constants
.STEPPING_SPEED		= $98
.CHECKSUM_CONST1	= $05	;%00000101 -> 4 times 01010
.CHECKSUM_CONST2	= $29	;%00101001
.CHECKSUM_CONST3	= $4a	;%01001010
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
___			= 0
_FF			= $ff

;adresses
.reset_drive		= $eaa0
.drivecode		= $0000
.bootstrap		= $0700
.tables			= $0600

.directory		= $0700
.dir_load_addr		= .directory
.dir_file_size		= .directory + 2
.dir_diskside		= .directory + $ff
.dir_first_file_track	= .directory + $fc		;starttrack of first file in dir
.dir_first_file_sector_index= .directory + $fd		;how many blocks are used on this track up to the file
.dir_first_block_pos	= .directory + $fe		;startposition within block
							;with those three values, the absolute position on disk is represented
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
			sta .fn + 1		;remember diskside#

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

			ldy #$00

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
			bit $1800		;no need to, check is done by get_byte
			bmi *-3

.fn			lda #$00		;load directory
			jmp .load_file




}
.bootstrap_end
.bootstrap_size = .bootstrap_end - .bootstrap_start

.drivecode_start
!pseudopc .drivecode {
.zp_start


;XXX TODO fill gaps with junk, remaining free zp bytes are zero else (enough zeroes there anyway due to wishlist
;			= .zp_start + $00
;			= .zp_start + $01
;			= .zp_start + $02
;			= .zp_start + $03
.max_sectors		= .zp_start + $08		;maximum sectors on current track
.dir_sector		= .zp_start + $10
.blocks_on_list		= .zp_start + $11		;blocks tagged on wanted list
.spin_count		= .zp_start + $18
.spin_up		= .zp_start + $19
.first_block		= .zp_start + $20
.filenum 		= .zp_start + $21
.block_num		= .zp_start + $22
.end_of_file		= .zp_start + $23
.byte			= .zp_start + $28
.dir_entry_num		= .zp_start + $29
.blocks 		= .zp_start + $30		;2 bytes
.wanted			= .zp_start + $3e		;21 bytes
.index			= .zp_start + $54		;current blockindex
.track			= .zp_start + $56		;DT ;current track
.to_track		= .zp_start + $58		;DT
.sector			= .zp_start + $59		;DS
.temp			= .zp_start + $5a
.preamble_data		= .zp_start + $60
.track_frob		= .zp_start + $66
.file_index		= .zp_start + $68
;.free			= .zp_start + $69
.is_loaded_track	= .zp_start + $6a
.is_loaded_sector	= .zp_start + $6c
.current_id1		= .zp_start + $70
.current_id2		= .zp_start + $71
.bogus_reads		= .zp_start + $78
.load_addr_hi		= .zp_start + $79
.first_block_size	= .zp_start + $6e
.last_block_num		= .zp_start + $72
.last_block_size	= .zp_start + $74
.first_block_pos	= .zp_start + $76
.send_end		= .zp_start + $7a
;.send_start		= .zp_start + $7c

.DT			= 18	;dir_track
.DS			= 18	;dir_sector
.PA			= $ff	;preamble
.BL			= 0	;blocks_on_list
.WT			= $ff	;wanted list

.tab00005555_hi		= * + $00
.tab00333330_hi		= * + $00
.tab05666660_lo		= * + $01

			;     0    1    2    3    4    5    6    7    8    9    a    b    c    d    e    f
                        !byte _FF, _FF, _FF, _FF, $f0, $60, $b0, $20, _FF, $40, $80, $00, $e0, $c0, $a0, $80	;00
                        !byte .DS, .BL, $f0, $1e, $70, $1f, $60, $17, _FF, _FF, $b0, $1a, $30, $1b, $20, $13	;10
                        !byte _FF, _FF, _FF, _FF, $50, $1d, $40, $15, _FF, _FF, $80, $10, $10, $19, $00, $11	;20
                        !byte _FF, _FF, $e0, $16, $d0, $1c, $c0, $14, _FF, _FF, $a0, $12, $90, $18, .WT, .WT	;30
                        !byte .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT, .WT	;40
                        !byte .WT, .WT, .WT, $0e, _FF, $0f, .DT, $07, .DT, _FF, _FF, $0a, _FF, $0b, _FF, $03	;50
                        !byte .PA, .PA, .PA, .PA, .PA, $0d, _FF, $05, _FF, _FF, _FF, $00, _FF, $09, _FF, $01	;60
                        !byte _FF, _FF, _FF, $06, _FF, $0c, _FF, $04, _FF, _FF, _FF, $02, _FF, $08		;70


;01010 01001 01010 01101 01010 11011 01011 10010
;0     8     0     c     0     b     1     2

;this reflects the perfect timing, it is most likely delayed by 2-3 cycles due to bvs branching before?

;           cycle
;bit rate   0         10        20        30        40        50        60        70        80        90        100       110       120       130       140       150       160
;0          1111111111111111111111111111111122222222222222222222222222222222333333333333333333333333333333334444444444444444444444444444444455555555555555555555555555555555	;after 10
;              1                      .............   2                                      3                                   4             v      5           |-
;1          111111111111111111111111111111222222222222222222222222222222333333333333333333333333333333444444444444444444444444444444555555555555555555555555555555	;after 8
;              1                      .............   2                              3                                   4             v      5           |-
;2          11111111111111111111111111112222222222222222222222222222333333333333333333333333333344444444444444444444444444445555555555555555555555555555	;misses after 6 cycles
;              1                      .............   2                      3                                   4             v      5           |-
;3          1111111111111111111111111122222222222222222222222222333333333333333333333333334444444444444444444444444455555555555555555555555555		;misses bvs when delayed by 4 cycles
;              1                      .............   2              3                                   4             v      5           |-
; in fact this can jitter a lot, bvc can loop at cycle 0 if it misses at the ende and then there's up to 5 cycles delay (branch + fallthrough

			;XXX TODO /!\ if making changes to gcr_read_loop also the partly decoding in read_sector should be double-checked, same goes for timing changes
			;XXX see if we can use bit 2 from original data, would save space in tables
.read_loop
			lda $1c01			;22333334
			sax <.threes + 1
			asr #$c1			;lookup? -> 4 cycles
			tax
			lda .tab11111000_hi,y
.twos			eor .tab02200222_lo,x
			tsx
			pha
			beq .gcr_end			;127

.chksum			eor #$00
			eor $0101,x
			eor $0102,x
.gcr_entry
			sta <.chksum2 + 1
;39
			lda $1c01			;44445555	second read
			ldx #$0f			;ldx + sax can be moved after lda + eor?
			sax <.fives + 1
			arr #$f0
			;sta <.fours + 1 to save tay and keep y free? if all tays are saved +0 3 cycles for 3 sta .num + 1
			tay				;44444---		;how's about having 4444---4?
			ldx #$03			;save another 2 cycles and use $0f
;15
.gcr_slow1		lda $1c01			;56666677		third read	;slow down by 6,12,18
			sax <.sevens + 1		;------77		;encode first 7 with sixes and by that shrink 6table? and add it with sevens?
			asr #$fc			;-566666-
			tax

.threes			lda <.tab00333330_hi		;ZP!
			adc .tab44444000_lo,y
			pha
.chksum2		eor #$00
			sta <.chksum + 1

.fives			lda <.tab00005555_hi		;ZP!
			adc <.tab05666660_lo,x
			pha
;36
			;bvs here?
			lax $1c01			;77788888	forth read	;slow down by 2, 4, 6
			and #$e0			;ora #$00011111 would also work, and create an offset of $1f? Unfortunatedly the tab then wraps :-(
							;will asr help here?
			tay
							;XXX TODO bit 2 can be used in original form
			lda .tab00088888_lo,x		;XXX TODO could combine two tables (low + highnibbles) and separate here by and? but must be all other tables that are combined
			ldx #$07
.sevens			adc .tab77700077_hi,y		;
;19 -> clv?
			pha
;21
			lda $1c01			;11111222	fifth read
			sax <.twos + 1
			and #$f8			;XXX TODO could shift with asr and compress ones table?
							;XXX TODO with shift, bit 2 of twos is in carry and could be added as +0 +4?
			tay
			ldx #$3e
;13
			;add 2 bvs per speedzone?! XXX TODO
			;let's check out, on real hardware slower speedzones more and more miss reads if only 4 loop runs are given
			;6 bvs were enough?! XXX TODO
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop
			bvs .read_loop			;and use 2 bvs as safety zone? Would be pretty much save that it misses however, so need to find out
			bvs .read_loop			;2 cycle jitter only
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
			jmp .read_sector		;took too long, retry
;29
.gcr_end
			;Z-Flag = 1 on success, 0 on failure (wrong type)
			jmp .read_sector_back
.read_header_back	;XXX TODO coyp back to ZP
			pla				;header_0f_3
			;cmp #.HEADER_0F
			;bne +
			pla				;header_0f_2
!if .SANCHECK_HEADER_0F = 1 {
			cmp #.HEADER_0F
			bne -
}
			pla				;header_0f_1
!if .SANCHECK_HEADER_0F = 1 {
			cmp #.HEADER_0F
			bne -
}
			pla				;header_id2
			tay
			eor <.chksum2 + 1
+
			jmp .read_header_back_


;XXX TODO fill up with junk?
!ifdef .second_pass {
	!warn $0100 - *, " bytes remaining in zeropage."
}

!if >*-1 != >.read_loop { !error "read_sector not in one page: ", .read_loop, " - ", * }

			* = $200

			;----------------------------------------------------------------------------------------------------
			;
			; CODE TO SLOW DOWN GCR LOOP DEPENDING ON BITRATE
			;
			;----------------------------------------------------------------------------------------------------

.tab02200222_lo
.gcr_00
			lda ($00,x)	;8
			nop		;XXX TODO add a nop here for thcms floppy?
.gcr_20
			lda ($00,x)	;8
			nop
.gcr_40
			nop		;8
			lda $1c01
			jmp .gcr_slow1 + 3

			;----------------------------------------------------------------------------------------------------
			;
			; FRAGMENT FROM SEND CODE TO FILL UP GAP
			;
			;----------------------------------------------------------------------------------------------------
.scramble_preamble
			sty <.preamble_data + 0		;ack/status to set load addr, signal block ready
-
			lda <.preamble_data,x		;smaller this way, as lda $zp,x can be used now
			and #$0f
			tay
			lda <.preamble_data,x
			and #$f0
			eor .ser2bin,y
			sta <.preamble_data,x
			dex
			bpl -
			bmi .start_send
.tab2_gap1
			* = .tab02200222_lo + $22
!ifdef .second_pass {
	!warn * - .tab2_gap1, " bytes remaining for tab2_gap1."
}
                        !byte           $0e, $0a, ___, $00, $06, $02

			;----------------------------------------------------------------------------------------------------
			;
			; FRAGMENTS FROM SEEK CODE TO FILL UP GAPS
			;
			;----------------------------------------------------------------------------------------------------
.seek_
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
.stepping_speed		lda #.STEPPING_SPEED
			sta $1c09
			tya
			bpl .seek__
.tab2_gap2

			* = .tab02200222_lo + $42
!ifdef .second_pass {
	!warn * - .tab2_gap2, " bytes remaining for tab2_gap1."
}
                        !byte           $0f, $0b, $0d, $09, $0c, $08

.seek__
.halftrack
			eor $1c00
			sec
			rol
			and #3
			eor $1c00
			sta $1c00
			bit $1c09
			bmi *-3

			;XXX TODO skip final wait, do stepping when transferring last sektor? but also need to do it beforehand to be at right position for file?
			;XXX TODO dec stepping speed if not min for each turn after whoel seek, reset stepping speed?
			dex
			bne .step
+
			lda <.to_track		;already part of set_bitrate -> load track
			jmp .set_bitrate

.tab2_gap3
			* = .tab02200222_lo + $62
!ifdef .second_pass {
	!warn * - .tab2_gap3, " bytes remaining for tab2_gap1."
}
                        !byte           $07, $03, $05, $01, $04

			;----------------------------------------------------------------------------------------------------
			;
			; SEND PREAMBLE AND DATA
			;
			;----------------------------------------------------------------------------------------------------
.start_send
			ldx #$0a		;masking value for later sax $1800
			lda #.preloop - .branch - 2
			sta .branch + 1
			ldy #$00
.preloop
			lda <.preamble_data,y
			;adds 3 cycles to send, but preamble is bytewise fetched via jsr calls on resident side, so no timing issues
			jmp .preamble_entry
.send_sector_data
;branches do $bd, $cf
			;XXX TODO inx -> detect pre or data by x later on, saves two bytes, but timing is tight here already :-(
			lda #.sendloop - .branch - 2
			sta .branch + 1
			ldy <.send_end
			sty .pre_len + 1
			ldy #$00
.sendloop					;send the data block
.send_start = * + 1
			lda $0100,y
.preamble_entry
			bit $1800
			bmi *-3
			sax $1800		;76540213	-> dddd0d1d
			asl			;6540213. 7
			ora #$10		;654X213. 7	-> dddX2d3d
			bit $1800
			bpl *-3
			sta $1800
			ror			;7654X213 x
			asr #%11110000		;.7654... x	-> ddd54d.d
			bit $1800
			bmi *-3
			sta $1800
			lsr			;..7654..
			asr #%00110000		;...76...	-> ddd76d.d
.pre_len		cpy #$ff
			iny
			bit $1800
			bpl *-3
			sta $1800
.branch			bcc .sendloop
			;XXX carry is set here, always, might be useful somewhen
.sendloop_end
			lda .branch + 1
			eor #.sendloop - .branch - 2
			bne .send_sector_data
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

			dec <.blocks_on_list	;last block on wishlist?
			beq .track_finished
			jmp .cont_track
.track_finished
			;set stepping speed to $0c, if we loop once, set it to $18
			;ldx #$8c			;autoincrement to_track and take care of skipping track 18
			;dop
			lda #18
			ldx #$98
			top
-
			ldx #$98
			;sec			;set by send_block and also set if beq
			isc <.to_track
			beq -			;skip dirtrack however

			stx .stepping_speed + 1

			;XXX TODO make this check easier? only done hre?
			lda <.end_of_file	;EOF
			bmi .idle
			;jmp .idle -> $0c /4c? eof: ror lsr ora #$0c sta branch
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
			sty <.blocks_on_list	;clear, as we didn't reach the dec <.blocks_on_list on this code path
-
			pla
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x		;swap bits 3 and 0
			dey
			sta .directory,y
			bne -

			lax <.filenum
			cmp #BITFIRE_REQ_DISC
			bcc .load_file_		;XXX TODO bcc .load_file but bus_lock gets in our way, we should jmp there to keep distances short

			eor .dir_diskside	;compare side info
			bne .turn_disc		;still wrong side
			sta <.filenum		;reset filenum
			top

			;----------------------------------------------------------------------------------------------------
			;
			; ENTRY POINT OF IDLE LOOP
			;
			;----------------------------------------------------------------------------------------------------
.idle
			inc <.filenum		;autoinc always, so thet load_next will also load next file after a load with filenum
!if BITFIRE_DECOMP = 1 {
			dec <.first_block	;-> $ff
}
			lda $1c00		;turn off LED
			and #.LED_OFF
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 {
			and #.MOTOR_OFF
}
			sta $1c00

			;----------------------------------------------------------------------------------------------------
			;
			; RECEIVE/WAIT FOR A BYTE FROM HOST
			;
			;----------------------------------------------------------------------------------------------------

			;remove as next step, incl. check
.get_byte
			ldy #.BUSY		;enough time for signal to settle
.get_byte_
.lock
			lda #$80		;execpt a whole new byte and start with a free bus
			sta $1800

			ldx $1800		;sanity check, do only enter loop if $1800 = 0 and $dd02 = $3f, can happen on turndisk, $dd02 is $1f afterwards? (last bit is set in filename)
			bne *-3
.wait_bit1
			cpx $1800
			beq .wait_bit1		;change in $1800
			ldx $1800
			bmi .lock		;atn is high? violation
			;cpx #$04		;clock is not set, but data?
			;bcc .lock		;then lock, it is a protocoll violation
			cpx #$05
			ror
.wait_bit2
			cpx $1800
			beq .wait_bit2
			ldx $1800
			cpx #$01
			ror

			bcc .wait_bit1		;more bits to fetch?
			sty $1800		;set busy bit

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
			sta <.filenum		;set new filenum
+
			cmp #BITFIRE_REQ_DISC	;sets carry if so, used later on on bcs
			lda #.MOTOR_ON
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 {
!if .BOGUS_READS > 0 {
			ldx #.BOGUS_READS
			stx <.bogus_reads
}
}
			ora $1c00		;turn on motor (no matter if already on)
			bcs +			;no LED during turn disc
			ora #.LED_ON
+
			sta $1c00

			bcc +

			;----------------------------------------------------------------------------------------------------
			;
			; TURN DISC / LOAD NEW DIR SECTOR
			;
			;----------------------------------------------------------------------------------------------------
.turn_disc
			ldy #.DIR_SECT		;first dir sector
.load_dir_sect
			tya
			sta <.dir_sector
!if .SANCHECK_HEADER_ID = 1 {
			inc .en_set_id		;disable id-check, as new disc side can mean, new id
}
			ldx #$4c
			stx .en_dis_td		;enable jump back
			ldx #.DIR_TRACK		;set target track to 18
			stx <.to_track
			ldx #$00
			stx .is_loaded_track	;throw away preloaded sector, to avoid skip of read_sector
			stx <.last_block_num	;end at index 0
			jmp .turn_disc_entry
+
			;----------------------------------------------------------------------------------------------------
			;
			; LOAD A FILE
			;
			;----------------------------------------------------------------------------------------------------

			lax <.filenum		;load filenum
.load_file_
			ldy #.DIR_SECT - 1	;second dir sector
			sbc #$3e		;carry is cleared
			bcs +			;no underflow, filenum is >= $3f
			txa
			iny			;select first dir sector
+
			cpy <.dir_sector	;is this dir sector loaded?
			bne .load_dir_sect

			asl			;shift by 4 to get index into dir
			asl
			sta <.dir_entry_num	;and save

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
			beq .next_track

			lda <.blocks + 0
			sec
			adc .dir_file_size,x
			sta <.blocks + 0

			;XXX TODO on very huge files, this could fail, as we overflow!!! but then again, files are max $d000 in size due to i/o limitation?
			lda <.blocks + 1
			adc .dir_file_size + 1,x
			sta <.blocks + 1
.next_track
			jmp .set_max_sectors			;setup max_sectors
.ms_back1
			cpx <.dir_entry_num
			beq .found_file

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
			txa
			sbx #-4
			bne .next_dir_entry
			;XXX TODO this line of code should never be reached
.found_file
			;store track
			sty <.to_track

			;remember file index
			stx .file_index

			lda .dir_file_size + 0,x
			ora .dir_file_size + 1,x
			ora .dir_file_size + 2,x
			ora .dir_file_size + 3,x
			bne +
			;file not found
			;lda $1c00
			;and #.MOTOR_OFF
			;ora #.LED_ON
			;sta $1c00
			;jam
			jmp .idle
+
			lda <.blocks + 0
			;calc first block size
			eor #$ff
			sta <.first_block_size

			;XXX TODO test corner cases, write $0100 big file at beginning, write several small files and load them

			;lda <.first_block_size
			cmp .dir_file_size + 0,x		;compare with file_size

			lda .dir_file_size + 0,x
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
			adc #BITFIRE_CONFIG_INTERLEAVE
			cmp <.max_sectors
			bcc +
								;next revolutiona
!if BITFIRE_CONFIG_INTERLEAVE = 4 {
			adc #$00				;increase
			and #$03				;modulo 4
} else {
			adc #$00				;increase
-
			sec					;modulo INTERLEAVE
			sbc #BITFIRE_CONFIG_INTERLEAVE		;subtract some #sectors
			bcs -					;until underflow
			adc #BITFIRE_CONFIG_INTERLEAVE
}
+
			dex
			bne .fs_loop
.found_sector
.turn_disc_entry
			sta <.sector
			stx <.index				;reset block index, x = 0
								;a = file_size highbyte, carry set
.load_track
			;----------------------------------------------------------------------------------------------------
			;
			; SEEK
			;
			;----------------------------------------------------------------------------------------------------
.seek
			lax <.to_track
			jmp .seek_

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP BITRATE, MODIFY GCR LOOP IN ZEROPAGE
			;
			;----------------------------------------------------------------------------------------------------

.set_bitrate
			ldx #$98
			stx .stepping_speed + 1
			tay
!if .SANCHECK_TRACK = 1 {
			ldx #$0f
			sbx #$00
			and #$f0
			eor .ser2bin,x
			sta <.track_frob	;needs to be precacled here
}
			ldx #$ff
.set_max_sectors
			lda #16
			cpy #25
			adc #0
			cpy #31
			adc #0			;a is 12, 11, 10 ...
			eor #3			;a is 11, 12, 13 now
						;XXX TODO this is the value that coul be directly fed into the shifting below :-(
			cpy #18
			bcs +			;N = 0
			adc #2			;or 15 if < track 18 -> N = 0
+
			sta <.max_sectors
			sbc #$11		;carry still set depending on cpy #18
			asl
!if .SANCHECK_BVS_LOOP = 1 {
			sta .br0 + 1
}
			asl			;shift to right position
			asl
			asl
			asl

			cpx #$ff
			beq +
.ms_select		jmp .ms_back1
+
						;XXX TODO, all other bits cleared, can use and + ora now?
			ora #$0f		;preserve led, motor and stepper-bits
			tax                     ;0xx01111
			lda $1c00
			ora #$60		;x11xxxxx new bitrate bits to be set
			sax $1c00		;merge
!if .SANCHECK_BVS_LOOP = 1 {
			ldy #$a9		;restore 3 bvs
			sty <.bvs_01
			sty <.bvs_02
			sty <.bvs_03

			ldy #$70
.br0			bne *
			sty <.bvs_03
			sty <.bvs_02
			sty <.bvs_01
}
			txa

			ldy #$4c		;common values to set up for 1, 2, 3
			ldx #>.gcr_40		;should be $02

			and #$60
			beq .bitrate_3		;a = $00		;a bit pity that this needs another bunch of branches :-( TODO
			cmp #$40
			beq .bitrate_1		;a = $40
			lda #<.gcr_20		;03
			bcc .bitrate_2		;a = $20
						;a = $60
.bitrate_0
			ldy #$ad
			lda #$01
			ldx #$1c
			top
.bitrate_1
			lda #<.gcr_40		;06	XXX TODO 0,3,6 -> derivate from bitrate? -> asl + self?
.bitrate_2
.bitrate_3
			sty <.gcr_slow1 + 0
			sta <.gcr_slow1 + 1
			stx <.gcr_slow1 + 2

			;----------------------------------------------------------------------------------------------------
			;
			; LOAD ALL NEEDED SECTORS OF A TRACK AS MARKED ON WANTED-LIST
			;
			;----------------------------------------------------------------------------------------------------

			lda <.sector		;now crate our wishlist for the current track
.wanted_loop
			tax
			ldy <.index		;get index
			sty <.wanted,x		;write index into wantedlist
			inc <.blocks_on_list	;count number of blocks in list (per track num of blocks)
			cpy <.last_block_num	;inc index and check if index > file_size + 1 -> EOF
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
			clc			;XXX TODO should not happen and not needed?
}
			bne .wanted_loop	;if zero, done
						;carry is 0
						;just track is full
			sta .sector		;start next track with sector = 0
.load_wanted_blocks				;read and transfer all blocks on wishlist
			ror <.end_of_file	;shift in carry for later check, easiest way to preserve eof-state, if carry is set, we reached EOF
.cont_track
			lda <.is_loaded_track	;is the sector we hold in ram the one we need?
			cmp <.track
			bne .rs_cont		;nope

			ldx <.is_loaded_sector	;track is okay, sector too?
						;XXX TODO, why restricting this to index 0? Could in theory also be any other valid block, but saves code this way and as this block is forced, other cases should not happen
			ldy <.wanted,x		;check with wanted list if it is the first sector in our chain
			bne .rs_cont		;not requested, load new sector
			jmp .wipe_from_wanted
.rs_cont
			;----------------------------------------------------------------------------------------------------
			;
			; READ A SECTOR WITH HEADER AND DO VARIOUS SANITY CHECKS ON IT
			;
			;----------------------------------------------------------------------------------------------------

.read_sector
.read_gcr_header					;read_header and do checksum, if not okay, do again
			ldx #$07			;bytes to fetch
			ldy #$52			;type (header)
.read_gcr
			txa				;A is either $ff or $07
			asl				;$fe or $0e
			and #$4c			;$0c/$4c -> top or jmp
			sta <.gcr_end			;setup return jump
			eor #$2c
			sta .header_t2 + 1		;$20 or $60 depending if header or sector, just the right values we need there
			txs
!if .SANCHECK_FULL_SYNC = 1 {
			ldx #$00
			top
.still_sync
			ldx #$01
.wait_sync
			bit $1c00			;wait for end of sync
			bmi .still_sync
			txa
			beq .wait_sync			;no loop run taken, so fell through check
} else {
			bit $1c00			;wait for end of sync
			bmi *-3
}
			lda $1c01			;sync mark -> $ff
			clv
			bvc *
			clv
			cpy $1c01			;11111222
			bne .read_sector		;start over with a new header again, do not wait for a sectorheadertype to arrive
			bvc *
			lda $1c01			;22333334
			ldx #$3e
			sax <.threes + 1
			asr #$c1			;lookup? -> 4 cycles
.header_t2		eor #$c0
			bne .read_sector		;start over with a new header again, do not wait for a sectorheadertype to arrive
			nop
			pha
			pla
			pha
			pla
			lda #.EOR_VAL
			jmp .gcr_entry			;36 cycles until entry
.read_header_back_
			bne .read_sector

			pla				;header_id1
!if .SANCHECK_HEADER_ID = 1 {
			tax

.en_set_id		bcs .no_set_id			;will be enabled/disabled by increment/decrement, ends up as bcs or lda (xx),y, carry is always set due to preceeding cmp
			dec .en_set_id			;re-enable id-check
			sty <.current_id2
			stx <.current_id1		;fall through is no problem, tests will succeed
.no_set_id
			cpy <.current_id2
			bne .read_sector
			cpx <.current_id1
			bne .read_sector
}
			pla				;.header_track
!if .SANCHECK_TRACK = 1 {
			cmp <.track_frob		;needs to be precalced, else we run out of time
			bne .read_sector
}
			;XXX TODO, can only be $1x or 0x
			pla				;header_sector
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
!if BITFIRE_CONFIG_MOTOR_ALWAYS_ON = 0 {
!if .BOGUS_READS > 0 {
			lda <.bogus_reads
			beq +
			dec <.bogus_reads
			bne .read_sector
+
}
}
			ldy <.wanted,x			;sector on list?
!if .FORCE_LAST_BLOCK = 1 {
			cpy <.last_block_num		;current block is last block on list?
			bne .not_last			;nope continue
			ldx <.blocks_on_list		;yes, it is last block of file, only one block remaining to load?
			dex
			beq .last			;yes, so finally load last blovk
			bne .rs_retry2			;reread
}
.not_last
			iny
			bne .last			;if block index is $ff, we reread, as block is not wanted then
.rs_retry2
			jmp .read_sector		;will be sbc (xx),y if disabled
							;max 111/112 cycles passed, so still header_gap bytes flying by and we finish in time
.last
			;----------------------------------------------------------------------------------------------------
			;
			; HEADER DONE, NOW READ SECTOR DATA
			;
			;----------------------------------------------------------------------------------------------------

			ldx #$ff			;bytes to fetch
			ldy #$55			;type (sector)
			jmp .read_gcr
.read_sector_back
			;4 cycles of 13 passed, another 9 can pass?
			clv
			eor $0101
			sta .eor + 1
							;cheksum
			lax $1c01			;44445555
			bvc *
			arr #$f0
			clv				;after arr, as it influences v-flag
			tay				;44444---

!if .SANCHECK_TRAILING_ZERO = 1 {
			lda #$0f
			sbx #.CHECKSUM_CONST1		;4 bits of a trailing zero after checksum
			bne .rs_retry2			;check remaining nibble if it is $05
}

			;XXX TODO load those values when loading a dir sector and set them up just like the ID?
			ldx $1c01
			bvc *
!if .SANCHECK_TRAILING_ZERO = 1 {
			cpx #.CHECKSUM_CONST2		;0 01010 01 - more traiing zeroes
			bne .rs_retry2
}
			lda $1c01
!if .SANCHECK_TRAILING_ZERO = 1 {
			and #$e0
			cmp #.CHECKSUM_CONST3 & $e0	;010 xxxxx - and more trailing zeroes, last nibble varies on real hardware
			bne .rs_retry2
}
			ldx <.threes + 1
							;chksum
			lda $00,x
			ora .tab44444000_lo,y
			eor $0102
.eor			eor #$00
			eor <.chksum + 1		;XXX TODO annoying that last bytes nned to be checksummed here :-(
			bne .rs_retry2			;checksum okay? Nope, take two hops to get to the beginning of code again
							;counter dd db d8 d6
			;XXX TODO -> set this once anayway per track? but after send?
			ldx <.track			;remember T/S for later check if sector is cached
			stx <.is_loaded_track

			;XXX TODO a lot of wanted reads and decisions made, can they be aggregated?!?!?!?!
			ldx <.is_loaded_sector		;XXX TODO is check already above, can we remember result?
.wipe_from_wanted
			ldy #$ff			;blocksize full sector ($ff)
			lda <.wanted,x			;grab index from list (A with index reused later on after this call), also a is loaded last this way and we can directly check flags afterwards
			sty <.wanted,x			;clear entry in wanted list

.send_data
.en_dis_td		top .turn_disc_back		;can be disabled and we continue with send_data, else we are done here already

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP SEND LOOP, AND DECIDE BLOCK SIZES FIRST
			;
			;----------------------------------------------------------------------------------------------------


			cmp <.last_block_num		;compare once when code-path is still common, carry is not tainted until needed
			sta <.block_num
			tax
			bne .is_not_first_block
.is_first_block
			ldy <.first_block_size
			;lda #$00			;a is 0 already
			;cmp <.last_block_num		;do compare beforehand for both cases
			bcc .set_positions		;bcs = last_block, so a small file that starts and ends in same sector, bcc = big_file, all done
			tya
			clc
			adc <.blocks + 0
			jmp .set_positions - 2
.is_not_first_block
			;ldy #$ff			;y is $ff already
			;cmp <.last_block_num
			bcc .full_block			;a is 0 already
.last_block
			ldy <.last_block_size
.full_block
			tya
			eor #$ff
.set_positions
			sta .send_start			;setup start/end-position
			sty <.send_end

			;full block:
			;send_start = 0 - 1 - full_block_size
			;send_end = full_block_size

			;last block
			;send_start = 0 - 1 - last_block_size
			;send_end = last_block_size

			;first block till end of block
			;send_start = 0 - block_pos
			;send_end = first_block_size

			;first block small
			;sned_start = block_pos - 1 - first_block_size
			;send_end = first_block_size

			;----------------------------------------------------------------------------------------------------
			;
			; SET UP HEADERDATA FOR BLOCK TO BE SENT
			;
			;----------------------------------------------------------------------------------------------------
.preamble						;y = blocksize
							;XXX TODO check if something can be optimized, ldx file_index done twice, and tay done three times, this sucks
							;XXX TODO make this more readable and understandable :-(
			iny				;set up num of bytes to be transferred
			sty <.preamble_data + 3 + BITFIRE_DECOMP

			ldx <.file_index
			lda .dir_load_addr + 0,x	;fetch load address lowbyte
			sec				;neutralize upcoming sbc #$00

			ldy <.block_num			;first block? -> send load address, neutralize sbc later on, carry is set
			beq +
			ldy #$fc
			adc <.first_block_size		;else add first block size as offset
+
			sta <.preamble_data + 2 + BITFIRE_DECOMP	;block address low

			lda .dir_load_addr + 1,x	;add load address highbyte
			sta <.load_addr_hi
			sbc #$00			;subtract one in case of overflow
			clc
			adc <.block_num			;add block num
			sta <.preamble_data + 1 + BITFIRE_DECOMP	;block address high

			tya				;block_num == 0? -> first block of file?
			;XXX TODO same decision here again?
			beq .not_first			;y = $00 or $fc, depending on that, on $00 block-address is copied over as load-address on resident part
			lda #$00			;load default for barrier = 0
							;now set barrier to 0 to always make barrier-test fail, until first_block is transferred and load-address is set correctly
!if BITFIRE_DECOMP = 1 {				;no barriers needed with standalone loadraw
			cmp <.first_block		;at or past first_block?
			bne .not_first2			;not yet, default = 0 is already loaded, finish
.not_first
			sta <.first_block		;remember that first block has past for all upcoming blocks
			lda <.index			;max index to match against
			ldx #$14			;walk through list of sectors to be loaded
.min_loop
			cmp <.wanted,x			;compare
			bcc .is_bigger			;bigger index, next please
			lda <.wanted,x			;smaller (or same, but can't happen, as index is unique) remember new minimum
.is_bigger
			dex				;next entry
			bpl .min_loop

			clc
			adc <.load_addr_hi		;add load-address to barrier to make it compareable to block-pointers on resident side
			;clc
			sbc #$00			;subtract one, to give loader time to be >=$100 ahead (first partly filled sector + full sector)
.not_first2
			sta <.preamble_data + 1		;barrier, zero until set for first time, maybe rearrange and put to end?
} else {
.not_first
}
			;sty <.preamble_data + 0	;moved to .scramble_preamble to save bytes

			ldx #$03 + BITFIRE_DECOMP	;with or without barrier, depending on stand-alone loader or not
			stx .pre_len + 1		;set preamble size

			jmp .scramble_preamble

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
.tab00088888_lo		= .tables + $00

;table wth no offset
.tab77700077_hi		= .tables + $00

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

	 		* = .tables + $21
.table_start
			!byte      $f0, $22, $e0, $24, $25, $26, $27, $28, $0e, $0f, $07, $2c, $0a, $0b, $03
                        !byte $30, $31, $0d, $05, $34, $00, $09, $01, $38, $06, $0c, $04, $3c, $02, $08, $3f
                        !byte $40, $70, $50, $d0, $44, $45, $46, $47, $f0, $0e, $0f, $07, $0e, $0a, $0b, $03
                        !byte $70, $51, $0d, $05, $0f, $00, $09, $01, $60, $06, $0c, $04, $07, $02, $08, $5f
                        !byte $60, $60, $40, $c0, $64, $65, $66, $67, $b0, $0e, $0f, $07, $0a, $0a, $0b, $03
                        !byte $30, $71, $0d, $05, $0b, $00, $09, $01, $20, $06, $0c, $04, $03, $02, $08, $7f
.ser2bin
;        0b00001010,     00
;        0b00001011,     01
;        0b00010010,     02
;        0b00010011,     03
;        0b00001110,     04
;        0b00001111,     05
;        0b00010110,     06
;        0b00010111,     07
;        0b00001001,     08
;        0b00011001,     09
;        0b00011010,     0a
;        0b00011011,     0b
;        0b00001101,     0c
;        0b00011101,     0d
;        0b00011110,     0e
;        0b00010101      0f


			;lax src,y
			;and #$09
			;beq +
			;eor #$09
			;beq +
			;txa
			;eor #$09
			;dop
			;+
			;txa
			;eor #$7f

;			ldx #$09
;			sbx #$00
;			beq +
;			cpx #$09
;			beq +
;			eor #$09
;+
;			eor #$7f

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
                        !byte $a0, $b0, $80, $a0, $a4, $a5, $a6, $a7, $80, $0e, $0f, $07, $00, $0a, $0b, $03
                        !byte $10, $b1, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, $bf
                        !byte $c0, $30, $10, $90, $c4, $c5, $c6, $c7, $e0, $0e, $0f, $07, $06, $0a, $0b, $03
                        !byte $d0, $d1, $0d, $05, $0c, $00, $09, $01, $c0, $06, $0c, $04, $04, $02, $08, $df
                        !byte $e0, $20, $00, $e3, $e4, $e5, $e6, $e7, $a0, $0e, $0f, $07, $02, $0a, $0b, $03
                        !byte $90, $f1, $0d, $05, $08, $00, $09, $01, $f8, $06, $0c, $04, $fc, $02, $08, $ff

;                        !byte      $f0, ___, $e0, ___, ___, ___, ___, ___, $0e, $0f, $07, ___, $0a, $0b, $03
;                        !byte ___, ___, $0d, $05, ___, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
;                        !byte ___, $70, $50, $d0, ___, ___, ___, ___, $f0, $0e, $0f, $07, $0e, $0a, $0b, $03
;                        !byte $70, ___, $0d, $05, $0f, $00, $09, $01, $60, $06, $0c, $04, $07, $02, $08, ___
;                        !byte ___, $60, $40, $c0, ___, ___, ___, ___, $b0, $0e, $0f, $07, $0a, $0a, $0b, $03
;                        !byte $30, ___, $0d, $05, $0b, $00, $09, $01, $20, $06, $0c, $04, $03, $02, $08, ___
;                        !byte ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___, ___
;                        !byte $50, ___, ___, ___, $0d, ___, ___, ___, $40, ___, ___, ___, $05, ___, ___, ___
;                        !byte ___, $b0, $80, $a0, ___, ___, ___, ___, $80, $0e, $0f, $07, $00, $0a, $0b, $03
;                        !byte $10, ___, $0d, $05, $09, $00, $09, $01, $00, $06, $0c, $04, $01, $02, $08, ___
;                        !byte ___, $30, $10, $90, ___, ___, ___, ___, $e0, $0e, $0f, $07, $06, $0a, $0b, $03
;                        !byte $d0, ___, $0d, $05, $0c, $00, $09, $01, $c0, $06, $0c, $04, $04, $02, $08, ___
;                        !byte ___, $20, $00, ___, ___, ___, ___, ___, $a0, $0e, $0f, $07, $02, $0a, $0b, $03
;                        !byte $90, ___, $0d, $05, $08, $00, $09, $01, ___, $06, $0c, $04, ___, $02, $08, ___
}

.drivecode_end
.drivecode_size = .drivecode_end - .drivecode_start

.second_pass

;XXX TODO faster xfer: ;jmp ($1800) possible? 

;if in carry: rol asl asl or ror lsr if $40?
;and #$04 or and #$40? would be bit 6

;XXX TODO merge high and lownibbles in one table and separate by and #$0f, possible if we save data due to that and also cycles

;11111000 table fits into zp if compressed with asr #$f8, preamble then starts @ $89, zero bytes free then, but fits

;halfstep, send_data, timer elapsed? else wait rest, halfstep, wait
;XXX TODO
;XXX TODO change interleave depending on track (via set max_sectors?)

;XXX TODO intermediate stepping: half step, send $80 bytes if full block, else do normal stepping, half step, send another $80 bytes, keep barrier to old value on first block, second block: update addr and barrier on preamble
