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

.detect_sid
		sei
		lda #$35
		sta $01
		lda #$00
		sta $d015
		lda #$ff
		cmp $d012
		bne *-3

		lda #$ff
		sta $d412
		sta $d40e
		sta $d40f
		lda #$20
		sta $d412
		lda $d41b
		eor #$01
		and #$01
		ora link_chip_types	;0 = old, 1 = new sid
		sta link_chip_types

.detect_cia
		lda #$7f
		sta $dd0d
		sta $dc0d
		lda $dd0d
		lda $dc0d

		lda #$00
		sta $d01a
		sta $dd0e
		sta $dd0f
		sta $dc0e
		sta $dc0f
		sta $dd0e
		sta $dd0f
		sta $dd05
		sta $dd07
		sta $dc05
		sta $dc07

		inc $d019

		lda #<.detect_2
		sta $fffa
		lda #>.detect_2
		sta $fffb

		lda #$02
		sta $dd04
		sta $dc04
		lda #%10000001
		sta $dd0d
		lda #%00011001
		sta $dd0e
		tsx

		nop
		lda #BITFIRE_CIA2_NEW
		lda #$00
.detect_2
		ora link_chip_types
		sta link_chip_types

		lda #$7f
		sta $dd0d
		bit $dd0d

		lda #<.detect_1
		sta $fffe
		lda #>.detect_1
		sta $ffff

		lda #%10000001
		sta $dc0d
		lda #%00011001
		sta $dc0e
		cli

		nop
		lda #BITFIRE_CIA1_NEW
		lda #$00
.detect_1
		ora link_chip_types
		sta link_chip_types

		lda #$7f
		sta $dc0d
		bit $dc0d

		txs

		lda #$37
		sta $01
