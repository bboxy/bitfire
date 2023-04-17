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
		;lda $d011
		;bmi *-3
		;lda $d011
		;bpl *-3

		lda #<.detect_2
		sta $fffa
		lda #>.detect_2
		sta $fffb

		lda $dd0d
		lda #$81
		sta $dd0d

		lda #$04
		sta $dd04
		lda #$00
		sta $dd05

		sta $02

		lda #%10011001
		sta $dd0e

		lda $dd0d
		lda $dd0d
		inc $02
		jmp *

.detect_2
		lda $dd0d
		pla
		pla
		pla
		lda $02
		asl
		eor #$02
		ora link_chip_types
		sta link_chip_types

		lda #$7f
		sta $dd0d
		lda $dd0d

		lda #$37
		sta $01
