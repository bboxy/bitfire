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

!convtab scr
!cpu 6510

CHECKSUM = 0
REQDISC = 1
BUSLOCK = 0

TIME_RAW = 0
TIME_LOADCOMP = 1
TIME_DECOMP = 0

TIME_STRICT = 0

;num_files	= $1c
num_files	= $12

runs		= $10
max		= $12
min		= $14
cnt		= $16
dst		= $18

!src "../loader/loader_acme.inc"
!src "../macros/link_macros_acme.inc"

		* = $1000
!bin "../loader/installer",,2
		* = $0800
		lda $ba
		lda #$0b
!if TIME_STRICT == 1 {
		sta $d011
}
		sta $d020
		sta $d021
		ldx #$00
-
		lda #$20
		sta $0428,x
		sta $0500,x
		sta $0600,x
		sta $0700,x
		dex
		bne -
		lda #$00
		sta runs
		sta runs+1

		lda #$20
		sta dst
		lda #$04
		sta dst+1

		jmp benchmark

display
		ldx #$00
		lda #$0f
-
		sta $d800,x
		sta $d900,x
		sta $da00,x
		sta $db00,x
		dex
		bne -

		ldx #num_files
-
		txa
		ora #$80
		dex
		sta $0400+1*40,x
		bpl -

		ldx #$0e
-
		lda text1,x
		sta $0450,x
		lda text2,x
		sta $0478,x
		lda text3,x
		sta $04a0,x
		lda text4,x
		sta $04f0,x
		dex
		bpl -

		ldx #21
-
		lda text7,x
		sta $0518,x
		dex
		bpl -


		lda #$01
		sta $d850
		lda #$05
		sta $d878
		lda #$02
		sta $d8a0
		rts
reset
		lda #$00
		sta min
		sta min+1
		sta cnt
		sta cnt+1
		lda #$00
		sta numb+1
		lda #$ff
		sta max
		sta max+1
		rts


text1
		!byte $a0
		!text " loading      "
text2
		!byte $a0
		!text " checksum ok  "
text3
		!byte $a0
		!text " checksum fail"
text4
		!text "runs: $0000    "
text5
		!text "max: $0000     "
text6
		!text "min: $0000     "
text7
		!text "caps-lock for checksum"

benchmark
		jsr reset
		jsr display

		jsr bitfire_install_

		sei

		lda #$35
		sta $01

		lda #$7f
		sta $dc0d
		lda $dc0d
		lda #$01
		sta $d019
		sta $d01a
		lda #$ff
		sta $d012
		lda $d011
		and #$7f
		sta $d011
		lda #<irq
		sta $fffe
		lda #>irq
		sta $ffff
!if TIME_STRICT == 0 {
		cli
}

next
numb		lda #$00		;file number
		lax numb+1
		pha
		lda #$01
		sta $d828,x

!if BUSLOCK == 1 {
		+bus_lock		;raise ATN and lock bus, does it help to set bit 6 + 7 for output? Had problems on sx-64 with all the buslock and maybe drifting of raise/fall times

		nop
		nop
		nop
		nop

		ldy #$00
-
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		inc $dd00		;if we do a inc $dd00 here, this fails miserably on a sx-64 and will break the next send_byte o_O
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		dey
		bne -
;		ldy #$00
;-
;		nop
;		nop
;		nop
;		inc $dd00		;if we do a inc $dd00 here, this fails miserably on a sx-64 and will break the next send_byte o_O
;		dey
;		bne -

		nop
		nop
		nop
		nop

		+bus_unlock 3
}

!if TIME_RAW == 1 {
		jsr .start_timer
		pla
		pha
		jsr bitfire_loadraw_
}
!if TIME_DECOMP = 1 {
		pla
		pha
		jsr bitfire_loadraw_
		jsr .start_timer
		jsr bitfire_decomp_
}
!if TIME_LOADCOMP = 1 {
		jsr .start_timer
		pla
		pha
		jsr bitfire_loadcomp_
}
		pla
		asl
		asl
		tax
		jsr .stop_timer

!if CHECKSUM == 0 {
		lda #$fd
		sta $dc00
		lda $dc01
		cmp #$7f
		bne *+5
}
		jsr checksum

		inc numb+1
		lda numb+1
		cmp #num_files
		beq *+5
		jmp next

		jsr display
		jsr print_count
		jsr hex_runs
		jsr reset
!if TIME_STRICT == 1{
		jam
}

!if REQDISC == 1 {
.side		lda #$f0
		jsr req_disc
}
		jmp next

irq
		pha
		dec $d020
		dec $d019
;.bnk		lda #$00
;		sta $dd00
;		clc
;		adc #$01
;		and #$03
;		sta .bnk+1
;sau		lda #$00
;		cmp $d012
;		bne *-3
;		inc sau+1
;		lda $d012
;		bne *-3
;		bit $d011
;		bmi *-3
;		lda #$15+78+78
;		cmp $d012
;		bne *-3
		inc cnt
		bne *+4
		inc cnt+1
		inc $d020
		pla
		rti
checksum
		lax numb+1
		asl
		tay
		lda #$0f
		sta $d828,x
		lda loads,y
		sta src
		lda loads+1,y
		sta src+1

		lda sizes,y
		clc
		adc src
		sta endl
		lda sizes+1,y
		adc src+1
		sta endh

		lda #$00
-
src = * + 1
		eor $beef
		inc src
		bne +
		inc src+1
+
		ldx src
endl = * + 1
		cpx #$00
		bne -
		ldy src+1
endh = * + 1
		cpy #$00
		bne -
		ldx numb+1
		cmp chksums,x
		bne no

		lda src
		cmp $06
		;bne end_w
		lda src+1
		cmp $07
		;bne end_w

		lda #$05
		sta $d828+00*40,x

		;jsr clear
		;jsr clear
		;jsr clear
		;jsr clear
		;jsr clear
		;jsr clear
		jmp clear

no
		lda #$02
		sta $d828+00*40,x
		jmp reset_drv
end_w
		lda #$07
		sta $d828+00*40,x
reset_drv
		lda #$ff
		jsr bitfire_loadraw_
		;jsr bitfire_reset_drive_
		jmp *
req_disc
		+request_disk 0
		rts

clear
		lda #$10
		sta cln+2
--
		ldy #$00
-
		tya
cln
		sta $1000,y
		dey
		bne -
		inc cln+2
		lda cln+2
		cmp #$d0
		bne --
		rts

print_count
		ldy #$00
		lda #$24
		sta (dst),y
		iny
		lda cnt+1
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,x
		sta (dst),y
		iny
		lda cnt+1
		and #$0f
		tax
		lda hex,x
		sta (dst),y
		iny
		lda cnt
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,x
		sta (dst),y
		iny
		lda cnt
		and #$0f
		tax
		lda hex,x
		sta (dst),y

		lda dst+1
		and #$03
		ora #$d8
		sta dst+1

		ldy #$04
		lda #$01
-
		sta (dst),y
		dey
		bpl -

		lda dst+1
		and #$03
		ora #$04
		sta dst+1

		lda dst
		clc
		adc #$28
		sta dst
		bcc *+4
		inc dst+1

		lda dst+1
		cmp #$08
		bne +
		lda #$04
		sta dst+1
		lda #$20
		sta dst
+
		rts
hex_runs
		inc runs
		bne *+4
		inc runs+1

		lax runs+1
		and #$0f
		tay
		lda hex,y
		sta $04f8
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta $04f7
		lax runs
		and #$0f
		tay
		lda hex,y
		sta $04fa
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta $04f9
.barerts
		rts

.start_timer
                lda #$00
                sta $dc0e
                lda #$40
                sta $dc0f
                lda #$ff
                sta $dc04
                sta $dc05
                sta $dc06
                sta $dc07
                lda #$41
                sta $dc0f
                lda #$01
                sta $dc0e
                rts
.stop_timer
                lda #$00
                sta $dc0e
                lda #$40
                sta $dc0f

                lda $dc04
                eor #$ff
                sta $0f03,x
                lda $dc05
                eor #$ff
                sta $0f02,x
                lda $dc06
                eor #$ff
                sta $0f01,x
                lda $dc07
                eor #$ff
                sta $0f00,x
                rts


hex
		!text "0123456789abcdef"
sizes
!if TIME_RAW == 1 {
!word $c179-$b635
!word $bf80-$6a67
!word $bd00-$aa4a
!word $4900-$3eed
!word $6600-$3ee7
!word $4396-$322a
!word $62d5-$5e47
!word $2d00-$2b1f
!word $4500-$36e9
!word $6358-$54d2
!word $6200-$3ab4
!word $7300-$6954
!word $67a1-$5822
!word $bef7-$bb95
!word $8000-$7950
!word $af00-$a15a
!word $666b-$3d20
!word $a800-$95d4
} else {
!word $c179-$a000
!word $bf80-$2800
!word $bd00-$7400
!word $4900-$2f80
!word $6600-$2800
!word $4396-$2800
!word $62d5-$5c00
!word $2d00-$2800
!word $4500-$2900
!word $6358-$4800
!word $6200-$2800
!word $7300-$6100
!word $67a1-$2800
!word $bef7-$b900
!word $8000-$6600
!word $af00-$9000
!word $666b-$2800
!word $a800-$8000
!word $c179-$a000
!word $bf80-$2800
!word $bd00-$7400
!word $4900-$2f80
!word $6600-$2800
!word $4396-$2800
!word $62d5-$5c00
!word $2d00-$2800
!word $4500-$2900
!word $6358-$4800
}

chksums
!if TIME_RAW == 1 {
!byte $84
!byte $a0
!byte $eb
!byte $03
!byte $bd
!byte $e7
!byte $bf
!byte $ad
!byte $1c
!byte $48
!byte $b8
!byte $04
!byte $29
!byte $8d
!byte $9a
!byte $8a
!byte $22
!byte $ba
} else {
!byte $f2
!byte $d3
!byte $a6
!byte $6f
!byte $60
!byte $fd
!byte $59
!byte $42
!byte $bb
!byte $f6
!byte $fa
!byte $79
!byte $02
!byte $a5
!byte $35
!byte $14
!byte $34
!byte $1f
!byte $f2
!byte $d3
!byte $a6
!byte $6f
!byte $60
!byte $fd
!byte $59
!byte $42
!byte $bb
!byte $f6
}

loads
!if TIME_RAW == 1 {
!word $b635
!word $6a67
!word $aa4a
!word $3eed
!word $3ee7
!word $322a
!word $5e47
!word $2b1f
!word $36e9
!word $54d2
!word $3ab4
!word $6954
!word $5822
!word $bb95
!word $7950
!word $a15a
!word $3d20
!word $95d4
} else {
!word $a000
!word $2800
!word $7400
!word $2f80
!word $2800
!word $2800
!word $5c00
!word $2800
!word $2900
!word $4800
!word $2800
!word $6100
!word $2800
!word $b900
!word $6600
!word $9000
!word $2800
!word $8000
!word $a000
!word $2800
!word $7400
!word $2f80
!word $2800
!word $2800
!word $5c00
!word $2800
!word $2900
!word $4800
}

