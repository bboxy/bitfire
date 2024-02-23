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

CHECKSUM = 1
CHECKSUM_CLEAR = 1
REQDISC = 1
BUSLOCK = 0
WAIT_SPIN_DOWN = 1

TIME_RAW = 1
TIME_LOADCOMP = 0
TIME_RAW_DECOMP = 0
TIME_DECOMP = 0

TIME_STRICT = 0

num_files	= $16
;num_files	= $11

runs		= $10
cnt		= $16
dst		= $18
dst_		= $1a
prnt		= $1c
prnt_		= $1e
err		= $24
endh		= $26
endc		= $27

accum		= $30

screen		= $2000

!src "../config.inc"
!src "../loader/loader_acme.inc"
!src "../macros/link_macros_acme.inc"

		* = $1000
!bin "../loader/installer",,2

		* = $0900
		lda #$0b
!if TIME_STRICT == 1 {
		sta $d011
}
		sta $d020
		sta $d021

		lda #$84
		sta $d018

		ldx #$00
		stx err
		stx err + 1
!if CONFIG_DEBUG == 1 {
		stx bitfire_error
		stx bitfire_error_acc
}
-
		lda #$20
		sta screen + $0000,x
		sta screen + $0100,x
		sta screen + $0200,x
		sta screen + $0300,x
		dex
		bne -

		jsr bitfire_install_

		sei
		ldx #$ff
		txs

		lda #$35
		sta $01

		lda #<nmi
		sta $fffa
		lda #>nmi
		sta $fffb

		lda #$7f
		sta $dc0d
		sta $dd0d
		lda $dc0d
		lda $dd0d
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
		jmp benchmark

display
		lda #<screen
		sta prnt + 0
		lda #>screen
		sta prnt + 1

		ldx #$01
		ldy #$00
-
		txa
		sta (prnt),y
		lda prnt + 1
		pha
		and #$03
		ora #$d8
		sta prnt + 1
		lda #$0f
		sta (prnt),y
		pla
		sta prnt + 1

		lda prnt + 0
		clc
		adc #$28
		sta prnt + 0
		bcc +
		inc prnt + 1
+
		inx
		cpx #num_files + 1
		bne -

		ldx #$12
-
		lda text4,x
		sta screen + 23 * 40,x
		lda #$0f
		sta $d800 + 23 * 40,x
		dex
		bpl -

		ldx #20
-
		lda text3,x
		sta screen + 22 * 40,x
		lda #$0c
		sta $d800 + 22 * 40,x
		dex
		bpl -

		ldx #21
-
		lda text7,x
		sta screen + 24 * 40,x
		lda #$0c
		sta $d800 + 24 * 40,x
		dex
		bpl -
		rts
reset
		lxa #0
		stx numb+1
		stx cnt
		stx cnt+1
-
		sta $1000,x
		sta $1100,x
		sta $1200,x
		dex
		bne -
		rts

nmi
		inc $d021
		rti

text3
		!text "#    cycles   err acc"
text4
		!text "runs: $0000   $0000"
text7
		!text "caps-lock for checksum"

benchmark
		ldx #$00

		lda #$20
-
		sta screen + $0000,x
		sta screen + $0100,x
		sta screen + $0200,x
		sta screen + $0300,x
		inx
		bne -

		lda #$00
		ldx #$19
-
		sta accum,x
		dex
		bpl -

		sta runs
		sta runs+1

		lda #$20
		sta dst + 0
		lda #>screen
		sta dst + 1

		lda #<($d800)
		sta prnt_ + 0
		lda #<($d800 + $20)
		sta dst_ + 0
		lda #>($d800)
		sta prnt_ + 1
		sta dst_ + 1

		jsr display
		jsr reset

next
;		lda #$e0
;		sta link_blk + 2
numb		lda #$00		;file number
		tax
		pha
		lda #$01
		jsr setcol
!if BUSLOCK == 1 {
		+bus_lock		;raise ATN and lock bus, does it help to set bit 6 + 7 for output? Had problems on sx-64 with all the buslock and maybe drifting of raise/fall times

		ldy #$00
		lda irq,y
-
		sta $dd00
		pha
		pla
		pha
		pla
		pha
		pla
		pha
		pla
		pha
		pla
		pha
		pla
		pha
		pla
		nop
		bit $ea
		dey
		bne -
}
		lda #$03
		sta $dd00
!if BUSLOCK == 1 {
		+bus_unlock
		;jmp next
}

!if WAIT_SPIN_DOWN == 1 {
		ldy #$1c
-
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		dey
		bne -
}

!if TIME_RAW == 1 {
		jsr .start_timer
		pla
		pha
		;clc
		;adc #1
		jsr bitfire_loadraw_
}
!if TIME_DECOMP = 1 or TIME_RAW_DECOMP = 1 {
!if TIME_RAW_DECOMP = 1 {
		jsr .start_timer
}
		pla
		pha
		;clc
		;adc #1
		jsr bitfire_loadraw_
!if TIME_DECOMP = 1 {
		jsr .start_timer
}
		jsr bitfire_decomp_
}
!if TIME_LOADCOMP = 1 {
		jsr .start_timer
		pla
		pha
		;clc
		;adc #1
		jsr bitfire_loadcomp_
}
		pla
		asl
		asl
		tax
		jsr .stop_timer

!if CHECKSUM == 0 {
;		lda #$fd
;		sta $dc00
;		lda $dc01
;		cmp #$7f
;		bne +
}
		ldx numb + 1
!if CONFIG_DEBUG == 1 {
		lda accum,x
		clc
		adc bitfire_error_acc
		sta accum,x
}

!if CHECKSUM == 1 & TIME_STRICT != 1 {
		jsr checksum
}
+
!if CONFIG_DEBUG == 1 {
		lda err
		clc
		adc bitfire_error_acc
		sta err
		bcc +
		inc err + 1
+
		lda #$00
		sta bitfire_error_acc
}

		inc numb+1
		lda numb+1
		cmp #num_files
		beq +
		jmp next
+

		jsr display
		jsr print_count
		jsr hex_runs
		jsr reset
!if TIME_STRICT == 1{
		lda #$1b
		sta $d011
		jmp *
}

!if REQDISC == 1 {
		+request_disk 0
}
		jmp next

irq
		pha
		lda #$37
		sta $00
		sta $01
		;pha
		dec $d020
		dec $d019
		bit $dc0d
		bpl +
		inc $d021
+
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
		bne +
		inc cnt+1
+
		inc $d020
		lda #$35
		sta $01
		pla
		rti
checksum
		lax numb+1
		asl
		tay
		lda #$07
		jsr setcol

		lda sizes + 1,y
		sta endh
!if CHECKSUM_CLEAR == 1 {
		sta endc
}
		lax sizes,y
		clc
		adc loads,y
		sta srch
!if CHECKSUM_CLEAR == 1 {
		sta srcd
}
		txa
		eor #$ff
		tax
		inx
		beq +
		inc endh
		inc endc
+
		lda loads + 1,y
		sbc #$00
		sta srch + 1
!if CHECKSUM_CLEAR == 1 {
		sta srcd + 1
}
		lda #$00
-
		clc
srch = * + 1
		adc $1000,x
;!if CHECKSUM_CLEAR == 1 {
;srcd = * + 1
;		sta $1000,x	;overwrite with junk
;}
		inx
		bne -
		inc srch + 1
;!if CHECKSUM_CLEAR == 1 {
;		inc srcd + 1
;}
		dec endh
		bne -

		ldx numb+1
		cmp chksums,x
		bne no

!if CHECKSUM_CLEAR == 1 {
		lda #$69
-
srcd = * + 1
		sta $1000,x	;overwrite with junk
		inx
		bne -
		inc srcd + 1
		dec endc
		bne -
}
		ldx numb+1
		lda #$05
		jmp setcol

no
		lda #$02
		jsr setcol
reset_drv
		+reset_drive
		jmp *

setcol
		pha
		sty .y + 1
		stx .x + 1

		lda #$00
		ldy #$d8
-
		dex
		bmi .out
		clc
		adc #$28
		bcc +
		iny
+
		bne -
.out
		sta prnt
		sty prnt + 1

		pla
		ldy #$00
		sta (prnt),y

		cmp #$05
		beq rev
		cmp #$02
		bne +
rev
		lda prnt + 1
		and #$03
		ora #>screen
		sta prnt + 1
		lda (prnt),y
		eor #$80
		sta (prnt),y

		ldy #$05
		lda #$24
		sta (prnt),y
		iny
		lda numb + 1
		asl
		asl
		tax
		lda $0f01,x
		jsr print_
		lda numb + 1
		asl
		asl
		tax
		lda $0f02,x
		jsr print_
		lda numb + 1
		asl
		asl
		tax
		lda $0f03,x
		jsr print_

!if CONFIG_DEBUG == 1 {
		iny
		iny
		lda bitfire_error_acc
		jsr print_

		iny
		iny
		ldx numb + 1
		lda accum,x
		jsr print_
}

		lda prnt + 1
		and #$03
		ora #$d8
		sta prnt + 1

		ldy #$05
-
		lda #$0f
		sta (prnt_),y
		lda #$01
		sta (prnt),y
		iny
		cpy #$14
		bne -

		lda prnt + 0
		sta prnt_ + 0
		lda prnt + 1
		sta prnt_ + 1
+
.x		ldx #$00
.y		ldy #$00
		rts

print_
		pha
		lsr
		lsr
		lsr
		lsr
		tax
		lda hex,x
		sta (prnt),y
		iny
		pla
		and #$0f
		tax
		lda hex,x
		sta (prnt),y
		iny
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
-
		lda #$0f
		sta (dst_),y
		lda #$01
		sta (dst),y
		dey
		bpl -

		lda dst + 0
		sta dst_ + 0
		lda dst + 1
		sta dst_ + 1

		lda dst + 1
		and #$03
		ora #>screen
		sta dst + 1

		lda dst + 0
		cmp #$e0
		bne +
		lda #>screen
		sta dst + 1
		lda #$20
		sta dst + 0
		rts
+
		clc
		adc #$28
		sta dst + 0
		bcc +
		inc dst + 1
+
		rts

hex_runs
		inc runs+1
		bne +
		inc runs
+
		lda runs
		tax
		and #$0f
		tay
		lda hex,y
		sta screen + 23 * 40 + $8
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta screen + 23 * 40 + $7
		lda runs+1
		tax
		and #$0f
		tay
		lda hex,y
		sta screen + 23 * 40 + $a
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta screen + 23 * 40 + $9

		lda err + 1
		tax
		and #$0f
		tay
		lda hex,y
		sta screen + 23 * 40 + $10
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta screen + 23 * 40 + $0f
		lda err
		tax
		and #$0f
		tay
		lda hex,y
		sta screen + 23 * 40 + $12
		txa
		lsr
		lsr
		lsr
		lsr
		tay
		lda hex,y
		sta screen + 23 * 40 + $11
.barerts
		rts

.start_timer
                lda #$00
                sta $dd0e
                lda #$40
                sta $dd0f
                lda #$ff
                sta $dd04
                sta $dd05
                sta $dd06
                sta $dd07
                lda #$01
                sta $dd0e
                lda #$41
                sta $dd0f
                rts
.stop_timer
                lda #$00
                sta $dd0e
                lda #$40
                sta $dd0f

                lda $dd04
                eor #$ff
                sta $0f03,x
                lda $dd05
                eor #$ff
                sta $0f02,x
                lda $dd06
                eor #$ff
                sta $0f01,x
                lda $dd07
                eor #$ff
                sta $0f00,x
                rts


hex
		!text "0123456789abcdef"


sizes
!if TIME_RAW == 1 {
!word $c179-$b635	;a	00000000
!word $bf80-$6a62	;b	00000001
!word $bd00-$aa49	;c	00000010
!word $4900-$3eed	;d	00000011
!word $6600-$3ee5	;e	00000100
!word $4396-$3229	;f	00000101
!word $62d5-$5e47	;g	00000110
!word $2d00-$2b1f	;h	00000111
!word $4500-$36e9	;i	00001000
!word $6358-$54d3	;j	00001001
!word $6200-$3ab4	;k	00001010
!word $7300-$6954	;l	00001011
!word $67a1-$5822	;m	00001100
!word $bef7-$bb95	;n	00001101
!word $8000-$7950	;o	00001110
!word $af00-$a15a	;p	00001111
!word $666b-$3d1f	;q	00010000
!word $a800-$95d4	;r	00010001
!word $bf80-$6a62	;s	00010010
!word $bf80-$6a62	;t	00010011
!word $bf80-$6a62	;u	00010100
!word $6600-$3ee5	;v	00010101
} else {
!word $c179-$a000	;a
!word $bf80-$2800	;b
!word $bd00-$7400	;c
!word $4900-$2f80	;d
!word $6600-$2800	;e
!word $4396-$2800	;f
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
!word $bf80-$2800
!word $bf80-$2800
!word $bf80-$2800
!word $6600-$2800	;e
}

chksums
!if TIME_RAW == 1 {
!byte $2d
!byte $67
!byte $9f
!byte $e2
!byte $c3
!byte $65
!byte $45
!byte $1e
!byte $a3
!byte $09
!byte $e8
!byte $b3
!byte $91
!byte $80
!byte $c1
!byte $d3
!byte $31
!byte $7a
!byte $67
!byte $67
!byte $67
!byte $c3
} else {
!byte $ea
!byte $fd
!byte $8e
!byte $ad
!byte $3c
!byte $2b
!byte $8f
!byte $60
!byte $23
!byte $1a
!byte $28
!byte $c1
!byte $5e
!byte $85
!byte $c3
!byte $5e
!byte $1c
!byte $c3
!byte $fd
!byte $fd
!byte $fd
!byte $3c
}

loads
!if TIME_RAW == 1 {
!word $b635
!word $6a62
!word $aa49
!word $3eed
!word $3ee5
!word $3229
!word $5e47
!word $2b1f
!word $36e9
!word $54d3
!word $3ab4
!word $6954
!word $5822
!word $bb95
!word $7950
!word $a15a
!word $3d1f
!word $95d4
!word $6a62
!word $6a62
!word $6a62
!word $3ee5
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
!word $2800
!word $2800
!word $2800
!word $2800
}


;XXX TODO copy away loadercode and benchmark code to compare
;log barrier and block-adresses


;load from-to, cycles, size, depacked -> print all on screen, use pointer for source and y/x offset to print at?
