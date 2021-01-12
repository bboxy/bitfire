!convtab scr
!cpu 6510

RAW = 1
CHECKSUM = 1
;REQDISC = 1

num_files	= $1c
;num_files	= $12

runs		= $10
max		= $12
min		= $14
cnt		= $16
dst		= $18

!src "../loader_acme.inc"
!src "../../link_macros_acme.inc"

		* = $1000
!bin "../installer",,2
		* = $0800
		lda #$0b
		sta $d020
		sta $d021
		ldx #$00
-
		lda #$20
		sta $0400,x
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

		ldx #num_files - 1
-
		txa
		ora #$80
		sta $0400+00*40,x
		dex
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
		lda #$1b
		sta $d011
		lda #<irq
		sta $fffe
		lda #>irq
		sta $ffff
		cli

next
!ifdef BITFIRE_TRANSFER_CHECKSUM {
		ldx #$00
		lda #$00
-
		sta $0f00,x
		dex
		bne -
}
numb		lda #$00		;file number
		lax numb+1
		pha
		lda #$01
		sta $d800,x

;		lda #$c7		;raise ATN and lock bus, does it help to set bit 6 + 7 for output? Had problems on sx-64 with all the buslock and maybe drifting of raise/fall times
;		sta $dd02
;
;		nop
;		nop
;
;		ldy #$00
;-
;		nop
;		nop
;		nop
;		sty $dd00		;if we do a inc $dd00 here, this fails miserably on a sx-64 and will break the next send_byte o_O
;		dey
;		bne -
;
;		nop
;		nop
;
;		lda #$c3
;		sta $dd00		;set bits
;
;		lda #$3f		;unlock bus
;		sta $dd02

		pla
!ifdef RAW {
		jsr bitfire_loadraw_
} else {
;		jsr bitfire_send_byte_
		jsr bitfire_loadcomp_
;		jsr link_load_next_comp
++
}
		lda #$fd
		sta $dc00
		lda $dc01
		cmp #$7f
		bne +
;!ifdef CHECKSUM {
		jsr checksum
;}
+
		inc numb+1
		lda numb+1
		cmp #num_files
		bne next

		jsr display
		jsr print_count
		jsr hex_runs
		jsr reset

!ifdef REQDISC {
;.side		lda #$f0
;		jsr req_disc
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
		sta $d800,x
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
		sta $d800+00*40,x

		jmp clear

no
		lda #$02
		sta $d800+00*40,x
		jmp reset_drv
end_w
		lda #$07
		sta $d800+00*40,x
reset_drv
		;jsr bitfire_reset_drive_
		jmp *
req_disc
!src "../request_disc.asm"

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

hex
		!text "0123456789abcdef"
sizes
!ifdef RAW {
!word $c179-$b569
!word $bf80-$6561
!word $bd00-$a93e
!word $4900-$3e1a
!word $6600-$3db1
!word $4396-$3162
!word $62d5-$5dec
!word $2d00-$2aff
!word $4500-$35f8
!word $6358-$548e
!word $6200-$3774
!word $7300-$68bb
!word $67a1-$56c6
!word $bef7-$bb61
!word $8000-$78da
!word $af00-$a06c
!word $666b-$3b53
!word $a800-$94af
!word $c179-$b569
!word $bf80-$6561
!word $bd00-$a93e
!word $4900-$3e1a
!word $6600-$3db1
!word $4396-$3162
!word $62d5-$5dec
!word $2d00-$2aff
!word $4500-$35f8
!word $6358-$548e
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
!ifdef RAW {
!byte $c6
!byte $b8
!byte $4c
!byte $54
!byte $d1
!byte $86
!byte $db
!byte $03
!byte $d8
!byte $d4
!byte $58
!byte $9b
!byte $9b
!byte $ed
!byte $35
!byte $f8
!byte $c5
!byte $dd
!byte $c6
!byte $b8
!byte $4c
!byte $54
!byte $d1
!byte $86
!byte $db
!byte $03
!byte $d8
!byte $d4
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
!ifdef RAW {
!word $b569
!word $6561
!word $a93e
!word $3e1a
!word $3db1
!word $3162
!word $5dec
!word $2aff
!word $35f8
!word $548e
!word $3774
!word $68bb
!word $56c6
!word $bb61
!word $78da
!word $a06c
!word $3b53
!word $94af
!word $b569
!word $6561
!word $a93e
!word $3e1a
!word $3db1
!word $3162
!word $5dec
!word $2aff
!word $35f8
!word $548e
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

