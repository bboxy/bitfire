!convtab scr
!cpu 6510

;RAW = 1
;CHECKSUM = 1

num_files	= $12

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

		lda #$03
		sta $dd02

		ldy #$00
-
		inc $dd00
		dey
		bne -

		lda #$c3
		sta $dd00

		lda #$27
		sta $dd02

		pla
!ifdef RAW {
		jsr bitfire_loadraw_
} else {
		jsr bitfire_loadcomp_
		;jsr bitfire_decomp_
;		bne +
;		jsr bitfire_loadcomp_
;		jmp ++
;+
;		jsr link_load_next_comp
++
}
!ifdef CHECKSUM {
		jsr checksum
}
		inc numb+1
		lda numb+1
		cmp #num_files
		bne next

		jsr display
		jsr print_count
		jsr hex_runs
		jsr reset

		;lda #$f1
		;jsr req_disc
		;jmp *
		;jmp reset_drv
		jmp next

;		lda #$f1
;		jsr bitfire_send_byte_
;		nop
;		lda $dd00
;		sta $0400
;		bit $dd00
;		bpl *-3
;		inc $d020
;		jsr reset
;		jmp *
irq
		pha
		dec $d020
		dec $d019
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
		jsr bitfire_reset_drive_
		jmp *
!src "../reset_drive.asm"
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
		rts

hex
		!text "0123456789abcdef"
sizes
!ifdef RAW {
!word $02f6
!word $07dd
!word $157f
!word $4023
!word $1ed9
!word $6469
!word $4894
!word $561e
!word $27da
!word $5467
!word $74b4
} else {
!word $1999
!word $1800
!word $4000
!word $7234
!word $253b
!word $9679
!word $8163
!word $66f1
!word $2d7e
!word $7eb4
!word $8d38
}

chksums
!ifdef RAW {
!byte $9e
!byte $26
!byte $69
!byte $fe
!byte $6c
!byte $af
!byte $ba
!byte $0e
!byte $a7
!byte $6a
!byte $04
} else {
!byte $aa
!byte $d9
!byte $22
!byte $49
!byte $53
!byte $51
!byte $de
!byte $f0
!byte $e2
!byte $79
!byte $b3
}

loads
!ifdef RAW {
!word $b2a3
!word $3823
!word $3a81
!word $4611
!word $1a62
!word $4610
!word $4ccf
!word $24d3
!word $19a4
!word $3e4d
!word $2c84
} else {
!word $9c00
!word $2800
!word $1000
!word $1400
!word $1400
!word $1400
!word $1400
!word $1400
!word $1400
!word $1400
!word $1400
}
