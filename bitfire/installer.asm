!src "config.inc"
!convtab pet
!cpu 6510

!zone installer {
.dc_src		= $fc
.dc_dst		= $fe
.mydrive	= $fb

.listen		= $ffb1
.listen_sa	= $ff93
.iecout		= $ffa8
.unlisten	= $ffae

		* = BITFIRE_INSTALLER_ADDR
		!src "loader_acme.inc"
.init_inst
		lda #$37
		sta $01
		lda #$00
		sta $d015
		lda $ba
		sta .mydrive

		ldx #8
-
		stx $ba
		jsr .open_w_15
		bmi .not_present
		jsr .unlisten
		ldx $ba
		cpx .mydrive
		beq .not_present
		jsr .install_responder
.not_present
		ldx $ba
		inx
		cpx #$0c
		bne -
.do_install
		lda .mydrive
		sta $ba

		;install bootloader with fast m-w and onetime loader-init
		jsr .install_bootstrap
		sei

		lda #$c3		;XXX TODO, needed? $03 would suffice?
		sta $dd00

		lda #$3f
		sta $dd02

.cnt = * + 1
		lda #(>.drivecode_size) + 1

                bit $dd00		;wait until drive bootloader is active
                bmi *-3

		lda #$37
		sta $dd02

-
.dc_data = * + 1
		lda .drivecode_start
                sec
                ror
		sta .dc_src
		;transfer like this?
                lda #$2f
.s_loop
                and #$2f                        ;clear bit 4 and 0..2 and waste some cycles here
                adc #$00                        ;on carry set, clear bit 4, else keep
		eor #$30
                ora #$0f

                sta $dd02
		pha				;make NTSC machines happy
		pla
                lsr .dc_src
                bne .s_loop

		inc .dc_data
		bne +
		inc .dc_data+1
+
		lda .dc_data
		cmp #<.drivecode_end
		bne -
		lda .dc_data+1
		cmp #>.drivecode_end
		bne -

		lda #$37			;raise atn to signal end of transfer
		sta $dd02

!if (BITFIRE_RESIDENT_AUTOINST != 0) {
!if (bitfire_resident_size) < 256 {
		;better force to 8 bit, label might be defined as 16 bit
		ldx #<(bitfire_resident_size)
-
		lda .res_start,x
		sta BITFIRE_RESIDENT_ADDR,x
		dex
	!if bitfire_resident_size >= $80 {
		cpx #$ff
		bne -
	} else {
		bpl -
	}
} else {
		;copy resident part
		ldx #$00
-
		lda .res_start,x
		sta BITFIRE_RESIDENT_ADDR,x
		lda .res_start + ((bitfire_resident_size) - $100),x
		sta BITFIRE_RESIDENT_ADDR + ((bitfire_resident_size) - $100),x
		dex
		bne -
}
}

;.l1		lda $d012
;.l2		cmp $d012
;		beq .l2
;		bmi .l1
;		cmp #$20
;		bcs .nontsc

;		lda #$b9		;lda $xxxx,y
;		sta bitfire_ntsc_fix1
;		lda #$19		;ora $xxxx,y
;		sta bitfire_ntsc_fix2
;		sta bitfire_ntsc_fix3
;		lda #$39		;and $xxxx,y
;		sta bitfire_ntsc_fix4
;
;		lda #-$37
;		sta bitfire_ntsc_fix1 + 1
;		sta bitfire_ntsc_fix2 + 1
;		sta bitfire_ntsc_fix3 + 1
;		sta bitfire_ntsc_fix4 + 1
;
;		lda #$dc
;		sta bitfire_ntsc_fix1 + 2
;		sta bitfire_ntsc_fix2 + 2
;		sta bitfire_ntsc_fix3 + 2
;		sta bitfire_ntsc_fix4 + 2
;.nontsc
		lda #$3f			;drop atn to signal end of transfer
		sta $dd02

!if BITFIRE_AUTODETECT = 1 {
		!src "detect.asm"
}
		lda #$7f
		sta $dd0d
		lda $dd0d

		;wait until floppy is ready
		;wait for drive to initialize XXX TODO maybe wait for special signal on $dd00?

;		sei
;		ldx #$10
;wait
;-
;		bit $d011
;		bpl *-3
;		bit $d011
;		bmi *-3
;		dex
;		bpl -
		lda #$37
		sta <BITFIRE_LAX_ADDR
-
		lda $dd00
		bpl -
		rts

.open_w_15
		lda $ba
		jsr .listen
		lda #$00
		sta $90
		lda #$6f
		jmp .listen_sa

.install_bootstrap
		jsr .open_w_15
		lda #'i'
		jsr .iecout
		jsr .unlisten
		;ldx #$10
		;jsr wait
		;install first routines via m-w

		ldx #$00
.bs_loop

		jsr .open_w_15

		lda #'m'
		jsr .iecout
		lda #'-'
		jsr .iecout
		lda #'w'
		jsr .iecout
		txa
		clc
		adc #<.bootstrap_run
		php
		jsr .iecout
		plp
		lda #>.bootstrap_run
		adc #$00
		jsr .iecout
		ldy #$20
		tya
		jsr .iecout
-
		lda .bootstrap_start,x
		jsr .iecout
		inx
		dey
		bne -

		jsr .unlisten

		cpx #.bootstrap_size
		bcc .bs_loop

		;now execute bootstrap
		jsr .open_w_15

		ldx #$00
-
		lda .me_code,x
		jsr .iecout
		inx
		cpx #$05
		bne -
		jmp .unlisten

.me_code
		!text "m-e"
		!word .bootstrap_run

!src "drivecode.asm"

!if (BITFIRE_RESIDENT_AUTOINST != 0) {
.res_start
!bin "resident",,2
}

.install_responder
		jsr .open_w_15

		ldx #0
.datalo		lda .atnlo,x
		jsr .iecout
		inx
		cpx #.atnlo_end - .atnlo
		bne .datalo
		jsr .unlisten

		jsr .open_w_15
		ldx #0
.datahi		lda .atnhi,x
		jsr .iecout
		inx
		cpx #.atnhi_end - .atnhi
		bne .datahi
		jsr .unlisten

		jsr .open_w_15

		ldx #0
.exec		lda .responder,x
		jsr .iecout
		inx
		cpx #.responder_end - .responder
		bne .exec
		jmp .unlisten

;XXX TODO keep current drive in $ba? kill all other drives beforehand, then upload code to #8
.responder_code	= $0205
.atnlo_code	= $0400
.atnhi_code	= .atnlo_code + $80

.responder
		!text "m-e"
		!word .responder_code
!pseudopc .responder_code {
		sei
		lda #$ff
		sta $1803
		ldx #$04
		stx $1801
		lsr		;lda #$7f
		sta $1802
		ldy #$00
		ldx #$10
		sty $1800
		jmp ($1800)
}
.responder_end

.atnlo		!text "m-w"
		!word .atnlo_code
		!byte .atnlo_end - .atnlo_start
.atnlo_start
!pseudopc .atnlo_code {
		jmp ($1800)
		* = .atnlo_code + $10
		sty $1800
		jmp ($1800)
}
.atnlo_end

.atnhi		!text "m-w"
		!word .atnhi_code
		!byte .atnhi_end - .atnhi_start
.atnhi_start
!pseudopc .atnhi_code {
		stx $1800
		jmp ($1800)
		* = .atnhi_code + $10
		jmp ($1800)
}
.atnhi_end

}
