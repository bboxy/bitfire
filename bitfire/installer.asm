!src "config.inc"
!convtab pet
!cpu 6510

!zone installer {
.dc_src		= $fc
.dc_dst		= $fe

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
		sta .iec_units

		lda #8
		sta $ba
		;jmp do_install

		ldx #4
-
		jsr .open_w_15
		bmi +
		inc .iec_units
		lda $ba
		sta .my_drive
		jsr .unlisten
+
		inc $ba
		dex
		bne -
.iec_units = * + 1
		lda #$00
		cmp #1
		beq .do_install
		ldx #$00
-
		lda .pebcak,x
		beq .init_inst
		sta $07c0,x
		inx
		bne -
.do_install
.my_drive = * + 1
		lda #$08
		sta $ba

		;install bootloader with fast m-w and onetime loader-init
		jsr .install_bootstrap
		sei

		lda #$c3
		;and $dd00
		sta $dd00
		lda #$3f
		sta $dd02

.cnt = * + 1
		lda #(>.drivecode_size) + 1

                bit $dd00		;wait until drive bootloader is active
                bmi *-3

		lda #$37
		sta $dd02

		ldy #$00
-
.dc_data = * + 1
		lda .drivecode_start,y
                sec
                ror
		sta .dc_src
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

		iny
		bne -

		inc .dc_data+1
		dec .cnt
		bne -

		lda #$3f		;all lines low again
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

.l1		lda $d012
.l2		cmp $d012
		beq .l2
		bmi .l1
		cmp #$20
		bcs .nontsc

		lda #$b9		;lda $xxxx,y
		sta bitfire_ntsc_fix1
		lda #$19		;ora $xxxx,y
		sta bitfire_ntsc_fix2
		sta bitfire_ntsc_fix3
		lda #$39		;and $xxxx,y
		sta bitfire_ntsc_fix4

		lda #-$37
		sta bitfire_ntsc_fix1 + 1
		sta bitfire_ntsc_fix2 + 1
		sta bitfire_ntsc_fix3 + 1
		sta bitfire_ntsc_fix4 + 1

		lda #$dc
		sta bitfire_ntsc_fix1 + 2
		sta bitfire_ntsc_fix2 + 2
		sta bitfire_ntsc_fix3 + 2
		sta bitfire_ntsc_fix4 + 2
.nontsc
!if BITFIRE_AUTODETECT = 1 {
		!src "detect.asm"
}
		;wait until floppy is ready
		;wait for drive to initialize XXX TODO maybe wait for special signal on $dd00?
		sei
		ldx #$10
wait
-
		bit $d011
		bpl *-3
		bit $d011
		bmi *-3
		dex
		bpl -

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
		lda #<.bootstrap_start
		sta .dc_src
		lda #>.bootstrap_start
		sta .dc_src+1

		lda #<.bootstrap
		sta .dc_dst
		lda #>.bootstrap
		sta .dc_dst+1

		ldx #(.bootstrap_size / $20) + 1
.bs_loop
		jsr .open_w_15

		lda #'m'
		jsr .iecout
		lda #'-'
		jsr .iecout
		lda #'w'
		jsr .iecout
		lda .dc_dst		;target-address
		jsr .iecout
		lda .dc_dst+1
		jsr .iecout
		lda #$20	;payload
		jsr .iecout

		ldy #$00
-
		lda (.dc_src),y
		jsr .iecout
		iny
		cpy #$20
		bne -

		tya
		clc
		adc .dc_dst
		sta .dc_dst
		bcc *+4
		inc .dc_dst+1

		tya
		clc
		adc .dc_src
		sta .dc_src
		bcc *+4
		inc .dc_src+1

		jsr .unlisten

		dex
		bne .bs_loop

		;now execute installer
		jsr .open_w_15

		;ldx #$00
-
		lda .me_code,x
		jsr .iecout
		inx
		cpx #$05
		bne -
		jmp .unlisten

.pebcak
!convtab scr {
		!text "more than 1 drive on bus, turn off plz!"
		!byte 0
}

.me_code
!byte $4d,$2d,$45,<.bootstrap_run,>.bootstrap_run

!src "drivecode.asm"

!if (BITFIRE_RESIDENT_AUTOINST != 0) {
.res_start
!bin "resident",,2
}
}
