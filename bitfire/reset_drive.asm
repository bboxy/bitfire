!ifndef BITFIRE_IS_INCLUDED {
!src "loader_acme.inc"
}
bitfire_reset_drive_
!zone {
                ldy #.upload_size - 1
		;check if drive is idle
                bit $dd00
                bpl *-3
		;upload data
-
                ;wait for data ready
                lda .upload_start,y
		;only send 8 bits, no $dd02 sanitize
		;ldx #$07
                jsr bitfire_send_byte_
		lda #$3f
		sta $dd02
		;waste cycles to be sure drive keeps up
		jsr .waste
		dey
                bpl -
.waste
		rts
.upload_start
!pseudopc $0108 {
.code_start
		;code to be uploaded goes here
		jmp $eaa0
.code_end
}
.code_size = .code_end - .code_start;

                !byte >(.code_size - 1), <(.code_size - 1)
		!byte >(.code_end - 1),  <(.code_end - 1)
		!byte BITFIRE_UPLOAD
.upload_end
.upload_size = .upload_end - .upload_start;
}
