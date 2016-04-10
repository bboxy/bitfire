!ifndef BITFIRE_IS_INCLUDED {
!src "loader_acme.inc"
}
bitfire_request_disc_
		;send $fx as command
		jsr bitfire_send_byte_
		;give floppy time to enter busy mode, and set lines to input
		lda #$3f
		sta $dd02

		;wait until floppy is idle again
                bit $dd00
                bpl *-3
		rts
