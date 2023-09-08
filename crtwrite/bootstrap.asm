!cpu 6510
casbuf			= $33c			; Kassettenbuffer, für Code-Sequenz
tastbuf			= $277			; Tastaturpuffer
tbuffered		= $c6			; Zeichen im Tastaturpuffer
rom			= $8000

			* = rom
			!word launcher
			!word launcher
			!byte $c3 ;c
			!byte $c2 ;b
			!byte $cd ;m
			!byte $38 ;8
			!byte $30 ;0

launcher
			stx $D016				; Turn on VIC for PAL / NTSC check
			jsr $FDA3				; IOINIT - Init CIA chips
			jsr $FD50				; RANTAM - Clear/test system RAM
			jsr $FD15				; RESTOR - Init KERNAL RAM vectors
			jsr $FF5B				; CINT   - Init VIC and screen editor
			cli					; Re-enable IRQ interrupts

			jsr $E453				; Init BASIC RAM vectors
			jsr $E3BF				; Main BASIC RAM Init routine
			jsr $E422				; Power-up message / NEW command
			ldx #$FB
			txs					; Reduce stack pointer for BASIC

install
			ldx #(routine_end-routine_start-1)
copyloop
			lda routine_start,x
			sta casbuf,x
			dex
			bpl copyloop
run
			ldx #4
			stx tbuffered
			dex
.nextchar
			lda .tcmd,x
			sta tastbuf,x        ; write keyboard buffer
			dex
			bpl .nextchar
.leave
			jmp casbuf
.tcmd
			!text "RUN"
			!byte 13 ; Command to write to keyboard buffer
routine_start            ; current start address in rom of routine to be copied and run
!pseudopc casbuf {

		        lda $de00 ;The first access will disable $8000 and MAY count block+1

			;Call the Kernal to check for "CBM80" string
			;if still visible, we need to do another read $DE00.
			;This is due to RESET and COUNT of the 4040 IC are switched at the same time
		 	lda $df04
			cmp #$C3
			bne movecode
		 	lda $df05
			cmp #$C2
			bne movecode
		 	lda $df06
			cmp #$CD
			bne movecode
		        lda $de00 ;This access will then move definitely to block 1
movecode
                        jsr read_byte
                        sta .dst + 1
                        jsr read_byte
                        sta .dst + 2

                        jsr read_byte
                        sta .endpos_lo + 1
                        jsr read_byte
                        sta .endpos_hi + 1

                        lxa #0
                        tay
                        ;decrement counter to zero? would that work? cheaper to check
-
                        jsr read_byte
.dst                    sta $0000,y
                        iny
                        beq .chk_inc
.chk_done
.endpos_lo              cpy #$00
                        bne -
.chk_hi
.endpos_hi              cpx #$00
                        bne -
			lda $de00
        		jmp $E386 ;basic start

.chk_inc
                        inc .dst + 2
                        inx
                        bne .chk_done
read_byte
.off                    lda $df00
                        inc .off + 1
                        bne +
                        bit $de00
+
                        rts
casend
}
routine_end            ; current end address in rom of routine to be copied and run

			!align 255,0
files
