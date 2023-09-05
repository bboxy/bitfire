crt_loop
			lda $de00
			ldx #$04
-
			lda $df04,x
			cmp signature,x
			bne +
			dex
			bpl -
+
			inx
			bne crt_loop
			sei
			lda #$7f
			sta $dd0d
			sta $dc0d
			lda $dd0d
			lda $dc0d
			lda #$37
			sta $01
			jmp ($fffc)
signature
			!byte $c3 ;c
			!byte $c2 ;b
			!byte $cd ;m
			!byte $38 ;8
			!byte $30 ;0
