		.macro link_decomp_under_io () {
			dec $01                         //bank out IO
			jsr link_decomp                 //depack
			inc $01
		}

		.macro link_load_next_raw_decomp () {
			jsr link_load_next_raw
			dec $01                         //bank out IO
			jsr link_decomp                 //depack
			inc $01
		}

		//load next file as a raw file [do not decomp on the fly] and jump to .addr
		.macro link_load_next_raw_jmp (arg) {
			lda #>[arg-1]
			pha
			lda #<[arg-1]
			pha
			jmp link_load_next_raw
		}

		//load next file and decomp on the fly, then jump to arg
		.macro link_load_next_comp_jmp (arg) {
			lda #>[arg-1]
			pha
			lda #<[arg-1]
			pha
			jmp link_load_next_comp
		}

		//decomp previously loaded file and jump to arg
		.macro link_decomp_jmp (arg) {
			lda #>[arg-1]
			pha
			lda #<[arg-1]
			pha
			jmp link_decomp
		}

		.macro link_load_next_double () {
			jsr link_load_next_comp
			jsr link_load_next_raw
			dec $01
			jsr link_decomp
			inc $01
		}

		//link irq hook back to base irq
		.macro link_player_irq () {
			sei
			lda #<link_player
			sta $fffe
			lda #>link_player
			sta $ffff
			lda #$ff
			sta $d012
			cli
		}

		.macro reset_drive () {
			lda #BITFIRE_RESET
			jsr bitfire_send_byte_
		}

		//request next disk side and reset filenum_counter
		.macro request_disk (arg) {
			lda #arg + $ef
			//simply do a bogus loadraw, this will call ld_pblock until eof is raised, nothing is loaded as block_ready never happens :-]
			jsr bitfire_loadraw_
			//jsr bitfire_send_byte_
			//pha
			//pla
			//pha
			//pla
			//+wait_floppy_idle
		}

		//wait for given frame counter value
		.macro link_wait_syncpoint (arg) {
l:
			lda link_syncpoint
			cmp #arg
			bcc l
		}

		.macro setup_sync (arg) {
			lda #$00
			sta link_frame_count + 0	//avoid that the counter wraps worstcase, as we can't write atomic
			lda #>[$8000-arg]
			sta link_frame_count + 1
			lda #<[$8000-arg]
			sta link_frame_count + 0	//now setup counter
		}

		.macro sync () {
l:
			lda link_frame_count + 1	//Counter expired when $8000 is reached
			bpl l
		}

		.macro bus_lock () {
			lda #$37			//set ATN
			sta $dd02
		}

		.macro bus_unlock () {
			lda $dd00
			and #$03
			sta $dd00
			lda #$3f
			sta $dd02
		}

		.macro set_depack_pointers (arg) {
			lda #<arg
			sta bitfire_load_addr_lo
			lda #>arg
			sta bitfire_load_addr_hi
		}

		.macro link_music_update () {
			inc link_frame_count + 0
			bne l
			inc link_frame_count + 1
l:
			jmp [link_music_addr]
		}

		.macro to_nmi () {
			lda #$ad
			sta link_player + 7
			lda #$0d
			sta link_player + 8
			lda #$dd
			sta link_player + 9
		}

		.macro to_irq () {
			lda #$ce
			sta link_player + 7
			lda #$19
			sta link_player + 8
			lda #$d0
			sta link_player + 9
		}

		.macro switch_to_nmi () {
			lda #$fe
l:
			cmp $d012
			bne l
			sei
			:to_nmi()
			:start_music_nmi()
			jsr link_music_play
		}

		.macro switch_to_irq () {
			lda #$fe
l:
			cmp $d012
			bne l
			:stop_music_nmi()
			:to_irq()
			ldx #<link_player
			lda #>link_player

			stx $fffe
			sta $ffff
			lda #$ff
			sta $d012
			jsr link_music_play
		}

		.macro start_music_nmi () {
			//lda #$20
			//sta link_music_play_jmp
			ldx #<link_player
			lda #>link_player

			stx $fffa
			sta $fffb
			lda #$00
			sta $dd0e
			lda $dd0d
			lda #$c7
			sta $dd04
			lda #$4c
			sta $dd05
			lda #$81
			sta $dd0d

			lda #$ff
l:
			cmp $d012
			bne l

			lda #$11
			sta $dd0e
		}

		.macro stop_music_nmi () {
			lda #$7f
			sta $dd0d
			lda $dd0d
		}

		.macro restart_music_nmi () {
			lda $dd0d
			lda #$81
			sta $dd0d
		}

		.macro set_music_addr (arg) {
			lda #<arg
			sta link_music_addr + 0
			lda #>arg
			sta link_music_addr + 1
		}

		.macro wait_space () {
			lda #$7f
			sta $dc00
			lda #$10
l:

			bit $dc01
			bne l
		}

		.macro wait_shift_lock () {
			lda #$fd
			sta $dc00
l:
			lda $dc01
			bpl l
		}

		.macro crt_request_disk (arg) {
			:setup_sync(arg)
l:
			lda #$7f			//space pressed?
			sta $dc00
			lda $dc01
			and #$10
			beq e				//yes, exit
			lda link_frame_count + 1	//check counter
			bpl !+
			sta f + 1
!:
			lda #$fd			//shift lock pressed?
			sta $dc00
			lda $dc01
f:			and #$00
			bpl l				//shift lock pressed or not expired
e:
		}

		.macro skip_file () {
			lda #BITFIRE_SKIP_FILE
			jsr bitfire_loadraw_
		}

//		!macro gaps_setup {
//			ldx #$09
//l:
//			lda CONFIG_RESIDENT_ADDR + $000,x
//			pha
//			lda CONFIG_RESIDENT_ADDR + $100,x
//			pha
//			dex
//			bpl l
//		}
//
//		!macro gaps_restore {
//			ldx #$00
//l:
//			pla
//			sta CONFIG_RESIDENT_ADDR + $000,x
//			pla
//			sta CONFIG_RESIDENT_ADDR + $100,x
//			inx
//			cpx #$0a
//			bne l
//		}
