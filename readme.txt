What is it
----------

Bitfire is a fixed interleave loadersystem with depacker, a basic framework and an image writing tool. Aim was to make the loader as fast as possible while being as tiny as possible. So at some points size and speed had to be traded against each other. An own, however d64-compatible (bam copy is sufficient), file format is introduced to make the code less complex and loading faster. Also, functions that are not used regularly (like turn disk detection) are available as statically linkable functions and thus make the resident part on c64 side even smaller. Being that tiny ($84 to $200 bytes, depending on configuration) and still fast, makes it perfect for being used in demos. The imaging-tool creates diskimages with all demofiles and a dirart on it. Also it is accompanied by a lz-packer based on doynamite, however smaller in code and a bit faster than that, while yielding nearly the same results. The packer supports a lot of functionality and thus makes other tools obsolete, as it can split files, reference data in previous files, write in several formats, create sfx and much more. The framework supports a base irq and safe loading hooks, as well as a loadnext functionality (but this feature is also available without framework, see examples), however loading by filenumber is still supported if your demo is in need of random file access (like the comaland endpart)

dali
----

dali is a wrapper of salvador a zx0-compatible near-optimal compressor, that can also handle repeated offsets besides normal matches and literals. Salvador is written by Emmanuel Marty and is pretty fast compaed to the original zx0-compressor brought up by Einar Saukas. The tradeoff are slight differences in the pack-ration of a few bytes occasionally.

The packer/wrapper supports output of both, sfx as well as levelpacked files suitable for Bitfire

the following options are available:

-o [filename]
Set an output filename, else it will be derived from the input filename by adding an .lz extension

--sfx [$addr]
Create a c64 compatible sfx-executable that starts your code at [$addr] after depacking. Further options are available to configure the sfx-depacker.

--01 [$num]                 Set $01 to [num] after sfx
Address $01 in zeropage will be set to the given value after depacking, so that you end up using the right memory configuration, mostly needed when your executeable starts under basic, kernal or i/o.

--cli [$num]                Do a CLI after sfx, default is SEI
Enable interrupts again after depacking, default is to leave the depacker with a SEI instruction, so interrupts do not go mayhem after disabling any ROM with $01.

--small
Use a very small depacker that fits into zeropage, but --01 and --cli are ignored and it trashes zeropage (!) whereas the normal sfx restores the zeropage from $01 - $df after depacking, $e0-$ff are trashed.

--no-inplace
Disables the inplace-decompression, and thus always an end-marker is added to the file.

--binfile
Input file is a raw binary without load-address, the load-address needed is taken from the --relocate-origin switch

--from [$num]
--to [$num]
With those two flags the file can be cut into desired slices that are then fed to the packer. This way the file can easily split up into a portion that goes up until $d000 and a portion that goes from $d000-$ffff (remember, no loading under i/o possible)

--prefix-from [$num]
Use preceeding data of file as dictionary. This requires, that the data referenced here is also in memory at the same location when depacking. You can specify from which address on the dictionary should be used.

--prefix-file [filename]
Same as above, but by handing over a file that contains the preceeding dictionary data.

--relocate-packed [$num]
Relocate packed data to desired address [$num]. The resulting file can't de decompressed inplace anymore after that, as it requires an end-marker then.

--relocate-origin [$num]
Set load-address of source file to [$num] prior to compression. If used on bin-files, load-address and depack-target is prepended on output.

A standalone depacker-example can be found in packer/dali/dzx0.asm

Disclayout
----------

When d64write writes files in bitfire format the full 256 byte of each sector are used for data and for that no track/sector link is added. This can be done, as the sectorchain is calculated statically from the beginning track/sector on. The needed information for each file is stored in an own directory format. The directory lies on max. 2 blocks in track 18 (means 126 files max). It fills up sector 18 as well as 17 if needed. Each fileentry consists of 4 bytes:
byte 0: load-address lowbyte
byte 1: load-address highbyte
byte 2: (filelength-1) lowbyte
byte 3: (filelength-1) highbyte

Thus 63 files fit into one sector (the remeining bytes contain the diskside info and the start-position of the first file on the current directory sector). This means, 63 files can be loaded sequentially without having any seeking action due to fetching new direntries. The last byte of the dir sectors represent the diskside the files are on. This value will be checked if another diskside is requested. The values range from $f0 to $fe, so demos with up to 15 different disksides can be created (hi Offence! :-P). Also this is less error prone than the usual checks on the disk id.

d64write
--------

d64write generates a suitable .d64 for you which can be read by bitfire and incorporates all the workflow for final image creation into a single tool. So it writes hidden files for you, while keeping maximum reading performance and providing a dirart linked to the bootloader.

After the bitfire formatted files are written, standard files are added to the diskimage. So there's still the possibility to add files that can be loaded normally, mixing both types on a disk is no problem. So a bigger bootloader or adding a note is no problem. It is even possible to add further files to a disk with original gear, as the BAM is maintained and used blocks are thus protected from allocation/overwriting. However bitfire-files must be written first, as it assumes starting at track 1, sector 0, to avoid data loss this requres the -c flag to be set, so that d64write starts wit ha fresh formated .d64.

Next up a small file, the bootloader, can be placed into the remaining sectors on track 18 to save blocks. It should be small enough to fit there, or an error occurs. Usually that program should not do more than install the loader and load/run the first file (bootstrap) on disk that then starts the demo(side). For more details see the installer and bootstrap folder in the examples-dir.

As a final step a dirart can be added to the dir, if there's still enough sectors free to accomodate it. An error will occur if not. d64write accepts a saved screen for that purpose, so it should be easy to create dirarts with e.g. a petscii editor (however take care that not all petscii symbols are accepted for dirart). From that screen every first 16 chars per row are taken for creating a dirart. The number of rows to be read in can be specified. The first row is used to specify header and id separated by an arbitrary char (see example dirart coming along with this release).

If you want to place multiple files, (in standard format, for bitfire format it is mandatory to write in one go), you can add the -b or -s option multiple times to one commandline. So the right order is kept on a single call. Files in bitfire format are written sequentially so that no unecessary seektimes are created, that can be broken when standard files are placed in between. That said, it is wisely to place files on disk in the same order to be loaded, if being loaded once (random access is possible of course, but gives penalties due to excessive seeking).

Example:
d64write -c cooldemo.d64 -h oxyron -i rules --side 1 --boot cooldemo.lz -b bootstrap1.lz -b part1.lz -b part2.lz -b part3.lz -s note

other files
-----------

link_macros*.inc contain many functions that can be linked in statically by including them when needed and calling a macro. All important labels are exported when creating the loader bianries via make. So no need to take care about them. The acme-version is maintained best, if you lack a function, feel free to extnd the includes with more macros for your favourite cross-assembler.

40 tracks support
-----------------

With the --40 switch d64write will create a diskimage that supports 40 tracks.

Building
--------

create all necessary files in the bitfire directory (including d64write) with the following command:
make

After building you will get a couple of files:
dali			The salvador packer ported to c64, based on salvador by Emmanuel Marty, details see above
d64write		The d64-tool, Described already above
installer		The installer that must be called beforehand. File is in .prg format. It includes already the resident part and copies it to the final destination in mem if called.
loader-acme.inc		All necessary labels to call the functions of the loader, acme-style (also works for dasm and dreamass).
loader-c6510.inc	See above, but c6510-style labels.
loader-kickass.inc	See above, but KickAssembler-style labels.

Now you can use the loader as a standalone version, but there's also other comfortable stuff that can be used.

The Framework
-------------

For easy demomaking there's already a basic framework included into bitfire and a bootloader as well as a prototype for a bootstrap per side. To make use of that features, build (make -C framework/installer), pack and write the installer on disk with the --boot option.
The installer installs the loader and its resident part, autodetects hardware (cia/sid/ntsc), and loads the bootstrap for the currently inserted side (see framework/bootstrap/).
Modify bootstrap/boot.asm to your needs. It usually loads music of the corresponding side and in case changes the music call if music has a different location, loading the first part and entering it with the value given by link_exit. It is volatile and executed once only and loaded into the stack area to avoid clashes with loaded parts.
The bootstrap is compiled depending on the SIDE switch (make -C framework/bootstrap SIDE=1 link_exit=8192). Thus each side can get a different entry point and music, using the same bootloader on every side (framework/installer).

link_macro_*.inc can be included for easier linking handling. They contain a few helpful macros for hooking up the base irq or leaving/loading parts in a safe manner, so that new parts can be loaded without overwriting currently executed code.

music.inc and syncpoint.inc can be used to add syncpoints and an address from a sid player which indicates those via extra commands (supported for e.g. by goattracker). But there's also the good old framecounter available that counts up continously and that one can use to sync to.

Why separating things into a installer and bootstrap? The installer is there so that every side can be started directly with that bootloader, the bootstrap is the common entry point that the side starts with, no matter if loaded via the installer or turn disk part from a previous side.

The framework doesn't need a loader script that occupies additional ram for nothing. Therefore parts are ought to be daisychained with the link_exit label and finalized with for e.g. a +link_load_next_comp_jmp macro to load and enter the next part/transition in case. Convenience is a bitch, for a bit of it we are too often willingly sacrifice memory page by page. Stop it! Also, one always needs to return to a loader script and may not spoil the stack/stackpointer. Often one wants to load already while still an effect is in action, all this then has to happen with triggers and running in an irq for being able to make the loaderscript continue. This way it is even easier to let each part do the loading where necessary. The static way of linking lets each part grow by a very few bytes (9 bytes for a final call), but saves many bytes that are available for every part but else wasted for a danymic linking approach.</rant>

For a better understanding of the intended building process, an example Makefile is included to give an idea how things could be implemented. Of course you are free to use your own build-environment or scripts to implement a build process.

NTSC
----
NTSC support is working in vice, not tested on real hardware

Add/remove functionality
------------------------

In config.inc you may turn off certain functionality and by that save memory (is it necessary with that size however?). The plain loadraw-function will consume $7e bytes only. With all functions enabled the resident size is still smaller than $200 bytes, small, isn't it?

Else, choose from the following functions:
CONFIG_INCLUDE_DECOMP          = 1             ;Include decompressor including on the fly decompression capabilities
CONFIG_INCLUDE_FRAMEWORK       = 1             ;Include helpful calls (link_*) for loading files from a safe spot in mem, load_next functionality with no overhead, or to add an io safe base-irq

Also you might want to try the following:
CONFIG_CONFIG_MOTOR_ALWAYS_ON  = 1		;This lets the motor spin all way round like a record and thus save valueable spinup time. If it goes on your nerves, set it to 0 and the drive will stop after loading.

Those values speak for themself:
CONFIG_ZP_ADDR			= $02
CONFIG_INSTALLER_ADDR		= $1000
CONFIG_RESIDENT_ADDR		= $0200

Yes, i prefer to place the whole thing at $0200-$3ff, there's no point in wasting precious ram at $0800 or $0c00, charsets or screens can be placed there or a bigger sid that reaches from $0800-$1fff.

Framework calls and most macros start with link_*, original bitfire functions with bitfire_*

Functions via loader_acme.inc
-----------------------------

When includng the loader/loader_acme.inc, the following calls can be made:

link_load_raw			;loads file# given by A as raw file, $ef loads next file, $fx loads/requests disk side and returns with an empty file when disk is changed
link_load_comp			;loads file# given by A and decompress it on the fly while loading
link_load_next_raw		;loads next file as raw file, no file# needed
link_load_next_comp		;loads next file and decompress it on the fly while loading, no file# needed
link_load_next_raw_decomp	;loads next file and decompress it after loading, decompression can also happen under i/o
link_load_next_double		;loads one file wth on the fly decompression and a second one raw and decompresses it afterwards under i/o. In fact we can load a huge file under i/o this way, by simply splitting it up
link_decomp			;decompress last file being loaded (or file the pointers point to, see macros)
link_decomp_under_io		;decompress last file being loaded under i/o
link_frame_count		;the frame-counter that is bumped each frame
link_player			;a base irq within the resident part that can be used while loading, so no previous handler is overwritten, and music continues playing.
link_music_addr			;address of the music call that can be changed if a new song is loaded and located at a different location.
link_music_play			;the music play routine that should be called once per frame, to keep music playing and frame-counter counting.
link_music_play_side1		;variables that can be set for that purpose by music.inc
link_music_play_side2		;see above
link_music_fade_side1		;see above
link_music_fade_side2		;see above
link_music_init_side1		;see above
link_music_init_side2		;see above
link_chip_types			;the installer checks for CIA and SID types, results of those checks can be read here
link_sid_type			;alias for SID type
link_cia1_type			;alias for CIA1 Type
link_cia2_type			;alias for CIA2 type

Functions via macros
--------------------

The link_* functions represent the same functins as above, but they include a jmp-target that is jumped to after loading. Why? The advantage of this is, that any code in the memory can be overwritten, except the resident part of the loader dunring the loading-operation, and after laoding the code can be entered by the given address:

link_load_next_raw_jmp .addr
link_load_next_comp_jmp .addr
link_decomp_jmp .addr
link_load_next_double_jmp .addr
link_load_next_raw_decomp_jmp .addr
link_decomp_under_io_jmp .addr

Example:
+link_load_next_comp_jmp $2000	;will load next file and decompress it on the fly and afterwards jump to $2000 to execute the loaded part

link_player_irq			;links the raster-irq to the base-irq
reset_drive			;resets the drive, so the laoder can be reuploaded later again and the drive is free for own stuff like drive-code
request_disk .num		;command the floppy to check for given side#, will return as soon as the new diskside is detected
setup_sync .frames		;setup frame counter to wait for $0000-$7fff frames
sync				;wait for sync to happen or expire
bus_lock			;lock the bus, after that arbitrary writes to $dd00 are possible, for e.g. with a complex FPP, no loading or interaction with the drive is possible in that time
bus_unlock			;unlock the bus again and and reenable the drive communication
set_depack_pointers .addr	;change the decompression target-address of a loaded file, so it can be loaded and decompressed to a different location than given during compression
start_music_nmi			;start an NMI that will call the music-player once per frame via timer on rasterline $ff
stop_music_nmi			;stop the NMI (for e.g. to hand over to a raster-irq)
restart_music_nmi		;restart the NMI again, cheaper way when the timers are still set up

Examples
--------

lda #$00
jsr link_load_raw

Will load first file from disk.

lda #$02
jsr link_load_raw
jsr link_load_next_raw

Loads file number 3 and then file number 4. If all files are loaded sequentially from disk, it is enough to use those link_load_next* calls. File number will be reset to 0 upon disk change and init. If a file is loaded by file number beforehand, the next file will be based on that file number.

lda #$01
jsr link_load_comp

Will load and on the fly depack second file from disk.

lda #$02
jsr link_load_raw
jsr link_decomp

Will load third file from disk and depack it afterwards.

!src "link_macros_acme.inc"
+request_disc 1

Will wait until side 2 (counted from side 0 on) is inserted into floppy and directory is sucessfully read. If you want to wait for disc change in a different manner, just have a look into macro, there are just a few lines of code.

lda #BITFIRE_RESET
jsr bitfire_send_byte_
Will reset the drive after the demo.

lda #BITFIRE_LOAD_NEXT
jsr link_load_raw

This will load the next file even without a framework present, as this function is implemented within the floppy code. This might be useful if you do not want to remember a filenumber but just load through disc file by file.

Decomp
------

One can also load several parts and then decomp one by one. Therefore however the load-address must be set again before depacking, for the depacker to know where to start from.

Other depackers
---------------

Feel free to add your own depacker. Just be sure to call pollblock in the yet manner when needing new data, just as done already. If you encounter problems, feel free to ask for my support.

Reset drive
-----------

When sending $ff (bitfire_send_byte_), the floppy resets itself, this is handy when the demo ends and we want to leave the hardware in a sane state.

Depacker/Packer
---------------

To create a packed files, there is a packer derived from salvador written by Emmanuel Marty. It will pack and adjust the load-address accordingly.
The commandline might look like: dali -o file.lz file.prg
If a file is going under IO it is best to split it somewhere before the IO range starts, for e.g.:
dali -o part1.lz --from 0x0801 --to 0xd000 mypart
dali -o part2.lz --from 0xd000 --to 0xffff mypart

This will result in two files that can be loaded/depacked in one go by link_load_next_double. If the resulting part2.lz happens to be that small that it has a load-address below $e000, just move it further upwards with --relocate-packed 0xe000 to avoid it being loaded under IO and crash. However be careful with the --relocate-packed flag, the file will be slightly bigger as it can't be depacked in place anymore.
Basically the load_next_double call is loading the first part and decrunching it on the fly, then loading the next part raw, switching off the IO, depacking that and turning on IO again.

Other interesting options are:
--prefix-from [addr]
--prefix-file [filename]

If you split a file and load both parts one after another, you can add this switch to the second part. It will then be packed with using the already loaded data as dictionary from the given address on, what will result in smaller files. Alternatively you can also give a file that is prepended and used as dictionary.

Example:
dali -o demopart1.prg --from 0x2000 --to 0x8000 demopart.prg
dali -o demopart2.prg --prefix-from 0x2000 --from 0x8000 --to 0xd000 demopart.prg
dali -o demopart3.prg --prefix-from 0x2000 --from 0xd000 --to 0xffff demopart.prg

This will split up demopart.prg into three chunks that can be loaded one after another as soon as the memory is free. Part2 can already refer to data from part1 and part3 can refer to data from part2 and part1. If there's code inbetween that changes while loading, just choose another memory range for the dict:
dali -o demopart1.prg --from 0x2000 --to 0x8000 demopart.prg
dali -o demopart2.prg --prefix-from 0x2000 --from 0x8000 --to 0xd000 demopart.prg	#code from $2000-$4000 is changed while loading demopart3
dali -o demopart3.prg --prefix-from 0x4000 --from 0xd000 --to 0xffff demopart.prg

NMI-gaps
--------

What is this? If you need a few free bytes at $0200 and $0300 for a ninja-style NMI handler, just add your nops at .lz_gap1 and .lz_gap2 in resident.asm to accomodate your handler.

Zero-Overlap
------------

dali creates files that have no overlap at its end and thus can be completely depacked in place within its strict barriers.

Zeropage usage
--------------

The loader needs 6 bytes in zeropage, the packer needs another 5 bytes. The values can be changed in the source accordingly, but they have been placed where they are for a good reason:
$02/$03 can then be used by music and thus all zeropage from $0e upwards can be used, even if there's code inside the zeropage it can easily grow and even reach into the stack without the need to take care of the addresses being used by bitfire.
When the loader and depacker is idle, you can use the whole zeropage and leaving any garbage there. However address $00 is used to hide a magic value #$37 there that is read by the loader with teh lax-opcode. Usually we do not make use of $00 as it is the DDR of the processor's port for bank switching. $37 is a totally sabe value there and is of no harm. Better leave it as is, unless you really know better.

Bank switching
--------------

To switch the VIC banks you can set $dd00 with $00..$03 at any time, also while loading:

lda #$03
sta $dd00

but NOT:
lda $dd00
and #$fc
ora #$03
sta $dd00

If you want to write arbitrary values to $dd00 this can be done when idle, but one needs to lock the bus with the bus_lock macro first, and before loading again unlock it with bus_unlock $bank.

What it can't
-------------

This loader is not able to load or loadcompd under IO. This has two reasons: Saving code size and saving cycles during loading. If you really need data under IO it can be either copied there or loaded raw and depacked there afterwards, so in fact there's not much need for a possibility to load under IO, except a slight improvment in convenience. To split up files use the (--cut-input-addr) to cut them into two parts (for e.g. if we have no overlap: $0800-$cfff and $d000-$ffff).
With the old packing scheme, Packed files have a safety margin at the end, means, their end address was usually a few bytes higher than on the original file (sometimes even more, if a huge literal is found at the end of a file). This could of course bring you in trouble if you came close to the IO range or executed code with it. With the new overlapping packing scheme this is however history.

The loader does not implement a bunch of workwrounds to support virtual drives with broken firmware. Get your latest firmware and bother the developer of that hardware to fix things. The loader worked fine so far with a 1541u-II and Firmware 3.7, as well with an up to date Vice.

Building
--------

The .asm files need ACME 0.94 or newer, i didn't focus much on that, as i always use the current version from SVN. At this time 0.97. So if anyone tries to compile this with the medieval version 0.93, it will fail, sorry :-)

Dirart
------

The dirart is expected like a normal dir-layout would look like on a screen, see the exampledir.bin and exampledir.png, so all you need to do is draw things on a screen (or multiple screens if you need a larger dir-layout) and save the screen. Content must be aligned top/left.

Synching to music
-----------------

As loading times differ from floppy to floppy, loading can introduce timing jitter in a demo that is difficult to handle when music has to happen on spot with a certain effect.
To make the parts of a demo happen always at the same time, there are a few possibilities to cover that.
The framework gives a frame-counter and macros to cover loading with a timer that elapses some time after loading has finished, don't choose values too tight if doing so:

+setup_sync $180		;setup timer to $180 frames
jsr link_load_next_comp		;load
+sync				;wait for timer to elapse -> so this happens $180 frames after the setup

Other possibilities are to use syncpoints in SIDs and sync to those syncpoints. There's a syncpoint.inc included to have a single file to edit with all framecounts, so that the timings are not scattered all over your project.

Technical Limitations
---------------------

1. Drive Speed / Wobble

It might be a good idea to have a look at the rpm*.prgs at this URL:
https://sourceforge.net/p/vice-emu/code/HEAD/tree/testprogs/drive/rpm/

Please check your drive so that it is running at 300 rpm. In fact the loader can cope with floppys that are off that range by a few rounds, still it will have retries and possible read errors and hickups. Belt driven drives (which can be found in 1541, 1541C and 1541-ii) show a so called wobble, so the drive's rotation speed is drifting between a minimum and maximum in a sinus like manner. There's also 1541-ii drives out there (jpn, sankyo) that come along with a direct driven spindle. The plot-programs show a quite straight line then. On other drives, one can see how strong the amplitude of the wobble is. I have drives that left the green area by quite a bit, and have drives with alps-mechanics, that jitter more or less randomly. They throw the most read errors in my tests. Things were better, when the disks were written with a drive, that has a direct drive. If the disks are written with the same floppy, on can assure, that the track alignment is the same, but when written with a strong wobble and read back with the same strong wobble, amplitudes can add up to twice the wobble, reading speed can increase and decrease fast and that can trip the gcr-read-loop of the loader. There are many sanity checks implemented, as the eor-checksum of a sector is a bit weak, it can happen that the checksum is still okay, but the content of a sector is wrong. So the last trailing bytes after the checksum are also checked for being zero. Also the header is double checked, if track, sector and disk-id are correct besides the checksum. A lot of tests on all kind of hardware showed, that there's the possibility of a file being loaded with a corrupt content in very seldom cases due to this physical restrictions. The gcr-loop used in the rom takes only 19 cycles for a byte to read from disk, that might allow for more tolerance regarding that matter.

2. Electromagnetic Fields

During the testing, i discovered, that my screen significantly influences the floppy, that was located beneath. With the screen turned off, i had no read erroros happening, but when having the screen turned on, there's suddenly read-errors and checksum-errors. Creating a distance of around 30cm between floppy and screen, ceased all the problems and reading was error-free. Moving the floppy side by side to the screen, made it even fail completely. I first suspected a bad timing in the gcr-loop, but no matter how i shifted the timing, the errors still happened. The errors also occur on a SX64, that has the screen and the floppy built in at a fixed distance, so not much one can change here.

3. Heat

When stacked with other floppys, heat can't dissipate well from the old floppydrives, what leads to more read and checksum errors as well, and can lead to false positive checksum at some point and make loading fail. Been there, seen that :-(

4. Cables

Thanks to Ikwai i happened to have hands on a setup with a 2m unshielded iec-cable on which the 72-cycle 2bit-ATN transfer failed, as it missed it's timing. A slower timing solved the problems, but also exchanging that cable.

5. Unsettled Head

Starting to read from disk directly after stepping can also lead to checksum errors, so waiting a bit is adviseable. To not waste time, data is sent directly after stepping (some even do a halftrack in between, like sparkle. Had done that too in the very early versions of bitfire, but dropped that to save code, it didn't bring much speed gain)

6. Spin Up

During spin up of the drive, the gcr read might fail, as it misses the window where timing is optimal, either by spinning too slow yet, or by overshooting. This also can cause checksum errors. As one file ends on the same sector, where a new one begins, we can force the last sector of a file to be read last. Upon loading of the next file, the sector is already cached and present and the first file chunk can be send during spin up. This covers at least a few errors that else occur on spin up.

7. Glitchy hardware

The buslock issues with THCM's SX64 are history and i found a solution to make this work also on his machine. Ikwai owns a floppy and cable, where the 72 cycles transfer fails. It needs both the combnation form an extra long cable and that certain floppy. The cable works with all other kind of floppys, as well as the floppy works with all other kind of cables. But combined they fail.

8. Jitter

The gcr read usually introduced 3 cycles jitter by using bvc * to sync on a byte_ready. Krill showed, that one can also use a bunch of bvs .loop to sync on a byte, what introduces only two cycles of jitter and allows for a better timing within range.

9. Fast Stepping

The so called shrydar-stepping that does the second half-step already $0c00 cycles after the first, works on most drives for single steps, but i also bumped in a 1541-ii and an old longboard that would choke and end up on a half track and failing to read any further.

Testing
-------

Bitfire has been tested on various hardware, 5 1541-ii drives (Newtronics, Sankyo, JPN mechanics), 2 1541C (shortboard, Newtronics) and 1 1541 (longboard, 3 different ALPS mechanics), 11 different cables, daisychained and single drive, 4 c64 (breadbox) 1 c64c, SX64 with ALPS drive.
