What is it
----------

Bitfire is a fixed interleave loadersystem with depacker, a basic framework and an image writing tool. Aim was to make the loader as fast as possible while being as tiny as possible. So at some points size and speed had to be traded against each other. An own, however d64-compatible (bam copy is sufficient), file format is introduced to make the code less complex and loading faster. Also, functions that are not used regularly (like turn disk detection) are available as statically linkable functions and thus make the resident part on c64 side even smaller. Being that tiny ($8b to $1ea bytes, depending on configuration) and still fast, makes it perfect for being used in demos. The imaging-tool creates diskimages with all demofiles and a dirart on it. Also it is accompanied by a lz-packer based on doynamite, however smaller in code and a bit faster than that, while yielding nearly the same results. The packer supports a lot of functionality and thus makes other tools obsolete, as it can split files, reference data in previous files, write in several formats, create sfx and much more. The framework supports a base irq and safe loading hooks, as well as a loadnext functionality (but this feature is also available without framework, see examples), however loading by filenumber is still supported if your demo is in need of random file access (like the comaland endpart)

Bitnax
------

The packer supports output of both, sfx as well as levelpacked files suitable for Bitfire (--bitfire switch)

--overlap					Enable the old style packing with files overlapping at the end, default with sfx
--sfx startaddr					Spit out an executable for c64
--bitfire					Produce a bitfire compatible output with special encoding scheme and zero overlap
--level						Spit out file in old doynamite format
--full-dict					Reference also the stuff that is cut off the file when using --cut-input-addr. The referenced part must be in c64-memory however to make this work upon depacking. It saves a few bytes on splitted files, as more matches can be found
--load-addr					Force packed file to be loaded to a differnet location (for e.g. if it starts/ends in IO range)
--depack-to					Depack file to a different location than the one given by the .prg header
--relocate-to					Move file to alternative address, prior to packing
--binfile					Given file is without 2 byte .prg header, --relocate-to can help out
--checksum					Build a 8 bit eor checksum over whole file. Good for testing data integrity after loading/depacking during development
--exit_on_warn					Exit with an error, even if the packer just gives a warning. You will be happy for that on messy outputs during a make process.
--cut-input-addr first last			Only compress the given part of a file. Addresses can be given in hex (0x0000, $0000) or decimal format. Addresses are absolute.
--offset-lengths s1/s2/s3/s4:l1/l2/l3/l4	Do things with your own magic offset-lengths, only works with --raw, --level or --sfx, not --bitfire, as it has its own fixed set of offsets
--emit-offset-tables tables.asm			Print out tables for easier integration into your selfmade decruncher
--statistics					Print some statistics
--trace-coding					Print out loads of gibberish that noone can follow anyway, but was great to trace bits when i did changes on the encoding :-D
--best-offset-tables				Iterate to find the optimal offset tables, this is  S L O W
--include-tables				Add tables to --level or --raw file

Disclayout
----------

When d64write writes files in bitfire format the full 256 byte of each sector are used for data and for that no track/sector link is added. This can be done, as the sectorchain is calculated statically from the beginning track/sector on. The needed information for each file is stored in an own directory format. The directory lies on max. 2 blocks in track 18 (means 126 files max). It fills up sector 18 as well as 17 if needed. Each fileentry consists of 4 bytes:
byte 0: load-address lowbyte
byte 1: load-address highbyte
byte 2: (filelength-1) lowbyte
byte 3: (filelength-1) highbyte

Thus 63 files fit into one sector (the remeining bytes contain the diskside info and teh start-position of the first file on the current directory sector). This means, 63 files can be loaded sequentially without having any seeking action due to fetching new direntries. The last byte of the dir sectors represent the diskside the files are on. This value will be checked if another diskside is requested. The values range from $f0 to $fe, so demos with up to 15 different disksides can be created (hi Offence! :-P). Also this is less error prone than the usual checks on the disk id.

d64write
--------

d64write generates a suitable .d64 for you which can be read by bitfire and incorporates all the workflow for final image creation into a single tool. So it writes hidden files for you, while keeping maximum reading performance and providing a dirart linked to the bootloader.

After the bitfire formatted files are written, standard files are added to the diskimage. So there's still the possibility to add files that can be loaded normally, mixing both types on a disk is no problem. So a bigger bootloader or adding a note is no problem. It is even possible to add further files to a disk with original gear, as the BAM is maintained and used blocks are thus protected from allocation/overwriting. However bitfire-files must be written first, as it assumes starting at track 1, sector 0, to avoid data loss this requres the -c flag to be set, so that d64write starts wit ha fresh formated .d64.

Next up a small file, the bootloader, can be placed into the remaining sectors on track 18 to save blocks. It should be small enough to fit there, or an error occurs. Usually that program should not do more than install the loader and load/run the first file (bootstrap) on disk that then starts the demo(side).

As a final step a dirart can be added to the dir, if there's still enough sectors free to accomodate it. An error will occur if not. d64write accepts a saved screen for that purpose, so it should be easy to create dirarts with e.g. a petscii editor (however take care that not all petscii symbols are accepted for dirart). From that screen every first 16 chars per row are taken for creating a dirart. The number of rows to be read in can be specified. The first row is used to specify header and id separated by an arbitrary char (see example dirart coming along with this release).

If you want to place multiple files, (in standard format, for bitfire format it is mandatory to write in one go), you can add the -b or -s option multiple times to one commandline. So the right order is kept on a single call. Files in bitfire format are written sequentially so that no unecessary seektimes are created, that can be broken when standard files are placed in between. That said, it is wisely to place files on disk in the same order to be loaded, if being loaded once (random access is possible of course, but gives penalties due to excessive seeking).

You can choose to write with different interleaves, however 4 always has been the best choice in any tested scenario and thus is the default. If you change the interleave you also have to change it with the loader as it needs to calculate the sectors belonging to each file by this value. To do so simply change the value for BITFIRE_CONFIG_INTERLEAVE in config.inc.

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
bitnax			The packer, details see above
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
NTSC support is broken at the moment

Add/remove functionality
------------------------

In config.inc you may turn off certain functionality and by that save memory (is it necessary with that size however?). The plain loadraw-function will consume $7e bytes only. With all functions enabled the resident size is still smaller than $200 bytes, small, isn't it?

Else, choose from the following functions:
BITFIRE_INCLUDE_DECOMP          = 1             ;Include decompressor including on the fly decompression capabilities
BITFIRE_INCLUDE_FRAMEWORK       = 1             ;Include helpful calls (link_*) for loading files from a safe spot in mem, load_next functionality with no overhead, or to add an io safe base-irq

Also you might want to try the following:
BITFIRE_CONFIG_MOTOR_ALWAYS_ON  = 1		;This lets the motor spin all way round like a record and thus save valueable spinup time. If it goes on your nerves, set it to 0 and the drive will stop after loading.

Those values speak for themself:
BITFIRE_ZP_ADDR			= $02
BITFIRE_INSTALLER_ADDR		= $1000
BITFIRE_RESIDENT_ADDR		= $0200

Yes, i prefer to place the whole thing at $0200-$3ff, there's no point in wasting precious ram at $0800 or $0c00, charsets or screens can be placed there or a bigger sid that reaches from $0800-$1fff.

Framework calls and most macros start with link_*, original bitfire functions with bitfire_*

Examples
--------

lda #$00
jsr bitfire_loadraw_

Will load first file from disk.

lda #$02
jsr bitfire_loadraw_
jsr link_load_next_raw

Loads file number 3 and then file number 4. If all files are loaded sequentially from disk, it is enough to use those link_load_next* calls. File number will be reset to 0 upon disk change and init. If a file is loaded by file number beforehand, the next file will be based on that file number.

lda #$01
jsr bitfire_loadcomp_

Will load and on the fly depack second file from disk.

lda #$02
jsr bitfire_loadraw_
jsr bitfire_decomp_

Will load third file from disk and depack it afterwards.

!src "request_disc.asm"
lda #$f1
jsr bitfire_request_disc_

Will wait until side 2 is inserted into floppy and directory is sucessfully read. If you want to wait for disc change in a different manner, just have a look into request_disc.asm, there are just a few lines of code.

lda #$80
jsr bitfire_send_byte_
...

Will start code upload to the floppy. Have a look into reset_drive.asm for a useful example. One should call it at the end of the demo to bring back the floppy to a sane state.

lda #BITFIRE_LOAD_NEXT
jsr bitfire_loadraw_

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

to create a levelpacked file use the bitnax packer with the --bitfire flag. It will pack and adjust the load-address accordingly. Be aware that a bunch of switches do not work when using the --bitfire flag, for e.g. a different offset table.
The commandline might look like: ./bitnax --bitfire -o file.lz file.prg
If a file is going under IO it is best to split it somewhere before the IO range starts, for e.g.:
bitnax --bitfire -o part1.lz --cut-input-addr 0x0801 0xcfff mypart
bitnax --bitfire -o part2.lz --cut-input-addr 0xd000 0xffff mypart

This will result in two files that can be loaded/depacked in one go by link_load_next_double. If the resulting part2.lz happens to be that small that it has a load-address below $e000, just move it further upwards with --load-addr 0xe000 to avoid it being loaded under IO and crash. However be careful with the --load-addr flag, the file will be slightly bigger as it can't be depacked in place anymore.
Basically the load_next_double call is loading the first part and decrunching it on the fly, then loading the next part raw, switching off the IO, depacking that and turning on IO again.

Other interesting options are:
--full-dict

If you split a file and load both parts one after another, you can add this switch to the second part. It will then be packed with using the already loaded data as dictionary, what will result in smaller files.

Another option might be the --depack-to switch. Here you can load the file to a comfortable gap and depack it later to another destination when the memory is available.

NMI-gaps
--------

What is this? This option leaves 6 bytes @ $0202 and 3 bytes @ $0302 free of code. This way THCM can let his NMI pointer point to those locations to achieve a stable NMI when he is doing some of his hot sample shit :-D So as soon as you need stable NMIs and do it the Ninja way, you might be glad for this option.

Zero-Overlap
------------

Bitnax now creates files that have no overlap at its end and thus can be completely depacked in place within its strict barriers. This is the new default behaviour and also enabled by default in config.inc. To switch back to the old behaviour (with tighter margins though) use the --overlap switch on bitnax and disable the BITFIRE_ZERO_OVERLAP option in config.inc

Zeropage usage
--------------

The loader needs a single byte in zeropage (default $04) the packer needs another 5 bytes ($05 - $09). The values can be changed in the source accordingly, but they have been placed where they are for a good reason:
$02/$03 can then be used by music and thus all zeropage from $0a upwards can be used, even if there's code inside the zeropage it can easily grow and even reach into the stack without the need to take care of the addresses being used by bitfire.

Bank switching
--------------

to switch the VIC banks you can set $dd00 with $00..$03 at any time, also while loading:

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

Building
--------

The .asm files need ACME 0.94 or newer, i didn't focus much on that, as i always use the current version from SVN. At this time 0.95.5. So if anyone tries to compile this with the medieval version 0.93, it will fail, sorry :-)

Zeropage-usage
--------------

When the loader and depacker is idle, you can use the whole zeropage and leaving any garbage there, also the addresses reserved for the loader. There's one exception, the loader uses $00 and places the value #$37 there, whenever you use $00, you need to restore this value before loading again, or things break.

Dirart
------

These are the positions on screen where the dirart is expected, it looks simply like a normal dir-listing, in doubt, there is a exampledir.bin and a .png to visualize that.

Synching to music
-----------------

As loading times differ from floppy to floppy, loading can introduce timing jitter in a demo that is difficult to handle when music has to happen on spot with a certain effect.
To make the parts of a demo happen always at the same time, there are a few possibilities to cover that.
The framework gives a frame-counter and macros to cover oading with a timer that elapses some time after loading has finished, don't choose values too tight if doing so:

+setup_sync $180		;setup timer to $180 frames
jsr bitfire_loadnext_compd_	;load
+sync				;wait for timer to elapse -> so this happens $180 frames after the setup

Other possibilities are to use syncpoints in SIDs and sync to those syncpoints. There's a syncpoint.inc included to have a single fiel to edit with all framecounts, so that the timings are not scattered all over your project.
