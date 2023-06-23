!ifndef CONFIG_LOADED {

;options for resident part
CONFIG_RESIDENT_ADDR		= $f000
CONFIG_ZP_ADDR			= $02		;maximum 11 bytes needed, 5 for loader, 6 for depacker
CONFIG_LAX_ADDR			= $00		;Usually save to have the value $37 here, but can be also set elsewhere, but must stay persistent during loading

;options for installer
CONFIG_INSTALLER_ADDR		= $1000
CONFIG_RESIDENT_AUTOINST	= 1		;Set this to zero if you want to do the install of the resident part on your own, but why would you want to do so anyway? :-D

;Framework options, certain features can be switched on/off separatedly
CONFIG_FRAMEWORK		= 1		;Enable/Disable whole framework, including load_next calls
CONFIG_FRAMEWORK_BASEIRQ	= 1		;Include a base irq handler into the framework
CONFIG_FRAMEWORK_FRAMECOUNTER	= 1		;Include framecounter handling
CONFIG_FRAMEWORK_MUSIC_NMI	= 1		;should base irq run in NMI or raster IRQ?

;Include a decompressor and by that on the fly decrunching?
CONFIG_DECOMP			= 1		;Include decompressor and on the fly decompression

;Include the loader
CONFIG_LOADER			= 1		;Include the loader or maybe decide for a standalone depacker + framework for onefilers

CONFIG_AUTODETECT		= 1		;Autodetect CIA + SID and store the results in the resident part for later use, so upcoming parts do not need an own detection anymore but can read out that values again

CONFIG_NMI_GAPS			= 0		;If you have Ninjy-style NMI-handlers @ $0200 and $0300 to install while still using the loader, choose that option and 10 bytes will be reserved at each location for your code to stay.

CONFIG_NEXT_DOUBLE		= 0		;If enabled the old load_next_double code stays in resident area (Hi Bob/Censor!)

;options for drivecode
CONFIG_MOTOR_ALWAYS_ON		= 0		;If you want to be annoying, let the motor spin during the whole demo :-D

CONFIG_DEBUG			= 0		;transfer retry-counts to c64 for benchmarking

CONFIG_LOADED			= 1		;avoid including config twice

}
