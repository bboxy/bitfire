;options for resident part
BITFIRE_RESIDENT_ADDR		= $0200
BITFIRE_ZP_ADDR			= $04

;Framework options, certain features can be switched on/off separatedly
BITFIRE_FRAMEWORK		= 1		;Enable/Disable whole framework
BITFIRE_FRAMEWORK_BASEIRQ	= 1		;Include a base irq handler into the framework
BITFIRE_FRAMEWORK_FRAMECOUNTER	= 1		;Include framecounter handling
BITFIRE_FRAMEWORK_MUSIC_NMI	= 1		;should base irq run in NMI or raster IRQ?

;Include a decompressor and by that on the fly decrunching?
BITFIRE_DECOMP			= 1		;Include decompressor and on the fly decompression
BITFIRE_DECOMP_ZERO_OVERLAP	= 1		;Packed files have no overlap and can be completely depacked in place. Bitnax by default creates files that meet that spec. If you wish the old behaviour, use --overlap

BITFIRE_NMI_GAPS		= 0		;Leaves gaps @ $0200 and $0300 (if resident part is at default location) for NMI handlers, needed for stable NMIs with THCM's digistuff.

BITFIRE_AUTODETECT		= 1		;Autodetect CIA + SID and store the results in the resident part for later use, so upcoming parts do not need an own detection anymore but can read out that values again

BITFIRE_WORLD_DOMINATION	= 1		;Soon to come :-D

;options for installer
BITFIRE_INSTALLER_ADDR		= $1000
BITFIRE_RESIDENT_AUTOINST	= 1		;Set this to zero if you want to do the install of the resident part on your own, but why would you want to do so anyway? :-D

;options for drivecode
BITFIRE_CONFIG_MOTOR_ALWAYS_ON	= 0		;If you want to be annoying, let the motor spin during the whole demo :-D
BITFIRE_CONFIG_INTERLEAVE	= 4		;Turned out to be the best choice, if you really think you know it better, change it and get slowed down :-)

;add extra debug functionality, not intended for demos
BITFIRE_DEBUG			= 0

;constants, better don't touch
BITFIRE_UPLOAD			= $80
BITFIRE_LOAD_NEXT		= $ef
BITFIRE_REQ_DISK		= $f0
BITFIRE_IS_INCLUDED		= 1

BITFIRE_SID_NEW			= 1
BITFIRE_CIA_NEW			= 2
