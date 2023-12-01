!ifndef CONFIG_LOADED {

;options for resident part
CONFIG_RESIDENT_ADDR		= $f000
CONFIG_ZP_ADDR			= $02		;maximum 11 bytes needed, 5 for loader, 6 for depacker
CONFIG_LAX_ADDR			= $00		;Usually save to have the value $37 here, but can be also set elsewhere, but must stay persistent during loading

;options for installer
CONFIG_INSTALLER_ADDR		= $1000
CONFIG_RESIDENT_AUTOINST	= 1		;Set this to zero if you want to do the install of the resident part on your own, but why would you want to do so anyway? :-D

CONFIG_LOADER_ONLY		= 0		;Only compile loader standalone

;CONFIG_AUTODETECT		= 1		;Autodetect CIA + SID and store the results in the resident part for later use, so upcoming parts do not need an own detection anymore but can read out that values again

CONFIG_NEXT_DOUBLE		= 0		;If enabled the old load_next_double code stays in resident area (Hi Bob/Censor!)

;options for drivecode
CONFIG_MOTOR_ALWAYS_ON		= 0		;If you want to be annoying, let the motor spin during the whole demo :-D

;enable crt loader only
CONFIG_CRT			= 0
}
