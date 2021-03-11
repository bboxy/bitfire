;mem location of sid player that utters syncpoints
link_syncpoint			= $0e81

;values for the various selfdefined syncpoints
sync_part_xxx_fadein		= $00
sync_part_xxx_fadeout		= $01

;central defines for parts that sync around loading operations. So all counters are maintained in a single file and not spread over many many parts 
;use like:

;!include "framework/syncpoints.inc"
;+setup_sync SYNC_FRAMES_PARTX
;jsr bitfire_loadnext_compd_
;+sync

SYNC_FRAMES_PARTX		= $0300
SYNC_FRAMES_PARTY		= $0380
