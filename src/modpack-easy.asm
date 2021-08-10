;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                              ZEPTOPOLIS MODPACK
;                                  Easy Mode
;                                Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; This is an easy Modpack. Maintenance costs are about half the normal amount
; for major buildings, and disasters are very uncommon. The starting Treasury
; is also increased.

* = $100d            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MODPACK TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following tables can be used to modify the behavior of the game, to make
; game balance harder or easier. Save a set of 58 bytes to the MODPACK address,
; and load using the in-game "L" city load option.

; Musical Mode
; Dorian         
Mode:       .byte 147,159,163,175,183,191,195,201         

; Musical Theme
Theme:      .byte $55,$aa,$55,$ab

; Starting conditions
StartYear:  .word 2021
StartTreas: .word 700
LakeCount:  .byte 6

; Build costs
NewDevCost: .byte 5
UpdateCost: .byte 10

; Yearly maintenance costs of maintainable structures
;                 Wind Farm,School, Firehouse, Clinic, Park
MaintCosts: .byte 3,        7,      5,         7,     1

; Assessed values for NEARBY structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusNVals:   .byte 0,         0,      2,         0,      0,    2,     0
HomeNVals:  .byte 0,         2,      0,         2,      1,    0,     2

; Assessed values for ADJACENT structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusAVals:   .byte $ff,       0,      1,         0,      1,    1,     2
HomeAVals:  .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff

; Fire Risk configuration
; This is the annual risk of an unprotected property (Home or Business) burning
; down. It is expressed as a carry at a specific bit value.
FireRisk:   .byte %00000010

; Earthquake configuration
; The next Earthquake will happen QuakeFreq years from now, plus 0-7
; years. When an Earthquake happens, this timer will be reset.
; QuakePower determines how much damage an earthquake does
; The default is 15 + (0-7) years, or between 15-22 years
QuakeFreq:  .byte 50
QuakePower: .byte 5

; Tornado configuration
; Tornadoes are checked every turn. If the pseudo-random value is 0, there will
; be a Tornado.
; TornPath determines the maximum path length
; The default is a 1 in 8 chance per year, with a maximum path length of 6
TornFreq:   .byte %00001000
TornPath:   .byte 4

; Pandemic configuration
; The next Pandemic will happen PandFreq years from now, plus 0-8 years.
; When a Pandemic happens, this timer will be reset.
; The default is 25 + (0-7) years, or between 25-32 years
PandFreq:   .byte 100
