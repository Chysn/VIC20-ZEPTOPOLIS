;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                              ZEPTOPOLIS MODPACK
;                                {MODPACK name}
;                                  {author}
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ABOUT THIS MODPACK
; {MODPACK description}
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

* = $100d            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MODPACK TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following tables can be used to modify the behavior of the game, to make
; game balance harder or easier. Set the values below and assemble the set of
; bytes to $100d. Load using the in-game "L" city load option.

; Musical Mode
; Dorian         
Mode:       .byte 147,159,163,175,183,191,195,201         

; Musical Theme
; Tempo is number of 60th of a second between rotations
Theme:      .byte $33,$44,$55,$66
Tempo:      .byte 10

; Starting conditions
StartYear:  .word 2021
StartTreas: .word 500
LakeCount:  .byte 6

; Build costs
NewDevCost: .byte 5
UpdateCost: .byte 10

; Yearly maintenance costs of maintainable structures
;                 Wind Farm,School, Firehouse, Clinic, Park
MaintCosts: .byte 5,        15,     10,        10,     1

; VALUE TABLES
; The tables below are the rules for property values. The first table contains
; values for structures nearby a Home or Business. This means connected by
; up to four cardinally-adjacent roads. Note that Wind Farms are a special case
; in that they don't need roads.
; 
; The second table contains values for structures CARDINALLY-adjacent to a Home
; or Business, meaning directly to the north, south, east, or west.
;
; Note that a Home will always be counted as "nearby" itself, so a value in
; that cell will always be assessed.
;
; Please ensure that the MAXIMUM possible assessment for each type of property
; (Business, Home) is 9.

; Assessed values for NEARBY structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusNVals:   .byte 0,         0,      2,         0,      0,    2,     0
HomeNVals:  .byte 0,         2,      0,         2,      1,    0,     2

; Assessed values for ADJACENT structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusAVals:   .byte $ff,       0,      1,         0,      1,    1,     2
HomeAVals:  .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff

; DISASTER TABLES

; Fire Risk configuration
; This is the annual risk of an unprotected property (Home or Business) burning
; down. It is expressed as a carry at a specific bit value.
FireRisk:   .byte %00010000

; Earthquake configuration
; The next Earthquake will happen QuakeFreq years from now, plus 0-7
; years. When an Earthquake happens, this timer will be reset.
; QuakePower determines how much damage an earthquake does
; The default is 15 + (0-7) years, or between 15-22 years
QuakeFreq:  .byte 15
QuakePower: .byte 15

; Tornado configuration
; Tornadoes are checked every turn. If the pseudo-random value is 0, there will
; be a Tornado.
; TornPath determines the maximum path length
; The default is a 1 in 8 chance per year, with a maximum path length of 6
TornFreq:   .byte %00100000
TornPath:   .byte 6

; Pandemic configuration
; The next Pandemic will happen PandFreq years from now, plus 0-8 years.
; When a Pandemic happens, this timer will be reset.
; The default is 25 + (0-7) years, or between 25-32 years
PandFreq:   .byte 25