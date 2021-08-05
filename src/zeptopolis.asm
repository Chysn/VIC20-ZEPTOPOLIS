;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                                  ZEPTOPOLIS
;                            (c)2021, Jason Justian
;                  
; Assembled with XA
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; This software is released under the Creative Commons
; Attribution-NonCommercial 4.0 International
; License. The license should be included with this file.
; If not, please see: 
;
; https://creativecommons.org/licenses/by-nc/4.0/legalcode.txt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; BASIC LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This is the tokenization of the following BASIC program, which
; runs this game
;     42 SYS4160
* = $1001
Launcher:   .byte $0b,$10,$2a,$00,$9e,$34,$31,$36
            .byte $36,$00,$00,$00
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MODPACK TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The following tables can be used to modify the behavior of the game, to make
; game balance harder or easier. Save a set of 57 bytes to the MODPACK address,
; and load with LOAD"MODPACK",8

; Musical Mode
; Dorian         
Mode:       .byte 147,159,163,175,183,191,195,201         

; Musical Theme
Theme:      .byte $55,$aa,$55,$ab

; Starting conditions
StartYear:  .word 2021
StartTreas: .word 500
LakeCount:  .byte 6

; Build costs
NewDevCost: .byte 5
UpdateCost: .byte 10

; Yearly maintenance costs of maintainable structures
;                 Wind Farm,School, Firehouse, Clinic, Park
MaintCosts: .byte 5,        15,     10,        10,     2

; Assessed values for NEARBY structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusVals:    .byte 0,         0,      2,         0,      0,    2,     0
HomeVals:   .byte 0,         3,      0,         2,      1,    0,     2

; Assessed values for ADJACENT structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
BusAVals:   .byte $ff,       0,      0,         0,      2,    1,     2
HomeAVals:  .byte $ff,       0,      $ff,       $ff,    1,    $ff,   $ff

; Earthquake configuration
; The next earthquake will happen QuakeFreq years from now, plus rand(QuakMarg) 
; years. When an earthquake happens, this timer will be reset.
; QuakePower determines how much damage an earthquake does
; The default is 15 + (0-7) years, or between 15-21 years
QuakeFreq:  .byte 15
QuakeMarg:  .byte %00100000
QuakePower: .byte 15

; Tornado configuration
; Tornados are checked every turn. If the pseudo-random value is 0, there will
; be a tornado.
; TornPath determines the maximum path length
; The default is a 1 in 8 chance per year, with a maximum path length of 6
TornFreq:   .byte %00100000
TornPath:   .byte 6
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Configuration
MENU_ITEMS  = 9                 ; Number of actions in menu
LOW_CONF    = %00100000         ; High business attrition
HIGH_CONF   = %00001000         ; Low business attrition

; Character Constants
CHR_PROG    = $f8               ; Progress Bar
CHR_CURSOR  = $3f               ; Cursor
CHR_CANCEL  = $22               ; Action Cancel
CHR_ROAD    = $23               ; Road Placeholder
CHR_UHOME   = $24               ; Unoccupied Home
CHR_UBUS    = $25               ; Unoccupied Business
CHR_WIND    = $26               ; Wind Farm
CHR_SCHOOL  = $27               ; School
CHR_FIRE    = $28               ; Firehouse
CHR_CLINIC  = $29               ; Clinic
CHR_PARK    = $2a               ; Park               
CHR_HOME    = $2c               ; Home
CHR_BUS     = $2d               ; Business
CHR_LAKE    = $2e               ; Lake (obstacle)
CHR_BURN    = $2f               ; Burned down
CHR_POP     = $3a               ; Population icon
CHR_THUMB   = $3c               ; Happiness icon

; Color Constants
COL_CURSOR  = 6                 ; Cursor, blue
COL_ROAD    = 0                 ; Road, black
COL_UNOCC   = 0                 ; Unoccupied properties, black
COL_OCC     = 0                 ; Occupied properties, black
COL_SCHOOL  = 4                 ; School, purple
COL_WIND    = 4                 ; Wind Farm, purple
COL_FIRE    = 2                 ; Firehouse, red
COL_CLINIC  = 6                 ; Clinic, blue
COL_PARK    = 5                 ; Park, green
COL_LAKE    = 6                 ; Lake, blue
COL_BURN    = 2                 ; Burned Down, red

; Index and Bit Value Constants
IDX_ROAD    = 1
BIT_WIND    = %00000001
BIT_SCHOOL  = %00000010
BIT_FIRE    = %00000100
BIT_CLINIC  = %00001000
BIT_PARK    = %00010000
BIT_HOME    = %00100000
BIT_BUS     = %01000000
BIT_ROAD    = %10000000

; Directional Constants
NORTH       = 0
EAST        = 1
SOUTH       = 2
WEST        = 3
FIRE        = 4
SAVE        = 5
LOAD        = 6

; Game Memory
UNDER       = $00               ; Character under pointer
TMP         = $01               ; Subroutine temporary storage
TMP_PTR     = $02               ; Temporary pointer (2 bytes)
CURR_ST     = $04               ; Current structure list (nearby or adjacent)
MISR_FL     = $05               ; Minimum ISR Flag (only timer and music)
PREV_X      = $08               ; Previous x coordinate
PREV_Y      = $09               ; Previous y coordinate
BUILD_IX    = $0a               ; Build index
PROGRESS    = $0b               ; Progress Bar
HAPPY       = $0c               ; Satisfaction (10% - 90%)
BUILD_MASK  = $30               ; Build mask (for wind farms)
ACTION_FL   = $31               ; Action flag
NEARBY_ST   = $32               ; Nearby structures bitfield
PATTERN     = $33               ; Current road search pattern
STEP_COUNT  = $34               ; Step count for pattern search
STEPS       = $35               ; Part of pattern with remaining steps
CURR_DIR    = $36               ; Current search direction
LAST_DIR    = $37               ; Last search direction
RNDNUM      = $38               ; Random number tmp
SOLD_HOMES  = $39               ; Number of homes sold this turn
SOLD_BUSES  = $3a               ; Number of businesses sold this turn
VALUE       = $3b               ; Current property value
ADJ_ST      = $3c               ; Adjacent structures bitfield
HOME_COUNT  = $3d               ; Home Count
BUS_COUNT   = $3e               ; Business Count
POP         = $3f               ; Population (2 bytes)
YR_EXPEND   = $41               ; Previous year expenditure (2 bytes)
YR_REVENUE  = $43               ; Previous year revenue (2 bytes)
DENOM       = $45               ; Happiness calculator register 1
NUMER       = $46               ; Happiness calculator register 2 (2 bytes)
QUAKE_FL    = $48               ; Earthquake flag
SMART_COUNT = $49               ; Count of Homes with nearby Schools
SMART       = $4a               ; Education (+0 - 9%)
BUS_CAP     = $4b               ; Business Activity (2 bytes)
BUS_CONF    = $4d               ; Business Attrition value
COL_PTR     = $f3               ; Screen color pointer (2 bytes)
COOR_X      = $f9               ; x coordinate
COOR_Y      = $fa               ; y coordinate
PTR         = $fb               ; Pointer (2 bytes)
P_RAND      = $fd               ; Pseudorandom seed (2 bytes)
TIME        = $ff               ; Jiffy counter

; Music Player Memory
MUSIC_FL    = $50               ; Bit 7 set if player is running
MUSIC_REG   = $51               ; Music shift register (4 bytes)
MUSIC_TIMER = $55               ; Music timer
MUSIC_MOVER = $56               ; Change counter

; Game State
YEAR        = $1ffa             ; Year (2 bytes)
TREASURY    = $1ffc             ; Treasury (2 bytes)
QUAKE_YR    = $1ffe             ; Year of next earthquake (2 bytes)

; System Resources - Memory
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
SCREEN      = $1e00             ; Screen character memory (unexpanded)
BOARD       = SCREEN+66         ; Starting address of board
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eaef             ; System ISR return point
ORIG_HORIZ  = $ede4             ; Default horizontal screen position
HORIZ       = $9000             ; Screen position
VICCR4      = $9004             ; Raster location
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
SCRCOL      = $900f             ; Screen color
VIA1DD      = $9113             ; Data direction register for joystick
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA2DD      = $9122             ; Data direction register for joystick
VIA2PB      = $9120             ; Joystick port (for right)
CASECT      = $0291             ; Disable Commodore case
VIATIME     = $9114             ; VIA 1 Timer 1 LSB
POWERSOF2   = $8270             ; Bit value at index (character ROM)
KEYDOWN     = $c5               ; Key held down

; System Resources - Routines
PRTSTR      = $cb1e             ; Print from data (Y,A)
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
PLOT        = $fff0             ; Position cursor 
CHROUT      = $ffd2             ; Write one character

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Welcome:    jsr Setup           ; Set up hardware and initialize game
                                ; Unlike most of my games, there's no welcome
                                ;   screen or "press fire to start" text. The
                                ;   game simply starts when the program does.
            jsr InitGame        ; Initialize game
            ; Fall through to Main                                

Main:       jsr Joystick        ; Wait for joystick movement
            bmi Main            ; ,,
ch_joy:     cpx #FIRE           ; Has fire been pressed?
            beq Action          ; If so, go to Action mode
            txa                 ; Move joystick direction to A and
            pha                 ;   save it for later
            lda UNDER           ; Replace previous under
            jsr Place           ; ,,
            lda COOR_X          ; Store current coordinate for boundary-
            sta PREV_X          ;   checking purposes
            lda COOR_Y          ;   ,,
            sta PREV_Y          ;   ,,
            pla                 ; Get direction back
            jsr MoveCoor        ; Move in that direction
            jsr CheckBound      ; Check if new position is within boundary
            bcc new_pos         ; ,,
            lda PREV_X          ; If the new cursor is out of bounds,
            sta COOR_X          ;   reset it
            lda PREV_Y          ;   ,,
            sta COOR_Y          ;   ,,
new_pos:    jsr DrawCursor      ; Draw the new cursor
            lda #10             ; Wait a short time before reading the
            jsr Delay           ;   joystick again
            jmp Main            ; Return to Main
      
; Enter Action Mode
; Here player may
;   - Build
;   - Cancel
;   - Finish turn            
Action:     bit MUSIC_FL        ; Start music on first action
            bmi a_debounce      ; ,,
            jsr MusicInit       ; ,,
a_debounce: jsr Joystick        ; Debounce the fire button before entering
ch_fire:    cpx #FIRE           ; ,,
            beq a_debounce      ; ,,
            sec                 ; Set Action flag
            ror ACTION_FL       ; ,,
            lda UNDER           ; Get character at cursor
            cmp #CHR_LAKE       ; If it's a lake, allow no action here
            bne ch_space        ; ,,
            lsr ACTION_FL       ; Clear action flag
            jmp Main            ; Back to Main
ch_space:   cmp #$20            ; If there's already something else there,
            beq show            ;   start the build index on that item, to
            cmp #CHR_BURN       ;   make it harder to accidentally remove
            beq show            ;   intentional development. Fire is
            ldx #0              ;   considered "nothing else there."
            stx BUILD_IX        ;   ,,
            jsr DrawInfo        ; Show info about this structure
show:       ldx #0              ; Show the item being selected
            lda BUILD_IX        ; ,,
            clc                 ; ,,
            adc #CHR_CANCEL     ; ,,
            sta (PTR,x)         ; ,,
-wait:      jsr Joystick        ; Wait for the action
            bmi wait            ; ,,
            cpx #SAVE
            bne ch_load
            jmp TapeSave
ch_load:    cpx #LOAD
            bne rs_cursor
            jmp TapeLoad
rs_cursor:  lda #%00001111      ; Reset time to reset cursor flash
            sta TIME            ; ,,
            cpx #FIRE           ; If fire is pressed, build the selected item
            beq Build           ; ,,
ch_prev:    cpx #WEST           ; If moving left, decrement the menu
            bne ch_next         ; ,,
            dec BUILD_IX        ; ,,
            bpl action_d        ; ,,
            lda #MENU_ITEMS-1   ; Or wrap around to the last item
            sta BUILD_IX        ; ,,
            bne action_d        ; ,,
ch_next:    cpx #EAST           ; If moving right, increment the menu
            bne ch_cancel       ; ,,
            inc BUILD_IX        ; ,,
            lda BUILD_IX        ; ,,
            cmp #MENU_ITEMS     ; ,,
            bcc action_d        ; Or wrap around to the first item
            lda #0              ; ,,
            sta BUILD_IX        ; ,,
ch_cancel:  cpx #SOUTH          ; If moving down, cancel the action mode
            bne ch_end          ; ,,
            lda #0              ; Set build index to 0 (cancel)
            sta BUILD_IX        ; ,,
            beq Build           ; Then go to regular build subroutine
ch_end:     cpx #NORTH          ; If moving up, go directly to end of
            bne wait            ;   turn
            lda #MENU_ITEMS     ; EOT is the item AFTER the last menu item
            sta BUILD_IX        ; ,,
action_d:   jsr Joystick        ; Debounce joystick for action select
            bpl action_d        ; ,,
            jmp show            ; Go back to show item

; Build Item
; In BUILD_IX
Build:      jsr Joystick        ; Debounce joystick for build
            bpl Build           ; ,,
            lsr ACTION_FL       ; Clear Action flag
            ldy #9              ; Clear status display
            lda #$20            ; ,,
-loop:      sta SCREEN+56,y     ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            lda BUILD_IX        ; Get the built structure
            beq cancel          ;
            cmp #MENU_ITEMS     ; Has the turn been ended?
            bne ch_dup          ; ,,
            jmp NextTurn        ; ,,
ch_dup:     clc                 ; If the built character is the same as the
            adc #CHR_CANCEL     ;   one already here, then cancel the
            cmp UNDER           ;   build, to save the money
            beq cancel          ;   ,,
            lda NewDevCost      ; Default to new development cost
            ldx UNDER           ; But if there's something already there,
            cpx #$20            ; ,,
            beq buy             ; ,,
            lda UpdateCost      ; Use the update cost instead
buy:        jsr Spend           ; Try to do the spend
            bcs cancel          ; ,,
            jsr DrawStats       ; Update Treasury amount
            lda BUILD_IX
            cmp #IDX_ROAD       ; If the player is building a road,
            bne reg_item        ;   
            lda #%11011110      ;   character offset for placeholder.
reg_item:   clc                 ; Show the item at the cursor
            adc #CHR_CANCEL     ; ,,
            sta UNDER           ; ,,
            jsr Place           ; ,,
            jsr Roads           ; Rebuild roads to account for potential
            ldx #0              ;   changes to the infrastructure
            lda (PTR,x)         ; Get the new character under the pointer
            sta UNDER           ;   and set that as UNDER. Don't show the
            jmp Main            ;   cursor until moved.
cancel:     lda UNDER
            jsr Place
            lda UNDER
            cmp #$20
            bne to_main
            jsr DrawCursor
to_main:    jmp Main
            
; Interrupt Service Routine 
; Replaces the BASIC ISR and performs these functions
;   - Advances the timer, used for delays
;   - Disables fire animation for tornados
;   - Services music player       
;   - Handles Wind Farm rotation and fire animation
;   - Flashes cursor in Action mode
;   - Shakes screen during earthquakes
ISR:        inc TIME            ; Circumventing BASIC's clock, so advance it
            jsr MusicServ       ; Service music player
            bit MISR_FL         ; Min-ISR, Handle only timer and music
            bmi isr_r           ;   ,,
            bit ACTION_FL       ; Flash cursor if in Action mode
            bpl move_wind       ; If not in Action mode, move wind turbines
            ldy #6              ; Default to blue
            lda #%00010000      ; Every 16 interrupts, flash the cursor
            and TIME            ; ,,
            bne flash_cur       ; ,,
            iny                 ; ,,
flash_cur:  tya                 ; ,,
            jsr SetColor        ; ,,
            jmp icons
move_wind:  ldx #0
            lda #%00011111      
            bit TIME
            bne svc_quake
            bvc rotate
            ldx #5
rotate:     ldy #4
-loop:      lda WFAnim1,x
            sta WindFarm+1,y
            lda BurnAnim1,x
            sta Burning+1,y
            inx
            dey
            bpl loop
svc_quake:  bit QUAKE_FL
            bpl icons
-loop:      lda VICCR4          ; Do wait for raster to be at top, so that
            bne loop            ;   the earthquake isn't all rastery
            lda HORIZ
            eor #$01
            sta HORIZ
            jsr Rand15          ; Randomize volume for rumbling sound
            sta VOLUME          ; ,,
            jmp isr_r
icons:      bit TIME            ; Flash icons as warnings that Homes or
            bmi def_icons       ;   Businesses are at risk of leaving.
            lda BUS_CONF        ; If the population is insufficient to support
            cmp #LOW_CONF       ;   area business, flash the Population icon
            bne ch_happy        ;   ,,
            lda #$21            ;   ,,
            sta SCREEN+22       ;   ,,
ch_happy:   jsr Sad             ; Check satisfacton >= 65%
            bcs isr_r           ;   If carry set, satisfaction is OK
            lda #$21            ;   ,,
            sta SCREEN+34       ;   ,,
            bne isr_r
def_icons:  lda #CHR_POP        ; Flash the default icons back
            sta SCREEN+22       ; ,,
            lda #CHR_THUMB      ; ,,
            sta SCREEN+34       ; ,,
isr_r:      jmp IRQ             ; Return from interrupt

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; GAME MECHANICS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Draw Cursor
; At X,Y Coordinate
DrawCursor: jsr Coor2Ptr        ; Convert x,y coordinates to screen location
            ldx #0              ; Preserve character under cursor, for when it
            lda (PTR,x)         ;   moves out
            sta UNDER           ;   ,,
OnlyCursor: lda #CHR_CURSOR     ; Place the cursor
            sta (PTR,x)         ; ,,
            lda #COL_CURSOR     ; Set cursor color
            jsr SetColor        ; ,,
            rts

; Spend Treasury
; In A
; If there's no enough left, do not subtract, and set Carry 
; Always add amount to yearly expenditures, even if the amount is
; unaffordable. This is for budgeting information purposes.         
Spend:      sta TMP
            lda YR_EXPEND       ; Record to year's budget for expenditure
            clc                 ; ,,
            adc TMP             ; ,,
            sta YR_EXPEND       ; ,,
            bcc ch_afford       ; ,,
            inc YR_EXPEND+1     ; ,,
ch_afford:  ldy TREASURY+1      ; If high byte of Treasury is non-zero, then of
            bne subtract        ;   course the item is affordable
            lda TMP             ; If the amount in the Treasury is less than 
            cmp TREASURY        ;   the available amount, return with carry set
            beq subtract        ;   ,,
            bcs spend_err       ;   ,,
subtract:   lda TREASURY        ; Spend the amount
            sec                 ; ,,
            sbc TMP             ; ,,
            sta TREASURY        ; ,,
            bcs spend_ok        ; ,,
            dec TREASURY+1      ; ,,
spend_ok:   clc                 ; Succeed with carry clear
spend_err:  rts
            
; Revenue to Budget
; In VALUE
; The yearly revenue budget is added to the Treasury in Collect, below.
Revenue:    lda VALUE           ; Get revenue from the current VALUE
            bmi revenue_r       ; If it's negative, never mind
            clc                 ; Add value to Treasury
            adc YR_REVENUE      ; ,,
            sta YR_REVENUE      ; ,,
            bcc revenue_r       ; ,,
            inc YR_REVENUE+1    ; ,,
revenue_r:  rts  

; Collect Revenue
; Add yearly revenue budget (YR_REVENUE) to Treasury
Collect:    lda YR_REVENUE
            clc
            adc TREASURY
            sta TREASURY
            lda YR_REVENUE+1
            adc TREASURY+1
            sta TREASURY+1
            rts
                        
; Draw Stats 
; Population   = Column 23
; Treasury     = Column 7
; Satisfaction = Column 13
; Year         = Column 18
DrawStats:  ldy #1              ; Plot population display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx POP             ; Numeric population display
            lda POP+1           ; ,,
            jsr PRTFIX          ; ,,
            lda #$21            ; Space for population decrease
            jsr CHROUT          ; ,,
            ; TREASURY
            ldy #7              ; Plot treasury display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx TREASURY        ; Numeric treasury display
            lda TREASURY+1      ; ,,
            jsr PRTFIX          ; ,,
            lda #$21            ; Space for treasury decrease
            jsr CHROUT          ; ,,
            ldy TREASURY+1      ; If the Treasury has gone below 256,
            bne draw_sat        ;   add a second space as a guard against
            jsr CHROUT          ;   single-turn precipitous drop
            ; SATISFACTION
draw_sat:   jsr GetHappy        ; Compute happiness
            jsr GetSmart        ; Compute education level
            ldy #13             ; Plot satisfaction display
            ldx #1              ; ,,
            clc                 ; ,,            
            jsr PLOT            ; ,,
            lda HAPPY           ; Happiness display
            jsr CHROUT          ; ,,
            lda SMART           ; With school percentage
            jsr CHROUT          ; ,,
            ; YEAR
            ldy #18             ; Plot year display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx YEAR            ; Numeric year display
            lda YEAR+1          ; ,,
pr_year:    jmp PRTFIX          ; ,,
       
; Draw Budget
; On status line       
DrawBudget: lda #<BudgetR       ; Show Budget Revenue Header
            ldy #>BudgetR       ; ,,
            jsr PRTSTR          ; ,,
            ldx YR_REVENUE      ; Show Revenue
            lda YR_REVENUE+1    ; ,,
            jsr PRTFIX          ; ,,
            lda #<BudgetE       ; Show Budget Expenditure Header
            ldy #>BudgetE       ; ,,
            jsr PRTSTR          ; ,,
            ldx YR_EXPEND       ; Show Expenditure
            lda YR_EXPEND+1     ; ,,
            jmp PRTFIX          ; ,,
                        
; Roads
; Add the proper roads                  
Roads:      lda COOR_X          ; Save cursor coordinates
            pha                 ; ,,
            lda COOR_Y          ; ,,
            pha                 ; ,,
            lda #$00            ; Start at the top left corner of the board
            sta COOR_X          ; ,,
            sta COOR_Y          ; ,,
-loop:      jsr Coor2Ptr        ; Get the character at the pointer and check if
            ldx #$00            ;   it's a road
            lda (PTR,x)         ;   ,,    
            cmp #$10            ;   ,,
            bcs next_cell       ; If it's not a road, proceed to next character
            stx TMP             ; TMP is the road bitfield (0000wsen)
            lda #WEST           ; Check West
            jsr CheckRoad       ; ,,
            rol TMP             ; ,,
            lda #SOUTH          ; Check South
            jsr CheckRoad       ; ,,
            rol TMP             ; ,,
            lda #EAST           ; Check East
            jsr CheckRoad       ; ,,
            rol TMP             ; ,,
            lda #NORTH          ; Check North
            jsr CheckRoad       ; ,,
            rol TMP             ; TMP now contains the bitfield of directions
            jsr Coor2Ptr        ; Convert pointer to screen address
            lda TMP             ; Get character
            ldx #0              ; Store in screen address
            sta (PTR,x)         ; ,,
next_cell:  jsr PlaceCol        ; Set the color of everything else
            inc COOR_Y          ; Continue iterating across all characters in   
            lda COOR_Y          ;   the maze
            cmp #20             ;   ,,
            bne loop            ;   ,,
            lda #$00            ;   ,,
            sta COOR_Y          ;   ,,
            inc COOR_X          ;   ,,
            lda COOR_X          ;   ,,
            cmp #22             ;   ,,
            bne loop            ;   ,,
            jmp ResetCoor       ; Reset coordinates
            
; Check Road
; Carry is clear if the specified adjacent cell is a road
CheckRoad:  pha
            jsr MoveCoor
            jsr CheckBound
            bcs restore
            jsr Coor2Ptr
            ldx #0
            lda (PTR,x)
            tay
            pla
            eor #%00000010
            jsr MoveCoor
            cpy #$10
            bcc is_road
            clc
            rts
restore:    pla
            eor #%00000010
            jsr MoveCoor
            clc
            rts
is_road:    sec
            rts  

; Nearby Structures
; Follow all combinations of between 1 and 4 roads, starting from
; the current coordinate. Return adjacent buildings in A.
; See AdjValues table for the meaning of the bits in the bitfield.
Nearby:     lda #0              ; Initialize pattern
            sta PATTERN         ; ,,
            sta NEARBY_ST       ; Initialize adjacent structures
            lda COOR_X          ; Set starting point in PREV_X and _Y
            sta PREV_X          ; ,,
            lda COOR_Y          ; ,,
            sta PREV_Y          ; ,,
next_patt:  jsr restore_c       ; Restore original coordinates
            lda #%11111111      ; Reset build mask to all buildings
            sta BUILD_MASK      ; ,,
            lda #4              ; Reset step counter to 4
            sta STEP_COUNT      ; ,,
            lda PATTERN         ; Set steps to the current pattern
            sta STEPS           ; ,,
next_step:  lda STEPS           ; Load the current step pattern
            and #%00000011      ; Mask next direction bits (0-3)
            sta CURR_DIR        ; Save current direction
            lda STEP_COUNT      ; If this is the first step in the pattern,
            cmp #4              ;   do not check the prior direction yet
            beq skip_ch         ;   ,,
            lda CURR_DIR        ; EOR current direction with prior direction
            eor LAST_DIR        ;   ,,
            cmp #$02            ; If $02, then pattern is backtracking
            beq patt_done       ;   indicating that it's done
skip_ch:    lda CURR_DIR        ; Current direction for the move
            sta LAST_DIR        ; Set last direction to current
            jsr MoveCoor        ; Move the coordinate based on direction
            jsr Coor2Ptr        ; Convert coordinate to screen location pointer
            ldx #0              ; Is this step on a road?
            lda (PTR,x)         ; ,,
            cmp #$10            ; ,,
            bcc sh_step         ; If not, allow only Wind Farms to be added for
            lda #BIT_WIND       ;   this pattern, as Wind Farms may be in range
            sta BUILD_MASK      ;   without being connected by road.
sh_step:    lsr STEPS           ; Shift to the next step, two bits right
            lsr STEPS           ; ,,
            dec STEP_COUNT      ; Decrement step count
            bne next_step       ; Do next step if not done with pattern
patt_done:  jsr Adjacents       ; Get buildings adjacent to the final step
            and BUILD_MASK      ; Mask allowable buildings for this pattern
            ora NEARBY_ST       ; Add this step's bitfield to cumulative byte
            sta NEARBY_ST       ; ,,
            inc PATTERN         ; Move to the next pattern
            bne next_patt       ; Until all patterns have been done
restore_c:  ldy PREV_X          ; Restore original coordinates
            sty COOR_X          ; ,,
            ldy PREV_Y          ; ,,
            sty COOR_Y          ; ,,
            jsr Coor2Ptr
            jsr Adjacents       ; Consider cardinally-adjacent structures to be
            ora NEARBY_ST       ; nearby
            sta NEARBY_ST       ; ,,
            lda NEARBY_ST       ; Put cumulative bitfield in A
            rts
                      
; Get Adjacent Buildings            
; A is a bitfield of cardinally-adjacent buildings
; See AdjValues for values
Adjacents:  lda #0
            sta ADJ_ST
            lda PTR             ; Back the pointer up to the upper left
            sec                 ;   corner of the surrounding space
            sbc #22             ;   ,,
            sta PTR             ;   ,,
            bcs search          ;   ,,
            dec PTR+1           ;   ,,
search:     ldy #3              ; The AdjPatt table adds numbers of cells
-loop:      lda CarPatt,y       ;   to PTR surrounding the pointer
            clc                 ;   ,,
            adc PTR             ;   ,,
            sta PTR             ;   ,,
            bcc lookup_val      ;   ,,
            inc PTR+1           ;   ,,
lookup_val: ldx #0              ; Get character at pointer position
            lda (PTR,x)         ; ,,
            cmp #$10            ; If this is a road, store it at bit 7
            bcs adj_range       ;   ,,
            lda #BIT_ROAD       ;   ,,
            bne add_adj         ;   ,,
adj_range:  cmp #CHR_ROAD       ; Range checking for characters
            bcc next_adj        ; ,,
            cmp #CHR_BURN       ; ,,
            bcs next_adj        ; ,,
            tax                 ; Look up the value in the table
            lda AdjValues-$23,x ; ,,
add_adj:    ora ADJ_ST          ; Add the value to the adjacent bitfield
            sta ADJ_ST          ; ,,
next_adj:   dey                 ; Iterate through entire pattern
            bpl loop            ; ,,
            jsr Coor2Ptr        ; Restore pointer
            lda ADJ_ST          ; Put adjacents bitfield into A
            rts
       
; Advance to Next Turn            
NextTurn:   lda UNDER           ; Replace previous character
            jsr Place           ; ,,
            inc YEAR            ; Increment year
            bne reset_ix        ; ,,
            inc YEAR+1          ; ,,
reset_ix:   lda #0              ; Reset build index
            sta BUILD_IX        ; ,,
            sta YR_EXPEND       ; Reset budgets
            sta YR_EXPEND+1     ; ,,
            sta YR_REVENUE      ; ,,
            sta YR_REVENUE+1    ; ,,
            sta BUS_CAP         ; ,,
            sta BUS_CAP+1       ; ,,
            jsr FiresOut        ; Put out last year's fires
            jsr Tornado         ; Is there a tornado this year?
            jsr Quake           ; Is there an earthquake this year????
            jsr Upkeep          ; Perform calculations and maintenance
            jsr Collect         ; Collect income
            jsr DrawStats       ; Draw the new stats bar
            jsr DrawBudget      ; Show the previous year's budget
            jsr DrawCursor      ; Put the cursor back
            ldy #HIGH_CONF      ; Calculate Business Confidence value
            lda BUS_CAP         ; If the annual Business value is greater
            cmp POP             ;   than the population, it means that the
            lda BUS_CAP+1       ;   population cannot support Businesses
            sbc POP+1           ;   ,,
            bcc next_r          ; Population is high enough
            ldy #LOW_CONF       ; Population is too low
next_r:     sty BUS_CONF        ; Set the Business attrition value
            jsr MusicPlay       ; Music may have stopped during a disaster
            jmp Main  
            
; Draw Info
; - If the structure has a maintenance cost, show that
; - If the structure is a Home or Business, show estimated revenue
; - If the strucutre has no maintenance cost, show structures nearby           
DrawInfo:   lda UNDER           ; If the structure under the cursor
            cmp #CHR_WIND       ;   has no maintenance cost,
            bcc struct          ;   then go to the structure display
            cmp #CHR_PARK+1     ;   ,,
            bcs struct          ;   ,,
            sec                 ; Otherwise, get the structure index
            sbc #CHR_WIND       ;   (relative to CHR_WIND)
            tay                 ;   and use that index to look up this
            lda MaintCosts,y    ;   structure's maintenance cost.
            pha                 ;   ,,
            ldx #2              ; Plot the cursor position
            ldy #19             ; ,,
            cmp #10             ;   (If the cost is more than 9, add another
            bcc u10             ;   column to the display to make room
            dey                 ;   ,,)
u10:        clc                 ; ,,
            jsr PLOT            ; ,,
            lda #$5e            ; Minus sign
            jsr CHROUT          ; ,,
            lda #";"            ; Show the coin
            jsr CHROUT          ; ,,
            pla                 ; Show the numeric maintenance cost
            tax                 ; ,,
            lda #0              ; ,,
            jmp PRTFIX          ; ,,
struct:     sec                 ; Set ISR to minimum, so that
            ror MISR_FL         ;   cursor flashes don't cause problems
            jsr Nearby          ;   during the Nearby routine
            sta TMP             ; Use TMP for structure list
            ldy #0              ; Y is the bit index
            ldx #7              ; X is the screen position
-loop:      lda BitChr,y        ; Get the character corresponding to the bit
            lsr TMP             ; Is this bit set?
            bcc skip_pos        ; ,,
            sta SCREEN+55,x     ; If the bit is set, add the character to the
            dex                 ;   display
skip_pos:   iny                 ; Move to the next bit
            cpy #7              ; ,,
            bne loop            ; ,,
            lda UNDER           ; Get character at pointer for additional info
            cmp #CHR_HOME
            beq show_val
            cmp #CHR_BUS
            bne struct_r            
show_val:   jsr Assess
            lda #$3b            ; Coin symbol
            sta SCREEN+63       ; ,,
            lda #$1f            ; Plus sign
            sta SCREEN+64       ; ,,
            lda VALUE  
            clc
            adc #$30            ; Convert value into a numeral
            sta SCREEN+65     
struct_r:   lsr MISR_FL         ; Turn off Min-ISR flag
            rts 
      
; Handle Earthquake Disaster            
Quake:      lda YEAR            ; Is it the Year of the Earthquake?
            cmp QUAKE_YR        ; ,,
            bne no_quake        ; ,,
            lda YEAR+1          ; ,,
            cmp QUAKE_YR+1      ; ,,
            beq yes_quake       ; ,,
no_quake:   rts
yes_quake:  jsr MusicStop
            lda #$ff            ; Turn on earthquake sound
            sta NOISE           ; ,,
            sta VOICEL          ; ,,
            sec                 ; Set Earthquake flag
            ror QUAKE_FL        ; ,,
            lda COOR_X          ; Save the current coordinates
            pha                 ; ,,
            lda COOR_Y          ; ,,
            pha                 ; ,,
            ldy QuakePower      ; How many map cells are going to be destroyed?
-loop:      jsr RandCoor        ; Randomize coordinate
            ldx #0              ; Lakes can't be destroyed by earthquakes
            lda (PTR,x)         ; ,,
            cmp #CHR_LAKE       ; ,,
            beq next_dmg        ; ,,
            lda #CHR_BURN       ; Place damage on screen
            jsr Place           ; ,,
            lda #50             ; Delay during the shaking
            jsr Delay           ; ,,
next_dmg:   dey                 ; Go back for more destruction!
            bne loop            ; ,,
            sei                 ; ----QUAKE IS OVER----
            lsr QUAKE_FL        ; Reset flag
            lda ORIG_HORIZ      ; Restore the original horiz position
            sta HORIZ           ; ,,
            cli
            lda #0              ; Turn off the earthquake sound
            sta NOISE           ; ,,
            sta VOICEL          ; ,,
            jsr NextQuake       ; Set the date of the next disaster  
            jsr MusicInit       ; Re-initialize the music after an earthquake
            jmp ResetCoor       ; Reset the coordinates  

; Handle Tornado Disaster            
Tornado:    lda TornFreq        ; Get frequency of tornado in power of two
            jsr PRand           ; If this random check is 0, there's a
            beq yes_tor         ;   tornado
            rts                 ; Otherwise, nothing happens
yes_tor:    jsr MusicStop       ; ,,
            lda COOR_X          ; Save the current coordinates for later
            pha                 ; ,,
            lda COOR_Y          ; ,,
            pha                 ; ,,
            lda #14             ; Change screen to dark and foreboding
            sta SCRCOL          ; ,,
            jsr FiresOut        ; Clear most of the remaining fires
            sec                 ; Turn on Min-ISR flag to disable fire animation
            ror MISR_FL         ; ,,
            ldy #7              ; Temporarily replace the burning icon with
-loop:      lda TornIcon,y      ;   the tornado icon
            sta Burning,y       ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            jsr RandCoor        ; Starting point of tornado
            ldy #0              ; Increase volume
-loop:      sty VOLUME          ; ,,
            lda #$f0            ; ,,
            sta NOISE           ; ,,
            lda #10             ; ,,
            jsr Delay           ; ,,
            iny                 ; ,,         
            cpy #10             ; ,,
            bne loop            ; ,,
            ldy TornPath        ; ---- START PATH ---- Maximum path length
-loop:      ldx #0              ; If there's a Lake at the new location, the
            lda (PTR,x)         ;   tornado dissipates over the lake
            cmp #CHR_LAKE       ;   ,,
            beq tornado_r       ;   ,,
            lda #CHR_BURN       ; Place the damage on the screen
            jsr Place           ; ,,
            ldx #7              ; Flash some lightning
flashes:    lda VICCR4          ;     (Wait for raster to get to 0 before
            bne flashes         ;       changing screen color)
            lda TornScr,x       ;   Lightning
            sta SCRCOL          ;   ,,
            lda TornThun,x      ;   Thunder
            sta NOISE           ;   ,,
            lda TornFlash,x     ;   Delay
            jsr Delay           ;   ,,
            dex                 ; Do next flash
            bpl flashes         ; ,,
tor_dir:    jsr Rand3           ; Pick a random direction
            cpy TornPath        ; If this is the first iteration, don't check
            beq skip_back       ;   for backtracking
            tax                 ; Keep direction safe from EOR
            eor TMP             ; If EOR with prior direction is %00000010, then
            cmp #%00000010      ;   the path is backtracking, so go back and
            beq tor_dir         ;   choose another direction
            txa                 ; Get back the chosen direction, which is good
skip_back:  sta TMP             ; Store the new direction for backtrack check
            jsr MoveCoor        ; Move in that direction
            jsr CheckBound      ; Check boundary, and end tornado if it leaves
            bcs tornado_r       ;   the city
            jsr Coor2Ptr        ; Convert coordinate to pointer
            dey                 ; Iterate through TornPath damage area
            bne loop            ; ,,
tornado_r:  ldy #$0a            ; Fade out the volume
-loop:      sty VOLUME          ; ,,
            cpy #8              ;   Turn the tornado icon back into the
            bcs dec_vol         ;     burning icon
            lda BurnOrig,y      ;     ,,
            sta Burning,y       ;     ,,
dec_vol:    lda #5              ; ,,
            jsr Delay           ; ,,
            dey                 ; Continue fading volume
            bpl loop            ; ,,
            lda #0              ; Cut off the tornado noise
            sta NOISE           ; ,,
            lsr MISR_FL         ; Turn Min-ISR flag off for animations
            lda #254            ; Restore proper screen color
            sta SCRCOL          ; ,,
            jmp ResetCoor       ; Reset coordinates
                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UPKEEP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Upkeep
; Iterate through all map cells
;   - For Homes, calculate tax base and add to Treasury
;   - For unprotected Homes, there's a chance of fire
;   - For Businesses, calculate tax base and add to Treasury
;   - For unprotected Businesses, there's a chance of fire
;   - For Wind Farms, Firehouses, Clinics, and Schools,
;     subtract maintenance from Treasury
;   - If maintenance cannot be paid, there's a chance of fire
;   - For unoccupied Homes and Businesses, calculate chance of
;     conversion of occupied
Upkeep:     jsr ClrStatus       ; Clear status bar
            lda #0              ; Reset sold Home and Business counters
            sta SOLD_HOMES      ;   which are used to limit new
            sta SOLD_BUSES      ;   occupancy
            sta HOME_COUNT      ; Reset Home count
            sta BUS_COUNT       ; Reset Business count
            sta SMART_COUNT     ; Reset smart Home count
            sta POP             ; Reset population for recalculation
            sta POP+1           ; ,,
            lda COOR_X          ; Save cursor coordinates
            pha                 ; ,,
            lda COOR_Y          ; ,,
            pha                 ; ,,
            lda #$00            ; Start at the top left corner of the board
            sta COOR_Y          ; ,,
            sta PROGRESS        ; Reset Progress Bar
rand_x:     jsr Rand31          ; Get random location 0-31
            cmp #20             ; If it's not a good X coordinate range,
            bcs rand_x          ;   get another one
            sta COOR_X          ;   ,,
-loop:      jsr Coor2Ptr        ; Get the character at the pointer
            ldx #$00            ; ,,
            lda (PTR,x)         ; ,,          
            cmp #CHR_UHOME      ; Handle UNOCCUPIED HOME
            bne ch_ubus         ; ,,
            jsr SellHome
            jmp next_map
ch_ubus:    cmp #CHR_UBUS       ; Handle UNOCCUPIED_BUSINESS
            bne ch_home
            jsr SellBus
            jmp next_map
ch_home:    cmp #CHR_HOME       ; Handle HOME
            bne ch_bus          ; ,,
            jsr CompHome        ; Compute Home
            jmp next_map
ch_bus:     cmp #CHR_BUS        ; Handle BUSINESS
            bne ch_maint
            jsr CompBus
            jmp next_map
ch_maint:   cmp #CHR_WIND       ; Pay maintenance for maintainable
            bcc next_map        ;   structures (Wind Farm, School,
            cmp #CHR_PARK+1     ;   Firehouse, Clinic, Park).
            bcs next_map        ;   ,,
            jsr Maint           ;   ,,
next_map:   inc COOR_Y
            lda COOR_Y
            cmp #20
            bne loop
            lda #0
            sta COOR_Y
            inc COOR_X
            inc PROGRESS        ; Advance Progress Bar and draw it
            ldy PROGRESS        ; ,,
            lda #CHR_PROG       ; ,, Progress bar character
            sta SCREEN+43,y     ; ,,
            lda #4              ; ,, (Slight delay for progress)
            jsr Delay           ; ,,
            cpy #22             ; If 22 columns have been done,
            beq upkeep_r        ;   upkeep is done
            lda COOR_X          ; Check X for bounds
            cmp #22             ; If X reached the right edge, loop back
            bne loop            ;   to 0
            lda #0              ;   ,,
            sta COOR_X          ;   ,,
            beq loop
upkeep_r:   jsr ClrStatus       ; Clear status bar
            jmp ResetCoor       ; Reset coordinates

; Maintenance
Maint:      sec                 ; Convert character to index
            sbc #CHR_WIND       ; ,,
            tay                 ; ,,
            lda MaintCosts,y    ; Get maintenance cost of structure
            jsr Spend           ; Take cost from Treasury
            bcc maint_r         ; If there's not enough, there's a 1 in 8
            jsr Rand7           ;   chance that the stucture will burn
            bne maint_r         ;   down
            lda #CHR_BURN       ;   ,,
            jsr Place           ;   ,,
maint_r:    rts            
            

; Sell Home
; For an unoccupied Home at the coordinate, see if it qualifies for
; occupancy
; Rule - If the unoccupied Home has a nearby Wind Farm, it can sell
SellHome:   lda SOLD_HOMES      ; If a Home was sold this turn, return
            bne sellh_r         ; ,,
            jsr Nearby          ; A Home must have a nearby Wind Farm
            and #BIT_WIND       ; ,,
            beq sellh_r         ; ,,
            lda #CHR_HOME       ; Place the Home
            jsr Place           ; ,,
            inc SOLD_HOMES      ; Increment sell count
            jmp populate        ; Add population to Home & add Home count
sellh_r:    rts

; Sell Business
; For an unoccupied business at the coordinate, see if it qualifies for
; occupancy
; Rules - An unoccupied Business cannot sell without a nearby Wind Farm
;       - If the unoccupied Business is adjacent to a Business, it can sell
;       - If the unoccupied Business has a nearby Home, it can sell
SellBus:    lda SOLD_BUSES      ; If a business was sold this turn, return
            bne sellb_r         ; ,,
            lda BUS_CONF        ; If the business climate is unfavorable,
            cmp #LOW_CONF       ;   the Business will not sell
            beq sellb_r         ;   ,,
            jsr Nearby          ; A business must have a nearby Wind Farm
            lda #BIT_WIND       ;   to sell
            bit NEARBY_ST       ;   ,,
            beq sellb_r         ;   ,,
            lda #BIT_ROAD       ; A business must have an adjacent Road
            bit ADJ_ST          ;   to sell
            beq sellb_r         ;   ,,
            lda #BIT_HOME       ; If these hurdles are cleared, it will sell
            bit NEARBY_ST       ;   by either (1) being nearby an occupied
            bne sold_bus        ;   Home, or
            jsr Adjacents       ;   (2) being adjacent to another occupied
            and #BIT_BUS        ;   Business
            beq sellb_r         ; Otherwise, it does not sell yet
sold_bus:   lda #CHR_BUS        ; Place sold Business on screen
            jsr Place           ; ,,
            inc SOLD_BUSES      ; Increment sell count
            inc BUS_COUNT       ; Increment Business count
sellb_r:    rts

; Compute Home
; - Determine whether the family leaves or stays
; - Assess the Home value
; - Add the tax revenue from the Home
CompHome:   jsr Nearby          ; Get nearby structures
            lda #BIT_WIND       ; Is there a Wind Farm nearby?
            bit NEARBY_ST       ; ,,
            bne ch_hfire        ; If so, go to next check
            jsr Rand3           ; If not, there's a 1 in 4 chance that
            bne ch_hfire        ;   occupants move out
            lda #CHR_UHOME      ; Otherwise, they move out
            jmp Place           ; ,,
ch_hfire:   jsr FireRisk        ; Handle fire risk
            bcs comph_r         ; ,,
ch_emp:     jsr Sad             ; If occupents are not satisfied, there's a
            bcs ch_hclinic      ;   1 in 8 chance they will move out
            jsr Rand7           ;   ,,
            bne ch_hclinic      ;   ,,
            lda #CHR_UHOME      ;   ,,
            jmp Place           ;   ,,             
ch_hclinic: lda #BIT_CLINIC     ; Is there a Clinic nearby?
            bit NEARBY_ST       ; ,,
            beq ch_hschool      ; ,,
            lda #1              ; A nearby Clinic adds to 1 a Home's
            jsr AddPop          ;   population
ch_hschool: lda #BIT_SCHOOL     ; Is there a School nearby?
            bit NEARBY_ST       ; ,,
            beq hrevenue        ; ,,           
            inc SMART_COUNT     ; If so, count this as a smart Home
hrevenue:   lda #CHR_HOME       ; Assess Home property value
            jsr Assess          ; ,,
            jsr Revenue         ; Add property value to Treasury
populate:   jsr Rand3           ; Add people to each Home (3-6)
            clc                 ; ,,
            adc #3              ; ,,
            jsr AddPop          ; ,,
            inc HOME_COUNT      ; Count Homes
comph_r:    rts

; Compute Business 
; - Determine whether the Business leaves or stays
; - Assess the Business value
; - Add the tax revenue from the Business
CompBus:    lda BUS_CONF        ; Do population-based confidence test
            jsr PRand           ; Every so often, a Business just
            bne bus_near        ;   closes up shop and leaves. This is
            lda #CHR_UBUS       ;   just the nature of Businesses.
            jmp Place           ;   ,,
bus_near:   jsr Nearby          ; Get nearby structures
            lda #BIT_WIND       ; Is there a Wind Farm nearby?
            bit NEARBY_ST       ; ,,
            bne ch_bfire        ; If so, go to next check
            jsr Rand3           ; If not, there's a 1 in 4 chance that
            bne ch_bfire        ;   the Business moves out
            lda #CHR_UBUS       ;   ,,
            jmp Place           ;   ,,
ch_bfire:   jsr FireRisk        ; Handle fire risk
            bcs compbus_r       ; ,,
ch_broad:   lda #BIT_ROAD       ; Does the Business still have an
            bit ADJ_ST          ;   adjacent road?
            bne brevenue        ;   ,,
            lda #CHR_UBUS       ; If not, the Business always moves out
            jmp Place           ;   ,,
brevenue:   lda #CHR_BUS        ; Assess Business property value
            jsr Assess          ; ,,
            jsr Revenue         ; Add property value to Treasury
            inc BUS_COUNT       ; Count Businesses
            lda VALUE           ; Add Business's value to the Business
            clc                 ;   Activity amount
            adc BUS_CAP         ;   ,,
            sta BUS_CAP         ;   ,,
            bcc compbus_r       ;   ,,
            inc BUS_CAP+1       ;   ,,
compbus_r:  rts  

; Handle Fire Risk
; For Homes and Businesses, the same
; Assumes Coordinate and NEARBY_ST are set
; Carry is set if the structure has been destroyed by fire
FireRisk:   lda #BIT_FIRE
            bit NEARBY_ST
            bne fire_r
            jsr Rand15
            bne fire_r
            lda #CHR_BURN
            jsr Place
            sec
            rts
fire_r:     clc
            rts   
            
; Sad
; Carry is set if Satisfaction is 65% or higher
; Carry is clear if folks are sad
Sad:        lda HAPPY           ; If employment is 70% or more   
            cmp #"7"            ;   residents are not sad
            bcs sad_r           ; Carry is set if >= 70%, so return
            cmp #"6"            ; Carry is clear if <= 50%, so return
            bcc sad_r           ; ,,          
            lda SMART           ; Carry is set if >= 65%, and
            cmp #"5"            ;   clear if <= 64%
sad_r:      rts
             
; Assess Home or Business Value
; A is CHR_HOME or CHR_BUS
; Store result in VALUE
; This assumes that Nearby has already been called so that NEARBY_ST and ADJ_ST
; are set correctly.
Assess:     ldy #0              ; Initialize value
            sty VALUE           ; ,,
            ldy #<BusVals       ; Set assessment table
            sty TMP_PTR         ; ,,
            ldy #>BusVals       ; ,,
            sty TMP_PTR+1       ; ,,
            cmp #CHR_HOME       ; ,,
            bne assess_st       ; ,,
            ldy #<HomeVals      ; Override assessment table
            sty TMP_PTR         ; ,,
            ldy #>HomeVals      ; ,,
            sty TMP_PTR+1       ; ,,
assess_st:  lda NEARBY_ST       ; Use nearby structures as the current
            sta CURR_ST         ;   assessment type
            jsr AssessLkup      ; Assess nearby structures
            lda ADJ_ST          ; Use adjacent structures as the next
            sta CURR_ST         ;   assessment type
            lda #14             ; Advance the table pointer by 14 bytes so that
            clc                 ;   it now references the Adjacent assessment
            adc TMP_PTR         ;   table
            sta TMP_PTR         ;   ,,
            bcc AssessLkup      ;   ,,
            inc TMP_PTR+1       ;   ,,
AssessLkup: ldy #6              ; Look up structures in the
-loop:      lda POWERSOF2,y     ;   value table and add them to
            bit CURR_ST         ;   the VALUE storage
            beq assess_nx       ;   ,,
            lda (TMP_PTR),y     ;   ,,
            clc                 ;   ,,
            adc VALUE           ;   ,,
            sta VALUE           ;   ,,
assess_nx:  dey                 ;   ,,
            bpl loop            ;   ,,
            lda VALUE           ; Prevent value from going negative
            bpl assess_r        ; ,,
            lda #0              ; ,,
            sta VALUE           ; ,,
assess_r:   rts

; Fires Out
; Converts MOST fires (75%) to spaces            
FiresOut:   lda COOR_X
            pha
            lda COOR_Y
            pha
            lda #0
            sta COOR_X
            sta COOR_Y
-loop:      jsr Coor2Ptr
            ldx #0
            lda (PTR,x)
            cmp #CHR_BURN
            bne no_fire
            jsr Rand3
            beq no_fire
            lda #$20
            sta (PTR,x)
no_fire:    inc COOR_X
            lda COOR_X
            cmp #22
            bne loop
            inc COOR_Y
            lda COOR_Y
            cmp #20
            beq f_out_r
            lda #0
            sta COOR_X
            beq loop
f_out_r:    jmp ResetCoor            
            
; Add Population
; In A
AddPop:     clc
            adc POP
            sta POP
            bcc addpop_r
            inc POP
addpop_r:   rts       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Delay A Jiffies
; Actually, between (A - 1) and A jiffies, but that shouldn't matter for the
; purposes of this routine
Delay:      clc
            adc TIME
-loop:      cmp TIME
            bne loop
            rts 
            
; Clear Status Bar            
ClrStatus:  ldy #21
            lda #$20
-loop:      sta SCREEN+44,y
            dey
            bpl loop
            rts            
                        
; Pseudo Random
Rand3:      lda #%01000000      ; 2-bit
            .byte $3c
Rand7:      lda #%00100000      ; 3-bit
            .byte $3c
Rand15:     lda #%00010000      ; 4-bit
            .byte $3c
Rand31:     lda #%00001000      ; 5-bit
            .byte $3c
Rand255:    lda #%00001000      ; 8-bit            
PRand:      sta RNDNUM
-loop:      lsr P_RAND
            ror P_RAND+1
            bcc shift_rnd
            lda P_RAND
            eor #$aa
            sta P_RAND
            lda P_RAND+1
            eor #$2b
            sta P_RAND+1
shift_rnd:  rol RNDNUM
            bcc loop
            lda RNDNUM
            rts            
            
; Read the Joystick
; Return the direction in X
; 0=North, 1=East, 2=South, 3=West, 4=Fire, 5=Save, 6=Load,
; $ff=None (for testing BMI/BPL)
Joystick:   lda VIA1PA          ; Read VIA1 port
            and #$3c            ; Keep track of bits 2,3,4,5
            sta TMP
            lda VIA2PB          ; Combine with read of bit 7
            and #$80            ;   from VIA2-B
            ora TMP
            eor #$bc            ; Flip each joystick bit in the
                                ;   combined read byte, so that
                                ;   on = 1
            sta TMP             ; Store temporary direction
            ldx #5              ; Check five directions (incl. fire)
-loop:      lda JoyTable,x      ; Is the joystick pointed in the
            bit TMP             ;   direction indexed by X?
            bne found_dir       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
found_dir:  lda KEYDOWN         ; Key current keypress
            cmp #41             ; "S" for Save
            bne ch_L            ; ,,
            ldx #6              ; ,, (will be 5 after DEX below)
ch_L:       cmp #21             ; "L" for Load
            bne control_r       ; ,,
            ldx #7              ; ,, (will be 6 after DEX below)
control_r:  dex                 ; dex to maybe set zero flag
            rts
            
; Move Coordinate
; Based on compass point in A
MoveCoor:   cmp #NORTH          ; Check each compass point and, if selected,
            bne ch_east         ;   move the appropriate coordinate
            dec COOR_Y          ;   ,,
ch_east:    cmp #EAST           ;   ,,
            bne ch_south        ;   ,,
            inc COOR_X          ;   ,,
ch_south:   cmp #SOUTH          ;   ,,
            bne ch_west         ;   ,,
            inc COOR_Y          ;   ,,
ch_west:    cmp #WEST           ;   ,,
            bne move_r          ;   ,,
            dec COOR_X          ;   ,,
move_r:     rts 

; Coordinates to Pointer
; Set PTR and PTR+1 with screen memory address referenced by the
; x and y coordinates.
Coor2Ptr:   lda #<BOARD         ; Start at the upper left corner of the screen
            sta PTR             ; ,,
            lda #>BOARD         ; ,,
            sta PTR+1           ; ,,
            lda COOR_Y          ; Get y coordinate
            beq no_y            ; If there's no offset, skip multiplication
            tax                 ; X is the row index
-loop:      lda #22             ; Drop to the next line...
            clc                 ; ,,
            adc PTR             ; ,,
            sta PTR             ; ,,
            bcc next_y          ; ,,
            inc PTR+1           ; ,,
next_y:     dex                 ; ...Y times
            bne loop            ; ,,
no_y:       lda COOR_X          ; Get x coordinate
            clc                 ; ,,
            adc PTR             ; Add it to the pointer
            sta PTR             ; ,,
            bcc t2p_r           ; ,,
            inc PTR+1           ; ,,
t2p_r       rts 

; Reset Coordinates
; Coordinates are often stored on the stack. This resets them.
ResetCoor:  pla
            sta COOR_Y
            pla
            sta COOR_X
            jsr Coor2Ptr
            rts  

; Check Boundary
; of COOR_X and COOR_Y
; Carry clear means in bounds, carry set means out of bounds
CheckBound: lda COOR_X          ; Check X coordinate for <0
            bmi out             ; ,,
            cmp #22             ; Check X coordinate for >21
            bcs out             ; ,,
            lda COOR_Y          ; Check Y coordinate for <0
            bmi out             ; ,,
            cmp #20             ; Check Y coordinate for >20
            bcs out             ; ,,
            rts                 ; Return with carry clear (in)
out:        sec                 ; Return with carry set (out)
            rts                 ; ,,
            
; Randomize Coordinate            
RandCoor:   jsr Rand31          ; Get random location 0-31
            cmp #20             ; If it's not a good X coordinate range,
            bcs RandCoor        ;   get another one
            sta COOR_X          ; Else, keep it as X
-rnd_y:     jsr Rand31          ; Do the same with Y
            cmp #18             ; ,,
            bcs rnd_y           ; ,,
            sta COOR_Y          ; ,,
            inc COOR_X          ; Keeping the Coords off the borders, mostly
            inc COOR_Y          ;   for aesthetic reasons
            jmp Coor2Ptr
            
; Place Character
; In A, at PTR location
; Then set color from Colors table
Place:      ldx #0              ; Place the item at the screen location
            sta (PTR,x)         ; ,,
PlaceCol:   cmp #$10            ; Handle color lookup, with special case
            bcs lookup_col      ;   for roads
            lda #COL_ROAD       ;   ,,
            jmp SetColor        ;   ,,
lookup_col: tax                 ; Look up the color in the table by screen
            lda Colors-$23,x    ;   code
            ; Fall through to SetColor

; Set Color
; In A, at color location corresponding to PTR
SetColor:   pha
            lda PTR
            sta COL_PTR
            lda PTR+1
            and #$03
            ora #>COLOR
            sta COL_PTR+1
            ldx #0
            pla
            sta (COL_PTR,x)
            rts
            
; Get Happiness
; Basically an employment percentage from 10% to 90%
; 90% if the number of employers exceeds the number of workers
; 0% if there are no Businesses
GetHappy:   lda #"9"            ; Default to "9" if there are more Homes than
            sta HAPPY           ;   Businesses
            ldy HOME_COUNT      ; Number of Homes
            cpy BUS_COUNT       ; ,,
            bcc happy_r         ; Same or more employers than workers so 90%
            beq happy_r         ; ,,
            lda #"1"            ; If there are no businesses to divide, set
            sta HAPPY           ;   happiness to 10%
            ldy BUS_COUNT       ;   ,,
            beq happy_r         ;   ,,
            sed
            ldy HOME_COUNT      ; Home count is the denominator
            jsr Hex2Deci        ; Convert to decimal
            sty DENOM           ;   and store
            ldy BUS_COUNT       ; Business count is numerator
            jsr Hex2Deci        ;   Convert to decimal
            sty NUMER           ;   And store
            jsr Divide          ; Multiply NUMER by 10
            cld
            clc                 ; Add quotient to HAPPY
            adc HAPPY           ; ,,
            sta HAPPY           ; ,,
            dec HAPPY           ; Decrement to compensate for starting "1"
happy_r:    rts
            
; Get Smartness
; Percentage of the number of Homes with a School nearby, to the
; tens place.
; 0% if there are no Schools or Homes
GetSmart:   lda #"0"            ; Starting default
            sta SMART           ; ,,
            ldy HOME_COUNT      ; Stay at "0" if either count is 0
            beq smart_r         ; ,,
            lda SMART_COUNT     ; ,,
            beq smart_r         ; ,,
            cpy SMART_COUNT     ; If the counts are the same, force
            bne do_div          ;   the count to "9"
            lda #"9"            ;   ,,
            sta SMART           ;   ,,
            bne smart_r         ;   ,,
do_div:     sed
            jsr Hex2Deci        ; Convert to decimal
            sty DENOM           ;   and store
            ldy SMART_COUNT     ; Number of smart Homes
            jsr Hex2Deci        ; Convert to decimal
            sty NUMER           ;   and store
            jsr Divide          ; Perform division
            cld
            clc                 ; Add the quotient to the numeral
            adc SMART           ;   character
            sta SMART           ;   ,,
smart_r:    rts

; Divide Numerator x 10 / Denominator
; Assumes decimal mode is set  
; A is the quotient to tens place
Divide:     lda #0              ; Set the high byte of the numerator
            sta NUMER+1         ; ,,
            ldx #4              ; Shift everything left by 4 nybbles
-loop:      asl NUMER           ; ,,
            rol NUMER+1         ; ,,
            dex                 ; ,,
            bne loop            ; ,,
            ldx #0              ; X is now the quotient
-loop:      inx                 ; Subtraction count
            lda NUMER           ; Subtract denominator from numerator
            sec                 ; ,,
            sbc DENOM           ; ,,
            sta NUMER           ; ,,
            bcs sub_div         ; ,,
            dec NUMER+1         ; ,,
sub_div:    lda NUMER+1         ; If the numerator goes below 0, division is
            bpl loop            ;   done
            dex                 ; Decrement to compensate for starting inx
            txa                 ; Returning 
            rts

; Hex to Decimal
; For decimal mode conversion               
Hex2Deci:   lda #$00
-loop:      clc
            adc #$01
            dey
            bne loop
            tay
            rts            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up hardware
Setup:      lda #254            ; Set background color
            sta SCRCOL          ; ,,
            lda #0              ; Turn off sound registers
            sta NOISE           ; ,, 
            sta VOICEL          ; ,,
            sta VOICEM          ; ,,
            sta VOLUME          ; ,,
            sta ACTION_FL       ; Clear Action flag
            sta MUSIC_FL        ; Clear Music Play flag
            lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$fe            ; Prepare for new waveform
            sta VOICEH          ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda TIME            ; Seed random number generator
            ora #$01            ; ,,
            sta P_RAND          ; ,,
            lda VIATIME         ; ,,
            ora #$80            ; ,,
            sta P_RAND+1        ; ,,
            lda #<Welcome       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Welcome       ; ,,
            sta NMINV+1         ; ,,
InstallISR: sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
LONE_RTS:   rts                 ; This is a CHROUT vector destination. Leave be!

; Initialize Game            
InitGame:   lda #<Header        ; Show Board Header
            ldy #>Header        ; ,,
            jsr PRTSTR          ; ,,
            lda #0              ; Reset build index to Cancel
            sta BUILD_IX        ; ,,
            sta POP             ; Initialize population
            sta POP+1           ; ,,
            sta HOME_COUNT     ; Initialize counts
            sta BUS_COUNT       ; ,,
            sta SMART_COUNT     ; ,,
            sta TIME            ; Initialize timer
            sta BUS_CONF        ; Starting Business Attrition value
            sta MISR_FL         ; Turn off Min-ISR flag
            lda StartTreas      ; Set initial treasury amount
            sta TREASURY        ; ,,
            lda StartTreas+1    ; ,,
            sta TREASURY+1      ; ,,
            lda StartYear       ; Initialize year
            sta YEAR            ; ,,
            lda StartYear+1     ; ,,
            sta YEAR+1          ; ,,
            ldy LakeCount       ; Add a number of Lakes to the board
-loop:      jsr RandCoor        ; ,,
            lda #CHR_LAKE       ; ,,
            jsr Place           ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            lda #10             ; Set initial coordinates
            sta COOR_X          ; ,,
            sta COOR_Y          ; ,,
            jsr DrawStats       ; Show stats in header
            ldy #21             ; Set status bar color to blue
            lda #6              ; ,,
-loop:      sta COLOR+44,y      ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            jsr DrawCursor      ; Show cursor
            ; Fall through to NextQuake
            
; Next Quake Year
; The next earthquake is predestined 
NextQuake:  lda YEAR+1          ; Set the year high byte
            sta QUAKE_YR+1      ; ,,
            lda QuakeMarg       ; Get a random number from the quake's
            jsr PRand           ;   margin (a power of two)
            clc                 ; Add this margin to the known frequency
            adc QuakeFreq       ; ,,
            adc YEAR            ; ,,
            sta QUAKE_YR        ; ,,
            bcc nxquake_r
            inc QUAKE_YR+1
nxquake_r:  rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  
; MUSIC PLAYER SERVICE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Service Music
; This is a modal shift register player. It's like my other shift register
; players, but it uses a Mode table for note lookup. Since this game doesn't
; have simultaneous sound effects, I'm also using volume to create a pluck
; envelope.
MusicServ:  bit MUSIC_FL        ; Skip if play flag is off
            bpl music_r         ;   ,,
            dec MUSIC_TIMER     ; Fetch new note when timer hits 0
            bmi musicsh
            lda VOLUME          ; Reduce volume, but not past 0
            beq music_r         ; ,,
            dec VOLUME          ; ,,
            rts
musicsh:    asl MUSIC_REG       ; Shift 32-bit register left
            rol MUSIC_REG+1     ; ,,
            rol MUSIC_REG+2     ; ,,
            rol MUSIC_REG+3     ; ,,
            lda #0              ; Put Carry into A bit 0
            rol                 ; ,,
            ora MUSIC_REG       ; And put that back at the beginning
            sta MUSIC_REG       ; ,,
            dec MUSIC_MOVER     ; When this counter hits 0, alter the music
            bne FetchNote       ;   by flipping bit 0 of each byte in the
            ldy #3              ;   shift register
-loop:      lda MUSIC_REG,y     ;   ,,
            eor #$01            ;   ,,
            sta MUSIC_REG,y     ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            lda MUSIC_REG+2
            lsr
            ora #%00000001
            sta MUSIC_MOVER     ; 
FetchNote:  lda #10             ; Reset the timer
            sta MUSIC_TIMER     ; ,,
            bit MUSIC_REG       ; A high note played when bit 7 of byte 0 is
            bpl play_high       ;   cleear
            lda #0              ; Otherwise, silence the high voice 
            sta VOICEH          ; ,,
            beq play_low        ; 
play_high:  lda MUSIC_REG+1     ; Get the mid note
            and #%00001110      ; Mask the low three bits and shift, then
            lsr                 ;   transfer the bits to
            tay                 ;   Y to be the mode degree index
            lda Mode,y          ; Get the modal note
            sta VOICEH          ; Put it in the sound register
play_low:   lda MUSIC_REG+1     ; Play the middle register, same byte as the
            and #%00000111      ;   hight, but one bit behind
            tay                 ; Y is the mode degree index
            lda Mode,y          ; ,,
            sta VOICEM          ; Set mid voice
set_vol:    lda MUSIC_REG+3
            and #$0f
            sta VOLUME
music_r:    rts

; Stop Music
MusicStop:  lsr MUSIC_FL
            lda #0
            sta VOICEM
            sta VOICEH
            rts

; Play Music            
MusicPlay:  sec
            ror MUSIC_FL
            rts

; Initialize Music
MusicInit:  ldy #3
-loop:      lda Theme,y
            sta MUSIC_REG,y
            dey
            bpl loop
            sec
            ror MUSIC_FL
            lda #0
            sta VOLUME
            sta MUSIC_MOVER
            jmp FetchNote
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TAPE SAVE/LOAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TapeSave:   lda UNDER           ; Clear the Pointer out of the way
            jsr Place           ; ,,
            jsr TapeSetup       ; Set up tape
            ldx #1              ; Device number
            ldy #$ff            ; Command (none)
            lda #42             ; Logical file number
            jsr $ffba           ; SETLFS
            ldx #<SCREEN        ; Low byte start
            stx $2b             ; ,,
            ldy #>SCREEN        ; High byte start
            sty $2c             ; ,,
            lda #$2b            ; Set tab
            iny                 ; 512 bytes
            iny                 ; ,,
            jsr $ffd8           ; SAVE
TapeClnup:  inc SCRCOL          ; Put the border color back
            lda #42             ; Close file 42
            jsr $ffc3           ; ,,
            lda #$7a            ; Restore original CHROUT
            sta $0326           ; ,,
            lda #$f2            ; ,,
            sta $0327           ; ,,
save_r:     jsr MusicPlay       ; Restart music
            lsr ACTION_FL       ; Turn off Action flag
            jmp cancel          ; Return from tape operation cancels Action         

; Tape Load
TapeLoad:   jsr TapeSetup       ; Set up tape
            lda #$12            ; Make text reverse
            jsr CHROUT          ; ,,
            ldx #1              ; Device number
            ldy #3              ; Command
            lda #42             ; Logical file number
            jsr $ffba           ; SETLFS
            lda #$00            ; Set up LOAD address
            ldx #$00            ; ,,
            ldy #>SCREEN        ; ,,
            jsr $ffd5           ; LOAD
            ldx #0              ; Preserve whatever came from the load
            lda (PTR,x)         ;   under the Cursor
            sta UNDER           ;   ,,
            jsr Roads           ; Re-colorize the world
            jmp TapeClnup       ; Clean up the operation

; Tape Setup
; Sets IRQ to its normal place during tape operation    
; Sets CHROUT to a lone RTS to suppress the normal prompts   
; Sets volume to 0
; Changes the border color to indicate tape operation        
TapeSetup:  jsr MusicStop       ; Stop music during tape
            dec SCRCOL          ; Change screen border
            lda #<LONE_RTS      ; Redirect CHROUT to a lone RTS
            sta $0326           ;   to suppress prompts
            lda #>LONE_RTS      ;   ,,
            sta $0327           ;   ,,
            lda #0              ; Zero-length filename
            jmp $ffbd           ; ,,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Board Header
Header:     .asc $93,$1f,"PQRSTU!!!!!!!!VWXYZ[",$5c,"]"
            .asc ":!!!!!;!!!!!<!!>!=!!!!",$00

; Budget Header
BudgetR     .asc $13,$11,$11,$5f,";",$00
BudgetE     .asc $21,$5e,";",$00

; Direction tables
JoyTable:   .byte $00,$04,$80,$08,$10,$20          ; Corresponding direction bit
DirTable:   .byte $01,$02,$04,$08                  ; Index to bit value

; Wind Farm and burning animation, and Tornado
WFAnim1:    .byte $10,$44,$38,$10,$10
WFAnim2:    .byte $00,$10,$10,$38,$44
BurnAnim1:  .byte $ee,$6e,$3c,$18,$10
BurnAnim2:  .byte $dc,$f8,$70,$30,$10
TornIcon:   .byte $00,$fe,$78,$1c,$38,$18,$92,$54
BurnOrig:   .byte $00,$10,$18,$3c,$6e,$ee,$cc,$78

; Colors of things
Colors:     .byte COL_ROAD,COL_UNOCC,COL_UNOCC,COL_WIND
            .byte COL_SCHOOL,COL_FIRE,COL_CLINIC,COL_PARK
            .byte 0,COL_OCC,COL_OCC,COL_LAKE,COL_BURN   
     
; Adjacent structure bit numbers
; Wind Farm = Bit 0
; School    = Bit 1
; Firehouse = Bit 2
; Clinic    = Bit 3
; Park      = Bit 4
; Lake      = Bit 4 (same land value as Park)
; Home      = Bit 5
; Business  = Bit 6
; Roads     = Bit 7 (not on the list because they're handled separately)
AdjValues:  .byte 0,0,0,BIT_WIND,BIT_SCHOOL,BIT_FIRE,BIT_CLINIC,BIT_PARK
            .byte 0,BIT_HOME,BIT_BUS,BIT_PARK,0     

; Structure character at bit number
; See bit numbers in AdjValues
BitChr:     .byte CHR_WIND,CHR_SCHOOL,CHR_FIRE,CHR_CLINIC,CHR_PARK
            .byte CHR_HOME,CHR_BUS
                                    
; Search pattern for adjacent search, starting from index 3 (PTR minus 22)
CarPatt:    .byte 21,2,21,0

; Tornado thunder and lightning patterns
TornScr:    .byte 254, 15,255,  8,254, 14,  8,  8
TornFlash:  .byte 20,   2,  2,  9,  3,  6,  8, 10
TornThun:   .byte $ff,$ff,$f4,$f6,$f6,$f4,$f2,$f0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CUSTOM CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The character set must start at $1C00. If you change anything
; anywhere, you must account for this. The easiest way is to use
; padding bytes immediately before this character data.
;
; The easiest way to tell if you've done this right is to
; make sure that the object code is exactly 3583 bytes. This is
; a reliable method as long as you don't add anything AFTER this
; character data.
;
; (The following lines generate padding for XA)
pre_charset:
* = $1c00
.dsb (*-pre_charset)
* = $1c00

; Roads $00 - $0f
CharSet:    .byte $00,$aa,$aa,$aa,$00,$aa,$aa,$aa  ; 0000 Parking Lot 
            .byte $54,$54,$44,$54,$54,$44,$7c,$00  ; 0001 N			  
            .byte $00,$00,$7f,$40,$4c,$40,$7f,$00  ; 0010 E			        
            .byte $54,$54,$47,$40,$4c,$40,$3f,$00  ; 0011 N/E           
            .byte $00,$7c,$44,$44,$54,$54,$44,$44  ; 0100 S
            .byte $54,$54,$44,$44,$54,$54,$44,$44  ; 0101 N/S            
            .byte $00,$00,$3f,$40,$56,$50,$47,$44  ; 0110 S/E            
            .byte $54,$54,$47,$40,$54,$50,$47,$44  ; 0111 N/S/E          
            .byte $00,$00,$fe,$02,$da,$02,$fe,$00  ; 1000 W		
            .byte $54,$54,$c4,$14,$d4,$04,$f8,$00  ; 1001 N/W            
            .byte $00,$00,$ff,$00,$cc,$00,$ff,$00  ; 1010 E/W            
            .byte $54,$54,$c7,$00,$cc,$00,$ff,$00  ; 1011 E/W/N          
            .byte $00,$00,$f8,$04,$d4,$14,$c4,$44  ; 1100 S/W            
            .byte $54,$54,$c4,$04,$d4,$14,$c4,$44  ; 1101 N/S/W          
            .byte $00,$00,$ff,$00,$cc,$00,$c7,$44  ; 1110 E/W/S          
            .byte $54,$54,$c7,$00,$c6,$00,$c7,$44  ; 1111 Intersection   

; Title $10 - $1f (PQRSTU___VW.__XYZ[Lb]UpLeft)
            .byte $08,$eb,$d8,$bb,$08,$ff,$ff,$ff  ; ZE
            .byte $46,$db,$c7,$df,$5f,$ff,$ff,$ff  ; PT
            .byte $33,$6d,$6d,$6d,$73,$ff,$ff,$ff  ; O
            .byte $1c,$6b,$1b,$7b,$7c,$ff,$ff,$ff  ; PO
            .byte $dd,$5d,$5d,$5d,$c5,$ff,$ff,$ff  ; LI
            .byte $8f,$7f,$9f,$ef,$1f,$ff,$ff,$ff  ; S
            .byte $f8,$fd,$fd,$fd,$f3,$ff,$ff,$ff  ; J
            .byte $ce,$b5,$86,$b7,$b4,$ff,$ff,$ff  ; AS
            .byte $33,$ed,$6d,$ad,$73,$ff,$ff,$ff  ; SO
            .byte $b7,$97,$a7,$b7,$b7,$ff,$ff,$ff  ; N
            .byte $e2,$f6,$f6,$f6,$cf,$ff,$ff,$ff  ; JU
            .byte $d8,$d7,$d9,$de,$31,$ff,$ff,$ff  ; US
            .byte $8b,$da,$da,$da,$da,$ff,$ff,$ff  ; TIA
            .byte $36,$d2,$14,$d6,$d6,$ff,$ff,$ff  ; AN
            .byte $ff,$ff,$ff,$ff,$c7,$ff,$ff,$ff  ; $1e Minus
            .byte $ff,$ff,$ff,$ef,$c7,$ef,$ff,$ff  ; $1f Plus
                        
; Board Pieces $20 - $2f
            .byte $00,$00,$00,$00,$00,$00,$00,$00  ; $20 Space
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff  ; $21 Reverse Space
            .byte $00,$00,$44,$28,$10,$28,$44,$00  ; $22 Cancel
            .byte $00,$00,$fe,$00,$6c,$00,$fe,$00  ; $23 Road Placeholder       
            .byte $00,$10,$38,$6c,$c6,$44,$44,$7c  ; $24 Unocc. Home
            .byte $00,$00,$60,$fe,$82,$aa,$82,$fe  ; $25 Unocc. Business
WindFarm:   .byte $00,$10,$10,$38,$44,$10,$10,$10  ; $26 Wind Farm
            .byte $00,$18,$10,$7c,$ee,$fe,$aa,$aa  ; $27 School
            .byte $00,$0e,$0a,$fe,$fe,$8a,$ae,$ae  ; $28 Firehouse
            .byte $00,$18,$92,$fe,$ee,$c6,$ee,$fe  ; $29 Clinic
            .byte $00,$00,$40,$e0,$e0,$e0,$4e,$4a  ; $2a Park
            .byte $00,$00,$88,$cc,$ee,$cc,$88,$00  ; $2b End Turn
            .byte $00,$10,$38,$6c,$fe,$5c,$74,$74  ; $2c Home
            .byte $00,$00,$60,$fe,$aa,$fe,$aa,$fa  ; $2d Business
            .byte $00,$38,$6c,$d6,$fe,$dc,$ac,$78  ; $2e Lake
Burning:    .byte $00,$10,$18,$3c,$6e,$ee,$cc,$78  ; $2f Burned Down
                        
; Numerals $30 - $39
            .byte $ff,$ff,$83,$bb,$bb,$bb,$83,$ff  ; 0
            .byte $ff,$ff,$ef,$ef,$ef,$ef,$ef,$ff  ; 1
            .byte $ff,$ff,$83,$fb,$83,$bf,$83,$ff  ; 2
            .byte $ff,$ff,$83,$fb,$83,$fb,$83,$ff  ; 3
            .byte $ff,$ff,$bb,$bb,$83,$fb,$fb,$ff  ; 4
            .byte $ff,$ff,$83,$bf,$83,$fb,$83,$ff  ; 5
            .byte $ff,$ff,$83,$bf,$83,$bb,$83,$ff  ; 6
            .byte $ff,$ff,$83,$fb,$fb,$fb,$fb,$ff  ; 7
            .byte $ff,$ff,$83,$bb,$83,$bb,$83,$ff  ; 8
            .byte $ff,$ff,$83,$bb,$83,$fb,$fb,$ff  ; 9    
            
; Indicators $3a - $3f
            .byte $c7,$a7,$ff,$ef,$c7,$83,$d7,$ff  ; $3a Population
            .byte $e3,$c9,$98,$9c,$8c,$c9,$e3,$ff  ; $3b Money
            .byte $cf,$d7,$d7,$11,$5d,$5b,$03,$ff  ; $3c Satisfaction
            .byte $83,$ff,$ab,$ff,$ab,$ff,$ab,$ff  ; $3d Calendar
            .byte $ff,$ff,$bb,$f7,$ef,$df,$bb,$ff  ; $3e Percent Sign
            .byte $00,$78,$70,$78,$5c,$0e,$04,$00  ; $3f Cursor
