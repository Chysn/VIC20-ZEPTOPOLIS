;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;                               ZEPTOPOLIS DELUXE
;                             Cartridge Adaptation
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
; CARTRIDGE LAUNCHER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
* = $a000
Vectors:    .word Welcome       ; Welcome Screen
            .word Restart       ; NMI Address
            .byte $41,$30,$c3,$c2,$cd
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Configuration
MENU_ITEMS  = 9                 ; Number of actions in menu
EMP_THRESH  = 7                 ; Employment threshhold
HEIGHT      = 19                ; Maximum Y position
DEC_PAD     = 0                 ; Left padding character (0 for no padding)
RULE_SIZE   = RuleEnd - RuleSrc ; Size (in bytes) of rulesets

; Color Constants
; So I don't need to remember them anymore
BLACK       = 0
WHITE       = 1
RED         = 2
CYAN        = 3
PURPLE      = 4
GREEN       = 5
BLUE        = 6
YELLOW      = 7

; Character Constants
CHR_PROG    = $f8               ; Progress Bar
CHR_CURSOR  = $22               ; Cursor
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

; Character Color Constants
COL_CURSOR  = BLUE              ; Cursor, blue
COL_ROAD    = BLACK             ; Road, black
COL_UNOCC   = BLACK             ; Unoccupied properties, black
COL_OCC     = BLACK             ; Occupied properties, black
COL_SCHOOL  = PURPLE            ; School, purple
COL_WIND    = PURPLE            ; Wind Farm, purple
COL_FIRE    = RED               ; Firehouse, red
COL_CLINIC  = BLUE              ; Clinic, blue
COL_PARK    = GREEN             ; Park, green
COL_LAKE    = BLUE              ; Lake, blue
COL_BURN    = RED               ; Burned Down, red

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
; Output values for Control, and input values for MoveCoor
NORTH       = 0
EAST        = 1
SOUTH       = 2
WEST        = 3
FIRE        = 4
S_KEY       = 5                 ; "S" has been pressed (Save)
L_KEY       = 6                 ; "L" has been pressed (Load)
F1_KEY      = 7                 ; "F1" has been pressed (Disk)
F3_KEY      = 8                 ; "F3" has been pressed (Ruleset)

; Game Memory
UNDER       = $00               ; Character under pointer
TMP         = $01               ; Subroutine temporary storage
TMP_PTR     = $02               ; Temporary pointer (2 bytes)
CURR_ST     = $04               ; Current structure list (nearby or adjacent)
MISR_FL     = $05               ; Minimum ISR Flag (only timer and music)
PREV_X      = $08               ; Previous x coordinate
PREV_Y      = $09               ; Previous y coordinate
BUILD_IX    = $0a               ; Build index
RNDNUM      = $0b               ; Random number tmp
STR_PTR     = $21               ; String pointer
BUILD_MASK  = $30               ; Build mask (for wind farms)
ACTION_FL   = $31               ; Action flag
NEARBY_ST   = $32               ; Nearby structures bitfield
PATTERN     = $33               ; Current road search pattern
STEP_COUNT  = $34               ; Step count for pattern search
STEPS       = $35               ; Part of pattern with remaining steps
CURR_DIR    = $36               ; Current search direction
LAST_DIR    = $37               ; Last search direction
PROGRESS    = $38               ; Progress Bar
SOLD_HOME   = $39               ; Home was sold this turn, if 1
SOLD_BUS    = $3a               ; Business was sold this turn, if 1
SMART_COUNT = $3b               ; Count of Homes with nearby Schools
HOME_COUNT  = $3c               ; Home Count
EMP_COUNT   = $3d               ; Employer Count
POP         = $3e               ; Population (2 bytes)
COOR_X      = $40               ; x coordinate
COOR_Y      = $41               ; y coordinate
PAND_FL     = $42               ; Pandemic flag
YR_REVENUE  = $43               ; Previous year revenue (2 bytes)
DENOM       = $45               ; Happiness calculator register 1
NUMER       = $46               ; Happiness calculator register 2 (2 bytes)
QUAKE_FL    = $48               ; Earthquake flag
VALUE       = $49               ; Current property value
SMART       = $4b               ; Education (+0 - 9%)
BUS_CAP     = $4c               ; Business Activity (2 bytes)
ADJ_ST      = $4e               ; Adjacent structures bitfield
DEC_OP      = $4f               ; PrintDec Operand (2 bytes)
DEC_PAD_FL  = $51               ; Decimal padding flag
QUOTE_FL    = $52               ; Directory quote flag
FILE_COUNT  = $53               ; File count for disk directory
SHOW_COUNT  = $54               ; Show count for disk directory
DIR_IX      = $55               ; Directory index for disk directory
SAVE_EXT    = $56               ; Saved city extension
COL_PTR     = $f3               ; Screen color pointer (2 bytes)
YR_EXPEND   = $f9               ; Previous year expenditure (2 bytes)
PTR         = $fb               ; Pointer (2 bytes)
P_RAND      = $fd               ; Pseudorandom seed (2 bytes)
TIME        = $ff               ; Jiffy counter

; Music Player Memory
MUSIC_REG   = $2b               ; Music shift register (4 bytes)
MUSIC_FL    = $10               ; Bit 7 set if player is running
MUSIC_TIMER = $11               ; Music timer
MUSIC_MOVER = $12               ; Change counter

; RAM storage
RULESET     = $100d             ; Working Ruleset (59 bytes)
SAVE_NAME   = $1080             ; Disk save name for SETNAM (6 bytes)
SWAP_BOARD  = $1100             ; Swapped-out game board (512 bytes)
DIRECTORY   = $1300             ; Disk directory, with names separated
                                ;   by zeroes (2304 bytes)

; Animated Character locations
WindFarm    = $1d30             ; Location of Wind Farm character data
Burning     = $1d78             ; Location of Fire character data

; Game State Memory
; Things that must be saved to tape
PAND_YR     = $1ff7             ; Years until next Pandemic
PAND_COUNT  = $1ff8             ; Pandemic countdown
BZCON_FL    = $1ff9             ; Business Confidence flag
YEAR        = $1ffa             ; Year (2 bytes)
TREASURY    = $1ffc             ; Treasury (2 bytes)
QUAKE_YR    = $1ffe             ; Years until next Earthquake
HAPPY       = $1fff             ; Satisfaction (10% - 90%)

; Ruleset
; Loaded from cartridge to RAM during DataXfer routine
Mode        = $100d
Theme       = $1015
Tempo       = $1019
StartYear   = $101a
StartTreas  = $101c
LakeCount   = $101e
NewDevCost  = $101f
UpdateCost  = $1020
MaintCosts  = $1021
BusNVals    = $1026
HomeNVals   = $102d
BusAVals    = $1034
HomeAVals   = $103b
FireRisk    = $1042
QuakeFreq   = $1043
QuakePower  = $1044
TornFreq    = $1045
TornPath    = $1046
PandFreq    = $1047

; System Resources - Memory
CINV        = $0314             ; ISR vector
NMINV       = $0318             ; Release NMI vector
;-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
IBSOUT      = $0326             ; CHROUT vector
SCREEN      = $1e00             ; Screen character memory (unexpanded)
BOARD       = SCREEN+66         ; Starting address of board
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eaef             ; System ISR return point
ORIG_HORIZ  = $ede4             ; Default horizontal screen position
HORIZ       = $9000             ; Screen position
VICCR3      = $9003             ; Screen height
VICCR4      = $9004             ; Raster location
VICCR5      = $9005             ; Character map register
VOICEH      = $900c             ; High sound register
VOICEM      = $900b             ; Mid sound register
VOICEL      = $900a             ; Low sound register
NOISE       = $900d             ; Noise register
VOLUME      = $900e             ; Sound volume register/aux color
SCRCOL      = $900f             ; Screen color
VIA1PA      = $9111             ; Joystick port (up, down, left, fire)
VIA1DD      = $9113             ; Data direction register for tape
VIA2PB      = $9120             ; Joystick port (for right)
VIA2DD      = $9122             ; Data direction register for joystick
VIA1PA2     = $911f             ; VIA 1 DRA, no handshake
CASECT      = $0291             ; Disable Commodore case
VIATIME     = $9114             ; VIA 1 Timer 1 LSB
POWERSOF2   = $8270             ; Bit value at index (character ROM)
KEYCVTRS    = $028d             ; Keyboard codes
KEYDOWN     = $c5               ; Key held down
MSGFLG      = $9d               ; KERNAL message mode flag,
IOSTATUS    = $90               ; I/O Status

; System Resources - Routines
PLOT        = $fff0             ; Position cursor 
CHROUT      = $ffd2             ; Write one character
SETLFS      = $ffba             ; Setup logical file
SETNAM      = $ffbd             ; Setup file name
SAVE        = $ffd8             ; Save
LOAD        = $ffd5             ; Load
OPEN        = $ffc0             ; Open logical file
CLOSE       = $ffc3             ; Close logical file
CLALL       = $ffe7             ; Close all files
CHKIN       = $ffc6             ; Define file as input
CHRIN       = $ffcf             ; Get input
CLRCHN      = $ffcc             ; Close channel

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Restart:    jsr Setup           ; Set up hardware and initialize game
                                ; Unlike most of my games, there's no welcome
                                ;   screen or "press fire to start" text. The
                                ;   game simply starts when the program does.
            ; Fall through to Main                                

Main:       lsr ACTION_FL       ; Turn off Action flag
-wait:      jsr Controls        ; Wait for game controls
            bmi wait            ; ,,
            bit MUSIC_FL        ; Start music on first movement
            bmi ch_save         ; ,,
            jsr MusicInit       ; ,,            
ch_save:    cpx #S_KEY          ; If "S" was pressed, save the city
            bne ch_load         ; ,,
            jmp TapeSave        ; ,,
ch_load:    cpx #L_KEY          ; If "L" was pressed, load a city
            bne ch_disk         ; ,,
            jmp TapeLoad        ; ,,
ch_disk:    cpx #F1_KEY         ; If "F1" was pressed, go to Disk menu
            bne ch_ruleset       ; ,,
            jmp DiskMenu        ; ,,
ch_ruleset: cpx #F3_KEY         ; If "F3" was pressed, go to Ruleset menu
            bne ch_action       ; ,,
            jmp RuleMenu        ; ,,
ch_action:  cpx #FIRE           ; Has fire been pressed?
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
Action:     jsr Controls        ; Debounce the joystick before entering
            bmi Action          ; ,,
            sec                 ; Set Action flag
            ror ACTION_FL       ; ,,
            lda UNDER           ; Get character at cursor
            cmp #CHR_LAKE       ; If it's a Lake, allow no action here
            beq Main            ;   ,,
ch_space:   cmp #$20            ; If there's already something else there,
            beq show            ;   start the build index on that item, to
            cmp #CHR_BURN       ;   make it harder to accidentally remove
            beq show            ;   intentional development. Fire is
            ldx #0              ;   considered "nothing else there."
            stx BUILD_IX        ;   ,,
            jsr DrawDetail      ; Show info about this structure
show:       lda BUILD_IX        ; Show the structure being selected
            clc                 ; ,,
            adc #CHR_CURSOR     ; ,,
            ldx #0              ; ,,
            sta (PTR,x)         ; ,,
-debounce:  jsr Controls        ; Debounce joystick for action select
            bpl debounce        ; ,,            
-wait:      jsr Controls        ; Wait for the action
            bmi wait            ; ,,
            lda #%00001111      ; Reset time to reset cursor flash
            sta TIME            ; ,,
            cpx #FIRE           ; If fire is pressed, build the selected item
            beq Build           ; ,,
ch_prev:    cpx #WEST           ; If moving left, decrement the menu
            bne ch_next         ; ,,
            dec BUILD_IX        ; ,,
            bpl show            ; ,,
            lda #MENU_ITEMS     ; Or wrap around to the last item
            sta BUILD_IX        ; ,,
ch_next:    cpx #EAST           ; If moving right, increment the menu
            bne ch_cancel       ; ,,
            inc BUILD_IX        ; ,,
            lda BUILD_IX        ; ,,
            cmp #MENU_ITEMS     ; ,,
            bcc show            ; Or wrap around to the first item
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
action_d:   bne show            ; Go back to show item

; Build Item
; In BUILD_IX
Build:      asl ACTION_FL       ; Prevents ISR flash from messing up screen
-debounce:  jsr Controls        ; Debounce joystick for build
            bpl debounce        ; ,,
            lda UNDER           ; Remove the cursor
            jsr Place           ; ,,
            jsr Roads           ; Clear the Road trace
clr_det:    ldy #9              ; Clear right-hand part of Detail Bar
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
            adc #CHR_CURSOR     ;   one already here, then cancel the
            cmp UNDER           ;   build, to save the money
            beq cancel          ;   ,,
            lda NewDevCost      ; Default to new development cost
            ldx UNDER           ; But if there's something already there,
            cpx #$20            ; ,,
            beq buy             ; ,,
            lda UpdateCost      ; Use the update cost instead
buy:        jsr Spend           ; Try to do the spend
            bcs cancel          ; ,,
            jsr DrawTreas       ; Update Treasury amount
            lda BUILD_IX
            cmp #IDX_ROAD       ; If the player is building a road...
            bne reg_item        ;
            lda #%11011110      ;   ...character offset for placeholder
reg_item:   clc                 ; Show the item at the cursor
            adc #CHR_CURSOR     ; ,,
            sta UNDER           ; ,,
            jsr Place           ; ,,
            jsr Roads           ; Rebuild roads to account for potential
            ldx #0              ;   changes to the infrastructure
            lda (PTR,x)         ; Get the new character under the pointer
            sta UNDER           ;   and set that as UNDER.
cancel:     lda UNDER           ; Show the cursor if there's a space under it
            cmp #$20            ; ,,
            bne to_main         ; ,,
            jsr DrawCursor      ; ,,
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
            bpl animate         ; If not in Action, animate Wind Farms & Fire
            ldy #6              ; Default to blue
            lda #%00010000      ; Every 16 interrupts, flash the cursor
            and TIME            ; ,,
            bne flash_cur       ; ,,
            iny                 ; ,,
flash_cur:  tya                 ; ,,
            jsr SetColor        ; ,,
            jmp icons
animate:    ldx #0              ; Handle animations of Wind Farms and Fire
            lda #%00011111      ; Check to see if the time is nnn00000
            bit TIME            ; ,,
            bne svc_quake       ; If any of the first 5 bits are on, skip
            bvc frame2          ; If bit 6 is on, use the second frame instead
            ldx #5              ;   of the first
frame2:     ldy #4              ; Change the top 5 bytes of the character
-loop:      lda WFAnim1,x       ;   ,,
            sta WindFarm+1,y    ;   ,, (the Wind Farm)
            lda BurnAnim1,x     ;   ,,
            sta Burning+1,y     ;   ,, (the Fire)
            inx                 ; Get the next byte index
            dey                 ; Decrement the update counter
            bpl loop            ; ,,
svc_quake:  bit QUAKE_FL        ; Is there an Earthquake in progress?
            bpl icons           ;   If not, handle the flashing icons
-loop:      lda VICCR4          ; Wait for raster to be at top, so that
            bne loop            ;   the earthquake isn't all rastery
            lda HORIZ           ; Shake the ground by alternating the
            eor #$01            ;   VIC horizontal direction register
            sta HORIZ           ;   ,,
            jsr Rand31          ; Randomize volume for rumbling sound
            sta VOLUME          ;   (the extra bit doesn't matter here)
icons:      bit TIME            ; Flash icons as warnings that Homes or
            bvc def_icons       ;   Businesses are at risk of leaving.
            bit BZCON_FL        ; If the population is insufficient to support
            bpl ch_happy        ;   area business, flash the Population icon
            lda #$21            ;   ,,
            sta SCREEN+22       ;   ,,
ch_happy:   lda HAPPY           ; Check employment level
            cmp #EMP_THRESH     ; ,,
            bcs isr_r           ; If carry set, satisfaction is OK
            lda #$21            ; Otherwise, flash the satisfaction icon
            sta SCREEN+34       ; ,,
            bne isr_r           ; ,,
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
            jmp SetColor        ; ,,

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
                        
; Draw Overview Bar
; - Population   = Column 23
; - Treasury     = Column 7
; - Satisfaction = Column 13
; - Year         = Column 18
DrawOvervw: ldy #1              ; Plot population display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx POP             ; Numeric population display
            lda POP+1           ; ,,
            ldy #3              ; ,,
            jsr PrintDec        ; ,,
            lda #$21            ; Extra space for decrease
            jsr CHROUT
            ; TREASURY
DrawTreas:  ldy #7              ; Plot treasury display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx TREASURY        ; Numeric treasury display
            lda TREASURY+1      ; ,,
            ldy #3              ; ,,
            jsr PrintDec        ; ,,
            lda #$21            ; Extra space for decrease
            jsr CHROUT
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
            clc                 ; ,,
            adc #"0"            ; ,,
            jsr CHROUT          ; ,,
            lda SMART           ; With school percentage
            clc                 ; 
            adc #"0"            ; 
            jsr CHROUT          ; ,,
            ; YEAR
            ldy #18             ; Plot year display
            ldx #1              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            ldx YEAR            ; Numeric year display
            lda YEAR+1          ; ,,
            ldy #3              ; ,,
pr_year:    jmp PrintDec        ; ,,
       
; Draw Budget
; On status line       
DrawBudget: lda #<BudgetR       ; Show Budget Revenue Header
            ldy #>BudgetR       ; ,,
            jsr PrintStr        ; ,,
            ldx YR_REVENUE      ; Show Revenue
            lda YR_REVENUE+1    ; ,,
            ldy #2              ; ,,
            jsr PrintDec        ; ,,
            lda #<BudgetE       ; Show Budget Expenditure Header
            ldy #>BudgetE       ; ,,
            jsr PrintStr        ; ,,
            ldx YR_EXPEND       ; Show Expenditure
            lda YR_EXPEND+1     ; ,,
            ldy #2              ; ,,
            jmp PrintDec        ; ,,
                        
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
            lda TMP             ; Bitfield is also the character code
            ldx #0              ; Store in screen address
            sta (PTR,x)         ; ,,
next_cell:  jsr PlaceCol        ; Set the color of everything else
            inc COOR_Y          ; Continue iterating across all characters in   
            lda COOR_Y          ;   the maze
            cmp #HEIGHT         ;   ,,
            bne loop            ;   ,,
            lda #0              ;   ,,
            sta COOR_Y          ;   ,,
            inc COOR_X          ;   ,,
            lda COOR_X          ;   ,,
            cmp #22             ;   ,,
            bne loop            ;   ,,
            jmp ResetCoor       ; Reset coordinates
            
; Check Road
; Carry is set if the specified adjacent cell is a road
CheckRoad:  pha                 ; Store direction for later reversal
            jsr MoveCoor        ; Move the coordinates in specified direction
            jsr CheckBound      ; Is the new location in bounds?
            bcs restore         ; If not, restore and return
            jsr Coor2Ptr        ; Align pointer with new coordinate
            ldx #0              ; Get the character at the pointer
            lda (PTR,x)         ; ,,
            tay                 ; Y is now the character
            pla                 ; Get passed direction
            jsr restore2
            cpy #$10            ; Is the character a Road?
            bcc is_road         ; If so, go set carry and return
            clc                 ; If not, clear carry and return
            rts                 ; ,,
is_road:    sec                 ; Set carry when it's a Road
            rts                 ; ,,
restore:    pla                 ; Get passed direction
restore2:   eor #%00000010      ; Reverse the passed direction
            jsr MoveCoor        ; And set pointer to it
            clc                 ; (Only relevant for return from CheckRoad)
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
            bcc do_trace        ; If not, allow only Wind Farms to be added for
            lda #BIT_WIND       ;   this pattern, as Wind Farms may be in range
            sta BUILD_MASK      ;   without being connected by road.
do_trace:   bit BUILD_MASK      ; Skip the trace if only looking at Wind Farms
            bpl sh_step         ; ,,
            lda #PURPLE         ; Change color of Road 
            jsr SetColor        ; ,,
sh_step:    lsr STEPS           ; Shift to the next step, two bits right
            lsr STEPS           ; ,,
            dec STEP_COUNT      ; Decrement step count
            bne next_step       ; Do next step if not done with pattern
patt_done:  jsr Adjacents       ; Get buildings adjacent to the final step
            and BUILD_MASK      ; Mask allowable buildings for this pattern
            ora NEARBY_ST       ; Add this step's bitfield to cumulative byte
            sta NEARBY_ST       ; ,,
adv_patt:   inc PATTERN         ; Move to the next pattern
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
            jsr Tornado         ; Is there a Tornado this year?
            jsr Quake           ; Is there an Earthquake this year????
            jsr Pandemic        ; Is there a Pandemic this year?
            jsr SetScrCol       ; Set screen color
            jsr Upkeep          ; Perform calculations and maintenance
            jsr Collect         ; Collect income
            jsr DrawOvervw      ; Draw the Overview Bar contents
            jsr DrawBudget      ; Show the previous year's budget
            jsr Roads           ; Re-colorize the Roads
            jsr DrawCursor      ; Put the cursor back
            jsr MusicPlay       ; Music may have stopped during a disaster
            lda POP             ; ---- BUSINESS CONFIDENCE ----
            pha                 ; Start by temporarily increasing
            lda POP+1           ;   Population by the percentage of
            pha                 ;   Homes with nearby Schools
            lda SMART           ; Temporarily add education to population
            jsr AddPop          ;   for Business Confidence calculation
            lsr BZCON_FL        ; Reset Business Confidence flag
            lda BUS_CAP         ; If the annual Business value is greater
            cmp POP             ;   than the population, it means that the
            lda BUS_CAP+1       ;   population cannot support Businesses
            sbc POP+1           ;   ,,
            bcc next_r          ; Population is high enough
            sec                 ; Population is too low, so set Business
            ror BZCON_FL        ;   Confidence flag
next_r:     pla                 ; Restore the original population
            sta POP+1           ; ,,
            pla                 ; ,,
            sta POP             ; ,,
            lda KEYCVTRS        ; If the Shift key is held down at the end of
            and #$01            ;   the end of turn processing, immediately go
            bne NextTurn        ;   to the next turn
            jmp Main  
            
; Draw Detail Bar
; - If the structure has a maintenance cost, show that
; - If the structure is a Home or Business, show estimated revenue
; - If the strucutre has no maintenance cost, show structures nearby           
DrawDetail: cmp #$10            ; If the structure is a Road, do nothing
            bcc detail_r        ; ,,
            sec                 ; Set ISR to minimum, so that
            ror MISR_FL         ;   cursor flashes don't cause problems
            jsr Nearby          ; Trace Roads for this position
            lsr MISR_FL         ; Turn off Min-ISR flag
            lda UNDER           ; If the structure under the cursor
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
            ldy #18             ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            lda #$5e            ; Minus sign
            jsr CHROUT          ; ,,
            lda #";"            ; Show the coin
            jsr CHROUT          ; ,,
            pla                 ; Show the numeric maintenance cost
            tax                 ; ,,
            lda #0              ; ,,
            ldy #1              ; ,,
            jmp PrintDec        ; ,,
struct:     lda NEARBY_ST       ; Get nearby structures stored by Trace
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
            cmp #CHR_HOME       ; If it's an occupied Home or Business, show
            beq show_val        ;   the assessed value
            cmp #CHR_BUS        ;   ,,
            bne detail_r        ;   ,,
show_val:   jsr Assess          ;   ,,
            lda #$3b            ;   Coin symbol
            sta SCREEN+63       ;   ,,
            lda #$1f            ;   Plus sign
            sta SCREEN+64       ;   ,,
            lda VALUE           ;   ,,
            clc                 ;   ,,
            adc #$30            ;   Convert value into a numeral
            sta SCREEN+65       ;   Store it to the screen
detail_r:   rts 
      
; Handle Earthquake Disaster            
Quake:      dec QUAKE_YR
            beq yes_quake
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
            bne not_lake        ; ,,
            jsr Rand3           ; Flooding - If an Earthquake hits a Lake, the
            jsr MoveCoor        ;   Lake will flood, creating a new adjacent
            jsr CheckBound      ;   Lake
            bcs next_dmg        ;   ,,
            jsr Coor2Ptr        ;   ,,
            lda #CHR_LAKE       ;   ,,
            .byte $3c           ;   ,, (Skip word)
not_lake:   lda #CHR_BURN       ; Place damage on screen
place_dmg:  jsr Place           ; ,,
            lda #30             ; Delay during the shaking
            jsr Delay           ; ,,
next_dmg:   dey                 ; Go back for more destruction!
            bne loop            ; ,,
            sei                 ; ----QUAKE IS OVER----
            lsr QUAKE_FL        ; Reset flag
            lda ORIG_HORIZ      ; Restore the original horiz position
            sta HORIZ           ; ,,
            cli
            jsr MusicStop       ; Turn off Earthquake sounds
            jsr NextQuake       ; Set the date of the next disaster  
            jsr MusicPlay       ; Re-initialize the music after an earthquake
            jmp ResetCoor       ; Reset the coordinates  

; Handle Tornado Disaster            
Tornado:    lda TornFreq        ; Get frequency of Tornado in power of two
            jsr PRand           ; If this random check is 0, there's a
            beq yes_tor         ;   Tornado
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
            jsr RandCoor        ; Starting point of Tornado
            ldy #0              ; Increase volume
-loop:      sty VOLUME          ; ,,
            lda #$e8            ; ,,
            sta NOISE           ; ,,
            lda #13             ; ,,
            jsr Delay           ; ,,
            iny                 ; ,,         
            cpy #10             ; ,,
            bne loop            ; ,,
            ldy #7              ; Temporarily replace the burning icon with
-loop:      lda TornIcon,y      ;   the Tornado icon
            sta Burning,y       ;   ,,
            dey                 ;   ,,
            bpl loop            ;   ,,
            ldy TornPath        ; ---- START PATH ---- Maximum path length
-loop:      ldx #0              ; If there's a Lake at the new location, the
            lda (PTR,x)         ;   Tornado dissipates over the lake
            cmp #CHR_LAKE       ;   ,,
            beq tornado_r       ;   ,,
            jsr Destroy         ; Destroy landscape in Tornado's path
            inc NOISE           ; Increase pitch of Tornado
            ldx #5              ; Flash some lightning
flashes:    lda VICCR4          ; ,,
            bne flashes         ; ,,
            lda SCRCOL          ; ,,
            eor #%11110000      ; ,,
            sta SCRCOL          ; ,,
            txa                 ; Slight delay
            asl                 ; ,,
            jsr Delay           ; ,,
            dex
            bpl flashes          
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
tornado_r:  lda #$ff            ; Fade out with thunder
            sta NOISE           ; ,,
            ldy #$0a            ; Fade out the volume
-loop:      sty VOLUME          ; ,,
            cpy #8              ;   Turn the Tornado icon back into the
            bcs dec_vol         ;     burning icon
            lda BurnOrig,y      ;     ,,
            sta Burning,y       ;     ,,
dec_vol:    lda #5              ; ,,
            jsr Delay           ; ,,
            dey                 ; Continue fading volume
            bpl loop            ; ,,
            jsr MusicStop       ; Stop noise register
            lsr MISR_FL         ; Turn Min-ISR flag off for animations
            jmp ResetCoor       ; Reset coordinates

; Handle Pandemic disaster            
Pandemic:   lda PAND_COUNT      ; Is there a Pandemic currenly in progress?
            bpl yes_pand        ; ,,
            dec PAND_YR         ; Count down years until Pandemic
            bne pand_r          ; Is it time? If not, return
st_pand:    jsr Rand3           ; Pandemic is starting
            sta PAND_COUNT      ; Set the countdown (how long it will last)
pand_r:     rts
yes_pand:   dec PAND_COUNT      ; Pandemic in progress, set screen color
            bpl pand_r          ; ,,
            jmp NextPand
                                     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; UPKEEP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Upkeep
; Iterate through all map cells from left to right, starting at a pseudorandom
; column. 
Upkeep:     lda COOR_X          ; Save cursor coordinates
            pha                 ; ,,
            lda COOR_Y          ; ,,
            pha                 ; ,,
            jsr ClrInfo         ; Clear status bar
            lda #0              ; Reset sold Home and Business counters
            ldx #9              ; There are some values all stored together that
-loop:      sta PROGRESS,x      ;   need to be reset to 0 at the beginning of
            dex                 ;   the Upkeep
            bpl loop            ;   ,,
            jsr RandCoor        ; Get random COOR_X
            lda #0              ; And put COOR_Y at the top of the screen
            sta COOR_Y          ; ,,
-loop:      jsr Coor2Ptr        ; Get the character at the pointer
            ldx #$00            ; ,,
            lda (PTR,x)         ; ,,   
            cmp #CHR_UHOME      ; Handle UNOCCUPIED HOME
            bne ch_ubus         ; ,,
            jmp SellHome
ch_ubus:    cmp #CHR_UBUS       ; Handle UNOCCUPIED_BUSINESS
            bne ch_home
            jmp SellBus
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
            cmp #HEIGHT
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
upkeep_r:   jsr ClrInfo         ; Clear Info Bar
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
            jmp Destroy         ;   ,,
maint_r:    rts            
            

; Sell Home
; For an unoccupied Home at the coordinate, see if it qualifies for
; occupancy
; Rules - If the unoccupied Home has a nearby Wind Farm, it can sell
SellHome:   lda SOLD_HOME       ; If a Home was sold this turn, return
            bne sellh_r         ; ,,
            jsr Nearby          ; A Home must have a nearby Wind Farm
            and #BIT_WIND       ; ,,
            beq sellh_r         ; ,,
            lda #CHR_HOME       ; Place the Home
            jsr Place           ; ,,
            inc SOLD_HOME       ; Increment sell count
            jsr populate        ; Add population to Home & add Home count
sellh_r:    jmp next_map

; Sell Business
; For an unoccupied business at the coordinate, see if it qualifies for
; occupancy
; Rules - An unoccupied Business cannot sell without a nearby Wind Farm
;       - If the unoccupied Business is adjacent to a Business, it can sell
;       - If the unoccupied Business has a nearby Home, it can sell
SellBus:    bit BZCON_FL        ; If Business Confidence is low, new
            bmi sellb_r         ;   Businesses will not move in
            lda SOLD_BUS        ; If a business was sold this turn, return
            bne sellb_r         ; ,,
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
            inc SOLD_BUS        ; Increment sell count
            inc EMP_COUNT       ; Increment employer count
sellb_r:    jmp next_map

; Compute Home
; - Determine whether the family leaves or stays
; - Assess the Home value
; - Add the tax revenue from the Home
CompHome:   jsr Nearby          ; Get nearby structures
            lda #BIT_WIND       ; Is there a Wind Farm nearby?
            bit NEARBY_ST       ; ,,
            bne ch_hfire        ; If so, go to next check
            lda #CHR_UHOME      ; If not, occupants move out
            jmp Place           ; ,,
ch_hfire:   jsr CatchFire       ; Handle fire risk
            bcs comph_r         ; ,,
ch_emp:     lda HAPPY           ; Check satisfaction against employment
            cmp #EMP_THRESH     ;   threshhold
            bcs ch_hclinic      ;   ,,
            jsr Rand7           ; If unsatisfied, there's a 1 in 8 chance
            bne ch_hclinic      ;   each Home will move out
            lda #CHR_UHOME      ;   ,,
            jmp Place           ;   ,,             
ch_hclinic: lda #BIT_CLINIC     ; Is there a Clinic nearby?
            bit NEARBY_ST       ; ,,
            beq ch_hschool      ; ,,
            lda PAND_COUNT      ; Skip the Clinic population bonus in a
            bpl ch_hschool      ;   Pandemic year
            lda #1              ; A nearby Clinic adds to 1 a Home's
            jsr AddPop          ;   population
ch_hschool: lda #BIT_SCHOOL     ; Is there a School nearby?
            bit NEARBY_ST       ; ,,
            beq hrevenue        ; ,,           
            inc SMART_COUNT     ; If so, count this as a smart Home
hrevenue:   lda #CHR_HOME       ; Assess Home property value
            jsr Assess          ; ,,
            jsr Revenue         ; Add property value to Treasury
            lda #BIT_CLINIC     ; If there's a clinic, go to healthy
            bit NEARBY_ST       ;   population increase
            bne healthy         ;   ,,
populate:   lda PAND_COUNT      ; Is there a Pandemic going on?
            bmi healthy         ;   If not, go to normal population
            lda #0              ; If this Home is affected by the Pandemic,
            beq sick            ;   it loses the random 0-3 population
healthy:    jsr Rand3           ; Add people to each Home (3-6)
sick:       clc                 ; ,,
            adc #3              ; ,,
            jsr AddPop          ; ,,
            inc HOME_COUNT      ; Count Homes
comph_r:    rts

; Compute Business 
; - Determine whether the Business leaves or stays
; - Assess the Business value
; - Add the tax revenue from the Business
CompBus:    bit BZCON_FL        ; Do population-based confidence test
            bpl bus_near        ; Confidence is okay, proceed to next check
            jsr Rand7           ; If confidence is low, there's a 1 in 8 chance
            beq bus_leave       ;   that the business leaves
bus_near:   jsr Nearby          ; Get nearby structures
            lda #BIT_WIND       ; Is there a Wind Farm nearby?
            bit NEARBY_ST       ; ,,
            bne ch_broad        ; If so, go to next check
bus_leave:  lda #CHR_UBUS       ; If not, business moves out
            jmp Place           ;   ,,
ch_broad:   lda #BIT_ROAD       ; Does the Business still have an
            bit ADJ_ST          ;   adjacent road?
            beq bus_leave       ; If not, it moves right out
            jsr CatchFire       ; Handle fire risk
            bcs compbus_r       ; ,,
brevenue:   lda #CHR_BUS        ; Assess Business property value
            jsr Assess          ; ,,
            jsr Revenue         ; Add property value to Treasury
            inc EMP_COUNT       ; Count employers
            lda VALUE           ; Add Business's value to the Business
            clc                 ;   Activity amount
            adc BUS_CAP         ;   ,,
            sta BUS_CAP         ;   ,,
            bcc compbus_r       ;   ,,
            inc BUS_CAP+1       ;   ,,
compbus_r:  rts  

; Handle Risk of Catching Fire
; For Homes and Businesses, the same
; Assumes Coordinate and NEARBY_ST are set
; Carry is set if the structure has been destroyed by fire
CatchFire:  lda #BIT_FIRE       ; Is there a nearby Firehouse?
            bit NEARBY_ST       ; ,,
            bne no_risk         ; If so, no risk
            lda FireRisk        ; Get the fire risk parameter (default is
            jsr PRand           ;   1 in 32)
            bne no_risk         ;   ,,
Destroy:    lda #CHR_BURN       ; Otherwise, burn it down!
            jsr Place           ; ,,
            sec                 ; Set Carry to indicate the property was
            rts                 ;   destroyed
no_risk:    clc                 ; Clear Carry to indicate that the property
            rts                 ;   survived
                         
; Assess Home or Business Value
; A is CHR_HOME or CHR_BUS
; Store result in VALUE
; This assumes that Nearby has already been called so that NEARBY_ST and ADJ_ST
; are set correctly.
Assess:     ldy #0              ; Initialize value
            sty VALUE           ; ,,
            ldy #<BusNVals      ; Set assessment table
            sty TMP_PTR         ; ,,
            ldy #>BusNVals      ; ,,
            sty TMP_PTR+1       ; ,,
            cmp #CHR_HOME       ; ,,
            bne assess_st       ; ,,
            ldy #<HomeNVals     ; Override assessment table
            sty TMP_PTR         ; ,,
            ldy #>HomeNVals     ; ,,
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
            
; Add Population
; In A
AddPop:     clc
            adc POP
            sta POP
            bcc addpop_r
            inc POP+1
addpop_r:   rts      

; Set Screen Color
SetScrCol:  lda #254            ; Return screen color to normal
            sta SCRCOL          ; ,,
            lda PAND_COUNT      ; If there's a Pandemic in progress, the border
            bmi no_pand         ;   color needs to be yellow instead of blue,
            inc SCRCOL          ;   so just increase by one. Convenient!
no_pand:    rts

; Fires Out
; Converts MOST fires (75%) to spaces  
; Handles screen between index 66 (after header, Overview and Detail) and
; 483 (last non-data-storage screen space)          
FiresOut:   ldx #209            ; (End (484) - Start (66)) / 2
-loop:      lda SCREEN+65,x     ; X goes 1-209, so SCREEN+65,x covers 66-274
            cmp #CHR_BURN       ;   Is there a fire here?
            bne bott_scr        ;   If not, check the bottom half
            jsr Rand3           ;   If so, roll D4 and preserve fire on 0
            beq bott_scr        ;   ,,
            lda #$20            ;   Put out this fire 75% of the time
            sta SCREEN+65,x     ;   ,,
bott_scr:   lda SCREEN+274,x    ; X goes 1-209, so SCREEN+285,x covers 275-483
            cmp #CHR_BURN       ;   Is there a fire here?
            bne no_fire         ;   If not, loop
            jsr Rand3           ;   If so, roll D4 and preserve fire on 0
            beq no_fire         ;   ,,
            lda #$20            ;   Put out this fire 75% of the time
            sta SCREEN+274,x    ;   ,,
no_fire:    dex
            bne loop
            rts
         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SUBROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reset Coordinates
; Coordinates are often stored on the stack. This resets them.
ResetCoor:  pla
            sta COOR_Y
            pla
            sta COOR_X
            jsr Coor2Ptr
            rts  

; Delay A Jiffies
; Actually, between (A - 1) and A jiffies, but that shouldn't matter for the
; purposes of this routine
Delay:      clc
            adc TIME
-loop:      cmp TIME
            bne loop
            rts 
            
; Clear Info Bar            
ClrInfo:    ldy #21
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
Rand31:     lda #%00001000      ; 5-bit
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
            
; Read the Controls
; Return the direction in X
; 0=North, 1=East, 2=South, 3=West, 4=Fire, 5=Save, 6=Load,
; $ff=None (for testing BMI/BPL)
Controls:   lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda VIA1PA          ; Read VIA1 port
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
            bne control_r       ; If so, set that as the joystick direction
            dex                 ; Loop back until found, or 0
            bne loop            ; ,,
            lda #$ff            ; Set DDR back to default
            sta VIA2DD          ; ,,
ch_S:       lda KEYDOWN         ; Key current keypress
            cmp #41             ; "S" for Save
            bne ch_L            ; ,,
            ldx #S_KEY+1        ; ,, (will be 5 after DEX below)
ch_L:       cmp #21             ; "L" for Load
            bne ch_F1           ; ,,
            ldx #L_KEY+1        ; ,, (will be 6 after DEX below)
ch_F1:      cmp #39             ; "F1" for Disk Menu
            bne ch_F3           ; ,,
            ldx #F1_KEY+1       ; ,,
ch_F3:      cmp #47             ; "F3" for Ruleset Menu
            bne control_r       ; ,,
            ldx #F3_KEY+1       ; ,,
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

; Check Boundary
; of COOR_X and COOR_Y
; Carry clear means in bounds, carry set means out of bounds
CheckBound: lda COOR_X          ; Check X coordinate for <0
            bmi out             ; ,,
            cmp #22             ; Check X coordinate for >21
            bcs out             ; ,,
            lda COOR_Y          ; Check Y coordinate for <0
            bmi out             ; ,,
            cmp #HEIGHT         ; Check Y coordinate
            rts                 ; Set is out, clear is in
out:        sec                 ; Return with carry set (out)
            rts                 ; ,,
            
; Randomize Coordinate            
RandCoor:   jsr Rand31          ; Get random location 0-31
            cmp #20             ; If it's not a good X coordinate range,
            bcs RandCoor        ;   get another one
            sta COOR_X          ; Else, keep it as X
-rnd_y:     jsr Rand31          ; Do the same with Y
            cmp #17             ; ,,
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
            bcc SetColor        ; Branch always, as Carry is clear here
lookup_col: tax                 ; X is screen code
            bpl col_table       ; If char >= $80, the character is from
            ldx #0              ;   scenario generator. Use Road index
col_table:  lda Colors-$23,x    ; Look up the color in the table by screen code
            ; Fall through to SetColor

; Set Color
; In A, at color location corresponding to PTR
SetColor:   pha                 ; A is the color
            lda PTR             ; But before I can make use of that, I need to
            sta COL_PTR         ;   set a color pointer. This is the same
            lda PTR+1           ;   method used by BASIC to set the color
            and #$03            ;   pointer from a screen pointer
            ora #>COLOR         ;   ,,
            sta COL_PTR+1       ;   ,,
            ldx #0              ; Now set the saved color in the color address
            pla                 ; ,,
            sta (COL_PTR,x)     ; ,,
setcol_r:   rts
            
; Get Happiness
; Basically an employment percentage from 10% to 90%
; 90% if the number of employers exceeds the number of workers
; 0% if there are no Businesses
GetHappy:   lda #9             ; Default to 9 if there are more Homes than
            sta HAPPY           ;   Businesses
            ldy HOME_COUNT      ; Number of Homes
            cpy EMP_COUNT       ; Compare to number of employers
            bcc happy_r         ; Same or more employers than workers so 90%
            beq happy_r         ; ,,
            lda #1              ; If there are no businesses to divide, set
            sta HAPPY           ;   happiness to 10%
            ldy EMP_COUNT       ;   ,,
            beq happy_r         ;   ,,
            sed
            ldy HOME_COUNT      ; Home count is the denominator
            jsr Hex2Deci        ; Convert to decimal
            sta DENOM           ;   and store
            ldy EMP_COUNT       ; Employer count is numerator
            jsr Hex2Deci        ;   Convert to decimal
            sta NUMER           ;   And store
            jsr Divide          ; Multiply NUMER by 10
            cld
            clc                 ; Add quotient to HAPPY
            adc HAPPY           ; ,,
            sta HAPPY           ; ,,
            dec HAPPY           ; Decrement to compensate for starting 1
happy_r:    rts
            
; Get Smartness
; Percentage of the number of Homes with a School nearby, to the
; tens place.
; 0% if there are no Schools or Homes
GetSmart:   lda #0              ; Starting default
            sta SMART           ; ,,
            ldy HOME_COUNT      ; Stay at 0 if either count is 0
            beq smart_r         ; ,,
            lda SMART_COUNT     ; ,,
            beq smart_r         ; ,,
            cpy SMART_COUNT     ; If the counts are the same, force
            bne do_div          ;   the count to 9
            lda #9              ;   ,,
            sta SMART           ;   ,,
            bne smart_r         ;   ,,
do_div:     sed
            jsr Hex2Deci        ; Convert to decimal
            sta DENOM           ;   and store
            ldy SMART_COUNT     ; Number of smart Homes
            jsr Hex2Deci        ; Convert to decimal
            sta NUMER           ;   and store
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
; For decimal mode conversion of a hex number in Y
; Return the decimal number in A
Hex2Deci:   lda #$00            ; Initialize return value
-loop:      clc                 ; Add one to A for each Y. When A hits $0a
            adc #$01            ;   it'll instead increment the high nybble
            dey                 ;   as the 10s place
            bne loop            ;   ,,
            rts            

; Swap Out
; For start of menu screen
SwapOut:    lda UNDER           ; Clear the Cursor out of the way
            jsr Place           ; ,,
            ldy #0
-loop:      lda SCREEN,y
            sta SWAP_BOARD,y
            lda SCREEN+$0100,y
            sta SWAP_BOARD+$0100,y
            dey
            bne loop
            rts
            
; Swap In
; For return to game
SwapIn:     ldy #0
-loop:      lda SWAP_BOARD,y
            sta SCREEN,y
            lda SWAP_BOARD+$0100,y
            sta SCREEN+$0100,y
            dey
            bne loop
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Welcome Screen
; This is the starting point of the cartridge auto start
Welcome:    jsr $fd8d           ; Test RAM, initialize VIC chip
            jsr $fd52           ; Restore default I/O vectors
            jsr $fdf9           ; Initialize I/O registers
            jsr $e518           ; Initialize hardware
            cli                 ; Clear interrupt flag from ROM jump
            lda #<Restart       ; Install the custom NMI (restart)
            sta NMINV           ; ,, 
            lda #>Restart       ; ,,
            sta NMINV+1         ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #44             ; Set 22-character screen height
            sta VICCR3          ;
            lda #254            ; Set initial screen color
            sta SCRCOL          ; ,,
            ldx #0
-loop:      lda Skyline,x
            sta SCREEN+234,x
            lda #6
            sta COLOR+234,x
            inx
            bne loop
            lda #240            ; Restore character set
            sta VICCR5          ; ,,
            lda #<Intro         ; Show Splash Screen
            ldy #>Intro         ; ,,
            jsr PrintStr        ; ,,
            ldx #RULE_SIZE
-loop:      lda RuleSrc,x
            sta RULESET,x
            dex
            bpl loop
-wait:      jsr Controls
            cpx #FIRE
            bne wait
-debounce:  jsr Controls
            bpl debounce
            jmp Restart
            
; Set up hardware
Setup:      lda VIATIME+1       ; Seed random number generator.
            ora #$01            ; ,,
            sta P_RAND          ; ,,
            lda VIATIME         ; ,,
            ora #$80            ; ,,
            sta P_RAND+1        ; ,,
            lsr ACTION_FL       ; Clear Action flag (before ISR starts)
            jsr MusicStop       ; Clear sound registers
            lda #$ff            ; Set custom character set
            sta VICCR5          ; ,,
            ldx #0
-loop:      lda CharSet,x       ; Move character set data
            sta $1c00,x         ; ,,
            lda CharSet+256,x   ; ,,
            sta $1d00,x         ; ,,
            dex                 ; ,,
            bne loop            ; ,,
InstallISR: sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            ; Fall through to InitGame
            
; Initialize Game            
InitGame:   lda #254            ; Set initial screen color
            sta SCRCOL          ; ,,
            lda #<Header        ; Show Board Header
            ldy #>Header        ; ,,
            jsr PrintStr        ; ,,
            lda #0              ; Reset build index to Cancel
            sta BUILD_IX        ; ,,
            sta POP             ; Initialize population
            sta POP+1           ; ,,
            sta HOME_COUNT      ; Initialize counts
            sta EMP_COUNT       ; ,,
            sta SMART_COUNT     ; ,,
            sta BZCON_FL        ; Starting Business Confidence flag
            sta MISR_FL         ; Turn off Min-ISR flag
            lda StartTreas      ; Set initial treasury amount
            sta TREASURY        ; ,,
            lda StartTreas+1    ; ,,
            sta TREASURY+1      ; ,,
            lda StartYear       ; Initialize year
            sta YEAR            ; ,,
            lda StartYear+1     ; ,,
            sta YEAR+1          ; ,,
            lda #$ff            ; Reset Pandemic Counter
            sta PAND_COUNT      ; ,,
            lda #"1"            ; Default Save extension
            sta SAVE_EXT        ; ,,
            ldy LakeCount       ; Add a number of Lakes to the board
-loop:      jsr RandCoor        ; ,,
            lda #CHR_LAKE       ; ,,
            jsr Place           ; ,,
            dey                 ; ,,
            bne loop            ; ,,
            lda #10             ; Set initial coordinates
            sta COOR_X          ; ,,
            sta COOR_Y          ; ,,
            jsr DrawOvervw      ; Show Overview Bar contents
            lda #6              ; Set Info Bar color to blue
            ldy #21             ; ,,
-loop:      sta COLOR+44,y      ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            jsr DrawCursor      ; Show cursor
            jsr NextPand        ; Set next Pandemic year
            ; Fall through to NextQuake
            
; Next Quake Year
NextQuake:  jsr Rand7
            clc
            adc QuakeFreq
            sta QUAKE_YR
            rts
                        
; Next Pandemic Year
NextPand:   jsr Rand7
            clc
            adc PandFreq
            sta PAND_YR
            rts

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
            bne FetchNote       ;   by flipping bit 0 of registers 1 and 3
            lda MUSIC_REG+1     ;   ,,
            eor #$01            ;   ,,
            sta MUSIC_REG+1     ;   ,,
            lda MUSIC_REG+3     ;   ,,
            eor #$01            ;   ,,
            sta MUSIC_REG+3     ;   ,,
            lda #$7f            ; Reset the mover for a little less than 4 loops
            sta MUSIC_MOVER     ;   per pattern so it goes longer without repeat
FetchNote:  lda Tempo           ; Reset the timer
            sta MUSIC_TIMER     ; ,,
            bit MUSIC_REG       ; A high note played when bit 7 of byte 0 is
            bpl play_high       ;   clear
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
set_vol:    lda MUSIC_REG+3     ; Set the starting volume based on one of the
            and #$0f            ;   registers. It will be reduced by one per
            sta VOLUME          ;   ISR cycle until 0
music_r:    rts

; Stop Music
MusicStop:  lsr MUSIC_FL        ; Turn off Music flag
            ldy #4              ; Clear sound registers $900a-$900e
            lda #0              ; ,,
-loop:      sta VOICEL,y        ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            rts

; Initialize Music
MusicInit:  ldy #3              ; Really, X would normally be used as the index
-loop:      lda Theme,y         ;   because there's a ZP,X mode. But the
            sta MUSIC_REG,y     ;   Control subroutine returns X, and using Y
            dey                 ;   here avoids having to keep track of that at
            bpl loop            ;   the cost of one extra byte for ABS,Y
            lda #0
            sta VOLUME
            sta MUSIC_MOVER
            jsr FetchNote
            ; Fall through to MusicPlay

; Play Music            
MusicPlay:  sec
            ror MUSIC_FL
            rts
            
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; TAPE SAVE/LOAD
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
TapeSave:   ldx #209            ; Record icon
            stx SCREEN+64       ; ,,
            lda #250            ; Screen/Border color (red border)
            jsr TapeSetup       ; Set up tape
            beq TapeClnup       ; STOP was pressed
            ldx #1              ; Device number
            ldy #0              ; Command (none)
            jsr SETLFS          ; ,,            
            lda #4              ; Filename is the current four-digit year,
            ldx #<SCREEN+40     ;   which can be scraped from the screen
            ldy #>SCREEN+40     ;   ,,
            jsr SETNAM          ;   ,,
            ldx #<SCREEN        ; Low byte start
            stx $c1             ; ,,
            ldy #>SCREEN        ; High byte start
            sty $c2             ; ,,
            lda #$c1            ; Set tab
            iny                 ; 512 bytes
            iny                 ; ,,
            jsr SAVE            ; SAVE
TapeClnup:  asl VIA1DD          ; Restore data direction register
            jsr SetScrCol       ; Set correct screen color
            jsr ClrPrompt       ; Clear tape prompts
            jsr MusicPlay       ; Restart music
            jmp new_pos         ; Return from tape operation        

; Tape Load
TapeLoad:   lda #253            ; Screen/Border color (green border)
            jsr TapeSetup       ; Set up tape
            beq TapeClnup       ; STOP was pressed
            ldx #1              ; Tape device number
            ldy #1              ; Load to header location
            jsr SETLFS          ; ,,
            lda #0              ; Get whatever the next file is
            jsr SETNAM          ; ,,
            lda #$00            ; Command for LOAD
            jsr LOAD            ; ,,
            ldx #0              ; Preserve whatever came from the load
            lda (PTR,x)         ;   under the Cursor
            sta UNDER           ;   ,,
            jsr Roads           ; Re-colorize the world
            jmp TapeClnup       ; Clean up the operation

; Tape Setup
; - Removes the Cursor from the screen
; - Stops music
; - Changes screen border color to green
; - Waits for tape button activation or STOP
; Returns with Z=1 if operation is canceled
TapeSetup:  sta TMP
            ldx #$3f            ; Show "Play" icon always
            stx SCREEN+65       ; ,,
            lda UNDER           ; Clear the Cursor out of the way
            jsr Place           ; ,,
            jsr MusicStop       ; Stop music during tape
-wait:      bit VIA1PA2         ; Is tape switch activated?
            bvc rolling         ;   If so, roll tape
            lda KEYDOWN         ; Check for STOP key
            cmp #$18            ; ,,
            bne wait            ; ,,
            rts                 ; Return from caller with Z=1
rolling:    lda #$00            ; set all DRA lines low
            sta	VIA1PA2         ; set VIA 1 DRA
	        sta	MSGFLG          ; disable KERNAL messages
            sta $91             ; Stop isn't held down
            lda	#%11000000      ; OOIIIIII, set cassette switch to output
            sta	VIA1DD          ; ,,
            lda TMP             ; Pull the screen color
            sta SCRCOL          ;   and set it
ClrPrompt:  lda #$20            ; Clear the tape prompt icons
            sta SCREEN+64       ; ,,
            sta SCREEN+65       ; ,,
            rts                 ; Return from caller with Z=0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DISK SAVE/LOAD (F1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Diskette Menu                        
DiskMenu:   jsr CLALL
            sec                 ; Activate minimal ISR for disk menu
            ror MISR_FL         ; ,,
            ldx #0              ; Clear keyboard buffer
            stx $c6             ; ,,
            lda #$f0            ; Set default character set
            sta VICCR5          ; ,,
            jsr MusicStop          
            jsr SwapOut         ; Swap out the existing city screen
            lda #<Disk1         ; Show Disk Menu
            ldy #>Disk1         ; ,,
            jsr PrintStr        ; ,,
MenuInput:  ldy #3              ; Put the year after the word Save
-loop:      lda SWAP_BOARD+40,y ; ,,
            sta SCREEN+145,y    ; ,,
            dey                 ; ,,
            bpl loop            ; ,,
            lda SAVE_EXT        ; Put the save extension after the year
            sta SCREEN+150      ; ,,
-debounce:  lda KEYDOWN         ; Debounce key
            cmp #$40            ; ,,
            bne debounce        ; ,,
-wait:      lda KEYDOWN         ; Wait for key
            cmp #$40            ; ,,
            beq wait            ; ,,
            cmp #$18            ; STOP pressed, return to Main
            bne ch_disk_l       ; ,,
ExitMenu:   jsr SwapIn          ; Swap city data back to screen
LoadDone:   lda #$ff            ; Set character set back to city
            sta VICCR5          ; ,,
            ldy #66             ; Set the header color back
            lda #BLUE           ;   to blue
-loop:      sta COLOR,y         ;    ,,
            dey                 ;    ,,
            bpl loop            ;    ,,
            jsr SetScrCol       ; Put screen color back to game state            
            jsr MusicPlay       ; Start music
            jsr Roads           ; Fix colors
            jsr DrawCursor      ; Unhide the cursor
            lsr MISR_FL         ; Clear minimal ISR flag
            jmp Main            ; Back to Main
ch_disk_l:  cmp #21             ; "L" for disk load
            bne ch_disk_s       ; ,,
            jmp LoadMenu        ; ,,
ch_disk_s:  cmp #41             ; "S" for disk save
            beq DiskSave        ; ,,
ch_filen:   jsr $ffe4           ; Change filename
            beq wait            ; If no keypress, go back to wait
            cmp #"0"            ; Make sure the key is between 0 and 9
            bcc wait            ; ,,
            cmp #"9"+1          ; ,,
            bcs wait            ; ,,
            sta SCREEN+150      ; Set it in the filename location
            sta SAVE_EXT        ; Set it in the extension storage
            jmp wait
            
; Save Current City
DiskSave:   lda #250            ; Screen/Border color (red border)
            sta SCRCOL          ; ,,
            ldy #6              ; Transfer name from screen into a
-loop:      lda SCREEN+145,y    ;   save name buffer for use by
            sta SAVE_NAME,y     ;   SETNAM
            dey                 ;   ,,
            bpl loop            ;   ,,
            ldy #66             ; Set the header color back
            lda #BLUE           ;   to blue
-loop:      sta COLOR,y         ;    ,,
            dey                 ;    ,,
            bpl loop            ;    ,,
            lda #$ff            ; Display city character set during save
            sta VICCR5          ; ,,
            jsr SwapIn          ; ,,
            jsr Roads           ; Show in the correct colors
            ldx #8              ; Device number
            ldy #0              ; Command (none)
            jsr SETLFS          ; ,,            
            lda #6              ; Filename is the current four-digit year,
            ldx #<SAVE_NAME     ;   number sign, and extension
            ldy #>SAVE_NAME     ;   ,,
            jsr SETNAM          ;   ,,
            ldx #<SCREEN        ; Low byte start
            stx $c1             ; ,,
            ldy #>SCREEN        ; High byte start
            sty $c2             ; ,,
            lda #$c1            ; Set tab
            iny                 ; 512 bytes
            iny                 ; ,,
            jsr SAVE            ; SAVE
            bcs ErrRecover      ; Recover from error, if carry set
            jmp ExitMenu        ; If SAVE was okay, exit disk menu

; Recover from Disk Error
; By restoring the disk menu screen           
ErrRecover: lda #254            ; Set the screen color back; it may have
            sta SCRCOL          ;   been changed by the failed operation
            lda #$f0            ; Back to default character set
            sta VICCR5          ; ,,
            jsr SwapOut         ; Swap out the existing city screen
            ; Fall through to DiskError
            
; Display Disk Error
; And return to Diskette Menu             
DiskError:  jsr CLALL           ; Close all files
            lda #<Disk1         ; Show Disk Menu
            ldy #>Disk1         ; ,,
            jsr PrintStr        ; ,,
            lda #<Disk2         ; Show Disk Error
            ldy #>Disk2         ; ,,
            jsr PrintStr        ; ,,
            jmp MenuInput       ; Back to Menu
              
; Generate Directory     
LoadMenu:   jsr CLALL
            lda #<DIRECTORY     ; Initialize directory storage
            sta TMP_PTR         ; ,,
            lda #>DIRECTORY     ; ,,
            sta TMP_PTR+1       ; ,,
            lda #0              ; Initialize file count
            sta FILE_COUNT      ; ,,
            lda #<Disk3         ; Show "getting directory"
            ldy #>Disk3         ; ,,
            jsr PrintStr        ; ,,
            lda #1              ; SETNAM - (1) Set name length
            ldx #<lfs+1         ; - Set name as the $ used below
            ldy #>lfs+1         ; ,,
            jsr SETNAM          ; ,,
lfs:        lda #"$"            ; SETLFS - Set file number as $
            ldx #8              ; - Device number
            ldy #0              ; - Command
            jsr SETLFS          ; ,,
            jsr OPEN
            bcs DiskError
            ldx #"$"
            jsr CHKIN
            jsr CharIn          ; Dispose of PRG header
            jsr CharIn          ; ,,
            bcs DiskError       ; ,,
newline:    lda #0              ; Set quote flag to 0. Needs to be 0 because
            sta QUOTE_FL        ;   this flag has three possible states
            ldx #4              ; Dispose of next line pointer and
-loop:      jsr CharIn          ;   line number
            bcs DiskError       ;   ,,
            dex                 ;   ,,
            bne loop            ;   ,,
-loop:      jsr CharIn          ; Get next character from line
            bcs DiskError       ; ,,
            beq EOL             ; 0 indicates end-of-line
            cmp #$22            ; Is character a quotation mark?
            bne proc_name       ; If not, go process the name
            sec                 ; Set the quote flag to either %10000000 or
            ror QUOTE_FL        ;   %11000000
            bcc loop            ; Go back for next character
proc_name:  bit QUOTE_FL        ; Check the quote state
            bvs loop            ; If bit 6 is set, the quote is finished
            bpl loop            ; If bit 7 is clear, the quote hasn't started
            jsr DirAdd          ; Add character to directory
            jmp loop
EOL:        bit QUOTE_FL        ; If the line ends without a filename in quotes
            bvc EOF             ;   go to end of file. Else go back for more.
            lda FILE_COUNT      ; If there are already 255 files, no more
            cmp #$ff            ;   can be displayed, so end file
            beq EOF             ;   ,,
            lda #0              ; Add delimiter to directory
            jsr DirAdd          ; ,,
            beq EOF             ; Out of memory, so end directory capture
            inc FILE_COUNT      ; Increment file count
            jmp newline
EOF:        jsr CLRCHN          ; Clear input channel and close file when
            lda #"$"            ;   a quote pair isn't found
            jsr CLOSE           ;   ,,
            lda #0              ; Add a final zero, for last file
            jsr DirAdd          ; ,,
            lda #<Disk4         ; Show Load Menu
            ldy #>Disk4         ; ,,
            jsr PrintStr        ; ,,
            lda FILE_COUNT
            cmp #1
            bne init_dir
            lda #<Disk5         ; No files on disk
            ldy #>Disk5         ; ,,
            jsr PrintStr        ; ,,
init_dir:   lda #1              ; Initialize directory display at top (skipping the
            sta DIR_IX          ;   disk name)
ShowDir:    lda #1              ; Short delay to make the list more controllable
            jsr Delay           ; ,,
            jsr ShowFiles       ; Show filenames from directory index
-wait:      lda KEYDOWN         ; Wait for control. Is STOP pressed?
            cmp #$18            ; ,,
            bne ch_load_f       ; ,,
            jmp ExitMenu        ; If so, exit disk menu
ch_load_f:  cmp #$0f            ; Is RETURN pressed?
            bne ch_sel          ; ,,
            jmp DiskLoad        ; If so, load file
ch_sel:     jsr Controls        ; Is the joystick pointing somewhere?
            bmi wait            ; If not, go back for more
            cpx #NORTH          ; North decreases the file index
            bne ch_nx_file      ; ,,
            dec DIR_IX          ; ,,
            bne ShowDir         ; ,, If it's 0, bring back to 1 because
            inc DIR_IX          ; ,, 0 is the disk name
            bne ShowDir         ; Redraw directory and continue
ch_nx_file: cpx #SOUTH          ; South increases the file index
            bne ShowDir         ; ,,
            inc DIR_IX          ; ,,
            lda DIR_IX          ; ,, If it's reached the file count, go
            cmp FILE_COUNT      ; ,, back
            bne ShowDir         ;    ,,
            dec DIR_IX          ;    ,,
            bne ShowDir         ;    ,,

; Load Selected File            
DiskLoad:   lda #253            ; Screen/Border color (green border)
            sta SCRCOL          ; ,,
            lda #<Disk6         ; Loading message
            ldy #>Disk6         ; ,,
            jsr PrintStr        ; ,,
            jsr AdvFile         ; Get pointer to name
            ldy #0              ; Get name length
-loop:      lda (TMP_PTR),y     ; Get next character
            iny                 ; Count name length
            cmp #0              ; Have we reached the end of the name?
            bne loop            ; ,,
            dey                 ; Went one too far, so back off
            dey                 ; Decrement Y to refer to the final character
            lda (TMP_PTR),y     ; If the last character of the filename is
            cmp #"0"            ;   a numeral, then use that numeral as the
            bcc load_setn       ;   save extension, as a courtesy to the
            cmp #"9"+1          ;   player, who may wish to keep the same
            bcs load_setn       ;   extension for other saved games
            sta SAVE_EXT        ;   ,,
load_setn:  iny                 ; Increment Y to be the length
            tya                 ; A is the filename
            ldx TMP_PTR         ; X is low byte of name pointer
            ldy TMP_PTR+1       ; Y is high byte of name pointer
            jsr SETNAM          ;   ,,
            ldx #8              ; Tape device number
            ldy #1              ; Load to header location
            jsr SETLFS          ; ,,
            lda #$00            ; Perform load
            jsr LOAD            ; ,,
            bcs load_err        ; Display error if carry set
            lda $af             ; Is this a city (ending at $2000)?
            cmp #$20            ; ,,
            beq city            ; If not, exit by swapping city back in
            jsr MusicInit       ;   ,, (Restart music if ruleset was loaded)
            jmp ExitMenu        ;   ,,
city:       ldx #0              ; Preserve whatever came from the load
            lda (PTR,x)         ;   under the Cursor
            sta UNDER           ;   ,,
            jmp LoadDone
load_err:   jmp DiskError       ; Because it's out of relative branch range            
                        
; Character Input
; C=1 if failure          
CharIn:     jsr CHRIN           ; Call KERNAL input
            pha                 ; Save character on stack
            lda IOSTATUS        ; Is the read status okay?
            cmp #$01            ; ,,
            pla                 ; Bring back read byte
            rts
            
; Add to Directory   
; Sets Z if out of memory                 
DirAdd:     ldx TMP_PTR+1       ; Out of directory space?
            cpx #$1c            ; ,,
            beq diradd_r        ; Then no more entries
            ldx #0              ; Store name character
            sta (TMP_PTR,x)     ; ,,
            jsr IncFnPtr        ; Increment pointer
diradd_r:   rts                              

; Show Filenames
; From DIR_IX   
ShowFiles:  lda FILE_COUNT      ; Show nothing if no files on disk
            cmp #1              ; ,,
            beq showf_r         ; ,,
            ldx #10             ; Plot start of file display
            ldy #0              ; ,,
            clc                 ; ,,
            jsr PLOT            ; ,,
            jsr AdvFile         ; Advance file pointer to file index
            ldy #10             ; Show up to ten files
file_line:  lda #$20            ; Space before filename
            jsr CHROUT          ; ,,
            lda #221            ; Left-hand border
            jsr CHROUT          ; ,,
            cpy #10
            bne no_rvs
            lda #18             ; Reverse on for first file
            jsr CHROUT          ; ,,
no_rvs:     lda #18             ; Set name length
            sta TMP             ; ,,
-loop:      ldx #0              ; Get next character
            lda (TMP_PTR,x)     ; ,,
            beq next_file       ; ,,
            jsr CHROUT          ; Show the character
            dec TMP             ; Decrement name length
            jsr IncFnPtr
            bne loop
next_file:  lda #$20            ; Clear the rest of the line
-loop:      jsr CHROUT          ; ,,
            dec TMP             ; ,,
            bne loop            ; ,,
            lda #146            ; Unreverse
            jsr CHROUT          ; ,,
            lda #221            ; Right-hand border
            jsr CHROUT          ; ,,
            lda #$0d            ; Drop to next line
            jsr CHROUT          ; ,,
            jsr IncFnPtr        ; Advance pointer off the 0
            lda SHOW_COUNT      ; Was the last file displayed?
            cmp FILE_COUNT      ; ,,
            beq show_bott        ; If so, file list is done
            inc SHOW_COUNT      ; Increment show count
            dey                 ; Decrement maximum file display
            bne file_line       ; Go get next file
show_bott:  lda #<DirBottom     ; Directory bottom
            ldy #>DirBottom     ; ,,
            jsr PrintStr        ; ,,
            ldy #21
-loop:      lda #$20
            jsr CHROUT
            dey
            bne loop
showf_r:    rts

; Advance File
; Put pointer at DIR_IX
AdvFile:    lda #0              ; Set show count to 0. This will be
            sta SHOW_COUNT      ;   incremented until we hit DIR_IX
            ldy DIR_IX          ; Y = directory index
            lda #<DIRECTORY     ; Initialize directory storage
            sta TMP_PTR         ; ,,
            lda #>DIRECTORY     ; ,,
            sta TMP_PTR+1       ; ,,
adv_more:   jsr IncFnPtr
            ldx #0
            lda (TMP_PTR,x)
            bne adv_more
            inc SHOW_COUNT
            lda SHOW_COUNT
            cmp DIR_IX
            bne adv_more
            ; Fall through to pointer increment for start of next filename

; Increment filename pointer
IncFnPtr:   inc TMP_PTR
            bne incfn_r
            inc TMP_PTR+1
incfn_r:    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RULESET SELECTION (F3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
RuleMenu:   jsr CLALL
            sec                 ; Activate minimal ISR for disk menu
            ror MISR_FL         ; ,,
            ldx #0              ; Clear keyboard buffer
            stx $c6             ; ,,
            lda #$f0            ; Set default character set
            sta VICCR5          ; ,,
            jsr MusicStop          
            jsr SwapOut         ; Swap out the existing city screen
            lda #<RuleInst      ; Show Disk Menu
            ldy #>RuleInst      ; ,,
            jsr PrintStr        ; ,,
-wait:      lda KEYDOWN         ; Wait for a key to be pressed
            cmp #$40            ; ,,
            beq wait            ; ,,
            cmp #$18            ; If stop is pressed, exit this menu
            bne ch_level        ; ,,
            jmp ExitMenu        ; ,,
ch_level:   jsr $ffe4           ; Check pressed level number
            beq wait            ; If no keypress, go back to wait
            cmp #"1"            ; Make sure the key is between 1 and 4
            bcc wait            ; ,,
            cmp #"4"+1          ; ,,
            bcs wait            ; ,,
            sec                 ; Convert pressed key to data index
            sbc #"1"            ; ,,
            tay                 ; Y is the data index
            lda #<RuleSrc       ; TMP_PTR will be the pointer to the ruleset
            sta TMP_PTR         ;   source data
            lda #>RuleSrc       ;   ,,
            sta TMP_PTR+1       ;   ,,
            cpy #0              ; If index is 0, we already have TMP_PTR
            beq rule_copy       ; ,,
-loop:      lda #RULE_SIZE      ; Add the size of the ruleset Y times
            clc                 ; ,,
            adc TMP_PTR         ; ,,
            sta TMP_PTR         ; ,,
            bcc next_rule       ; ,,
            inc TMP_PTR+1       ; ,,
next_rule:  dey                 ; ,,
            bne loop            ; ,,
rule_copy:  ldy #RULE_SIZE      ; Y is the index of the last byte in the ruleset
            dey                 ;   RULE_SIZE - 1
-loop:      lda (TMP_PTR),y     ; Install rule in ROM to working ruleset in RAM
            sta RULESET,y       ; ,,
            dey                 ; All the way down to byte 0
            bpl loop            ; ,,
            jsr MusicInit       ; Start new soundtrack
            jmp ExitMenu
                                                                                
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; OUTPUT ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Print Decimal Number
; Left-padded by spaces
; X = Low Byte
; A = High Byte
; Y = Number of Decimal Places Minus 1 (0-4)
;     Place overflow is not handled, so if you're wrong, you'll leave
;     base 10 for something undefined. Overflow can be handled by watching X
;     at print_pl, if you need to do that.
PrintDec:   lsr DEC_PAD_FL      ; Clear padding flag
            stx DEC_OP          ; Store operand
            sta DEC_OP+1        ; ,,
-next:      ldx #0              ; X is the number of subtractions per place
-loop:      lda DEC_OP          ; Is there enough of a remainder in the
            cmp PlaceLow,y      ;   operand to perform the division?
            lda DEC_OP+1        ;   ,,
            sbc PlaceHigh,y     ;   ,,
            bcc print_pl        ; If not, use 0 for this place
            inx                 ; Perform 16-bit substractions from the
            lda DEC_OP          ;   operand, counting the number of times
            sec                 ;   the place value was subtracted in X.
            sbc PlaceLow,y      ;   This will be the place digit.
            sta DEC_OP          ;   ,,
            lda DEC_OP+1        ;   ,,
            sbc PlaceHigh,y     ;   ,,
            sta DEC_OP+1        ;   ,,
            bcs loop            ; 
print_pl:   txa                 ; 
            bne in_number       ; If the number is nonzero, just print it
            bit DEC_PAD_FL      ; Has there been a nonzero value in this number?
            bmi in_number       ;   If so, print this as a zero
            cpy #0              ; Is this the ones place?
            beq in_number       ;   If so, print the final zero
            lda #DEC_PAD        ; Print this one as padding
            jmp dec_out         ; If DEC_PAD=0, number will be flush left
in_number:  sec                 ; When the first nonzero number is printed,
            ror DEC_PAD_FL      ;   set the padding flag to stop padding
            ora #"0"            ; Add "0" to the calculated place digit, print
dec_out:    jsr CHROUT          ;   ,, (X and Y are both safe from this)
            dey                 ; Decrement place counter and continue
            bpl next            ; ,,
            rts

; Print String
; Like BASIC's $cb1e, but not destructive to BASIC memory when coming from
; the BASIC input buffer (see $d4bb)         
PrintStr:   sta STR_PTR
            sty STR_PTR+1
            ldy #$00
-loop:      lda (STR_PTR),y
            beq print_r
            jsr CHROUT
            lda #$00            ; Turn off quote mode for each character
            sta $d4             ; ,,
            iny
            bne loop
print_r:    rts   

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Splash Screen text
Intro:      .asc $13,$0d,$0d,$1f,"      ZEPTOPOLIS",
            .asc $0d,$0d,$0d,$0d,"  2O21 JASON JUSTIAN",
            .asc $0d,$0d," BEIGEMAZE.COM/VICLAB",0

; Game Board Header
Header:     .asc $93,$1f,"PQRSTU!!!!!!!!VWXYZ[",$5c,"]"
            .asc ":!!!!!;!!!!!<!!>!=!!!!",0

; Disk Utility Text
; Menu
Disk1:      .asc $93,$0d,$1f,"    DISKETTE  MENU",$0d,$0d,$0d,
            .asc " ",$12,"0",$92,"-",$12,"9",$92," : SET FILENAME",$0d,$0d,
            .asc " ",$12,"S",$92,"   : SAVE ",$22,$90,"    # ",$1f,$22,$0d,$0d,
            .asc " ",$12,"L",$92,"   : LOAD",$0d,$0d,
            .asc " ",$12,"STOP",$92,": CANCEL",$0d,0
; Error
Disk2:      .asc $13,$0d,$1c,"  *** DISK ERROR *** ",$1f,$0d,$0d,$0d,$0d,$0d,
            .asc $0d,$0d,$0d,$0d,$0d,0
; Getting Directory
Disk3:      .asc $0d,$0d,$1e," GETTING DIRECTORY...",$1f,0
; Load Screen
Disk4:      .asc $93,$0d,$1f," LOAD CITY OR RULESET",$0d,$0d,$0d,
            .asc " JOYSTICK: SELECT",$0d,$0d,
            .asc " ",$12,"RETURN",$92,"  : LOAD",$0d,$0d,
            .asc " ",$12,"STOP",$92,"    : CANCEL",$0d,
            .asc $20,176,192,192,192,192,192,192,192,192,192,192,192,192,192
            .asc 192,192,192,192,192,174,0
DirBottom:  .asc $20,173,192,192,192,192,192,192,192,192,192,192,192,192,192
            .asc 192,192,192,192,192,189,$0d,0
; No Files
Disk5:      .asc $13,$0d,$1c,"   *** NO FILES *** ",$1f,0
; Loading...
Disk6:      .asc $13,$0d,$1e,"  *** LOADING... *** ",$1f,0

; Rule Menu instructions
RuleInst:   .asc $93,$0d,$1f,"     RULESET MENU",$0d,$0d,$0d,
            .asc " ",$12,"1",$92,"   : NORMAL",$0d,$0d,
            .asc " ",$12,"2",$92,"   : EASY ",$0d,$0d,
            .asc " ",$12,"3",$92,"   : HARD",$0d,$0d,
            .asc " ",$12,"4",$92,"   : DISASTERS!",$0d,$0d,
            .asc " ",$12,"STOP",$92,": CANCEL",$0d,$0d,$0d
            .asc "AFTER CHANGING RULESET"
            .asc "PRESS RESTORE TO START"
            .asc "A NEW CITY,OR CONTINUE"
            .asc "CURRENT CITY.",0

; Budget Header
BudgetR     .asc $13,$11,$11,$5f,";",$00
BudgetE     .asc $21,$5e,";",$00

; Direction tables
JoyTable:   .byte $00,$04,$80,$08,$10,$20          ; Corresponding direction bit
DirTable:   .byte $01,$02,$04,$08                  ; Index to bit value

; Image data - Wind Farm and Burning animations, Tornado
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

; Decimal system place values for 16-bit numbers, from low to high
; 1, 10, 100, 1000, 10000            
PlaceLow:   .byte $01,$0a,$64,$e8,$10
PlaceHigh:  .byte $00,$00,$00,$03,$27

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ROM RULESETS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; See ruleset-boilerplate.asm for more information.
; This data is loaded by the Welcome subroutine to $100d so that it can be
; overwritten by a ruleset load
RuleSrc

; Musical Mode
; Dorian         
_Mode:      .byte 147,159,163,175,183,191,195,201         

; Musical Theme
_Theme:     .byte $33,$44,$55,$66
_Tempo:     .byte 10

; Starting conditions
_StartYear: .word 2021
_StartTreas:.word 500
_LakeCount: .byte 6

; Build costs
_NewDevCost:.byte 5
_UpdateCost:.byte 10

; Yearly maintenance costs of maintainable structures
;                 Wind Farm,School, Firehouse, Clinic, Park
_MaintCosts:.byte 5,        15,     10,        10,     1

; Assessed values for NEARBY structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
_BusNVals:  .byte 0,         0,      2,         0,      0,    2,     0
_HomeNVals: .byte 0,         2,      0,         2,      1,    0,     2

; Assessed values for ADJACENT structures ($ff = -1)
;                 Wind Farm, School, Firehouse, Clinic, Park, Home,  Business
_BusAVals:  .byte $ff,       0,      1,         0,      1,    1,     2
_HomeAVals: .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff

; Fire Risk configuration
; This is the annual risk of an unprotected property (Home or Business) burning
; down. It is expressed as a carry at a specific bit value.
_FireRisk:  .byte %00001000

; Earthquake configuration
; The next Earthquake will happen QuakeFreq years from now, plus 0-7
; years. When an Earthquake happens, this timer will be reset.
; QuakePower determines how much damage an earthquake does
; The default is 15 + (0-7) years, or between 15-22 years
_QuakeFreq: .byte 15
_QuakePower:.byte 15

; Tornado configuration
; Tornadoes are checked every turn. If the pseudo-random value is 0, there will
; be a Tornado.
; TornPath determines the maximum path length
; The default is a 1 in 8 chance per year, with a maximum path length of 6
_TornFreq:  .byte %00100000
_TornPath:  .byte 6

; Pandemic configuration
; The next Pandemic will happen PandFreq years from now, plus 0-8 years.
; When a Pandemic happens, this timer will be reset.
; The default is 25 + (0-7) years, or between 25-32 years
_PandFreq:  .byte 25

RuleEnd                         ; For setting RULE_SIZE contant

; Easy Ruleset
EasyRules:
            .byte 147,159,163,175,183,191,195,201         
            .byte $02,$20,$00,$3c
            .byte 10
            .word 1999
            .word 700
            .byte 6
            .byte 5
            .byte 10
            .byte 3,        7,      5,         7,     1
            .byte 0,         0,      2,         0,      0,    2,     0
            .byte 0,         2,      0,         2,      1,    0,     2
            .byte $ff,       0,      1,         0,      1,    1,     2
            .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff
            .byte %00000010
            .byte 50
            .byte 5
            .byte %00001000
            .byte 4
            .byte 100

; Hard Ruleset
HardRules:
            .byte 147,159,163,175,183,191,195,201         
            .byte $05,$41,$85,$00
            .byte 7
            .word 1990
            .word 200
            .byte 2
            .byte 5
            .byte 10
            .byte 6,        17,     12,        12,     2
            .byte 0,         0,      2,         0,      0,    2,     0
            .byte 0,         2,      0,         2,      1,    0,     2
            .byte $ff,       0,      1,         0,      1,    1,     2
            .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff
            .byte %00010000
            .byte 12
            .byte 20
            .byte %00100000
            .byte 7
            .byte 18
            
; Disaster Ruleset
            .byte 147,159,163,175,183,191,195,201         
            .byte $55,$aa,$55,$ab
            .byte 9
            .word 2012
            .word 600
            .byte 4
            .byte 5
            .byte 10
            .byte 5,        15,     10,        10,     1
            .byte 0,         0,      2,         0,      0,    2,     0
            .byte 0,         2,      0,         2,      1,    0,     2
            .byte $ff,       0,      1,         0,      1,    1,     2
            .byte $ff,       1,      $ff,       $ff,    1,    $fe,   $ff
            .byte %01000000
            .byte 5
            .byte 20
            .byte %01000000
            .byte 8
            .byte 12            

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; CITY CHARACTER SET
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; This will be copied from CharSet to $1c00, the location of the custom
; character set.
; Roads $00 - $0f
CharSet:    .byte $00,$aa,$aa,$aa,$00,$aa,$aa,$aa  ; 0000 Parking Lot 
            .byte $54,$54,$44,$54,$54,$44,$7c,$00  ; 0001 N			 ??
            .byte $00,$00,$7f,$40,$4c,$40,$7f,$00  ; 0010 E			 ??      
            .byte $54,$54,$47,$40,$4c,$40,$3f,$00  ; 0011 N/E ?? ?? ?? ?? ?? 
            .byte $00,$7c,$44,$44,$54,$54,$44,$44  ; 0100 S
            .byte $54,$54,$44,$44,$54,$54,$44,$44  ; 0101 N/S ?? ?? ?? ?? ?? ??
            .byte $00,$00,$3f,$40,$56,$50,$47,$44  ; 0110 S/E ?? ?? ?? ?? ?? ??
            .byte $54,$54,$47,$40,$54,$50,$47,$44  ; 0111 N/S/E ?? ?? ?? ?? ??
            .byte $00,$00,$fe,$02,$da,$02,$fe,$00  ; 1000 W		
            .byte $54,$54,$c4,$14,$d4,$04,$f8,$00  ; 1001 N/W ?? ?? ?? ?? ?? ??
            .byte $00,$00,$ff,$00,$cc,$00,$ff,$00  ; 1010 E/W ?? ?? ?? ?? ?? ??
            .byte $54,$54,$c7,$00,$cc,$00,$ff,$00  ; 1011 E/W/N ?? ?? ?? ?? ??
            .byte $00,$00,$f8,$04,$d4,$14,$c4,$44  ; 1100 S/W ?? ?? ?? ?? ?? ??
            .byte $54,$54,$c4,$04,$d4,$14,$c4,$44  ; 1101 N/S/W ?? ?? ?? ?? ??
            .byte $00,$00,$ff,$00,$cc,$00,$c7,$44  ; 1110 E/W/S ?? ?? ?? ?? ??
            .byte $54,$54,$c7,$00,$c6,$00,$c7,$44  ; 1111 Intersection ?? 

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
            .byte $00,$78,$70,$78,$5c,$0e,$04,$00  ; $22 Cursor
            .byte $44,$54,$54,$44,$44,$54,$54,$44  ; $23 Road??Placeholder?? ?? ?? ??
            .byte $00,$10,$38,$6c,$c6,$44,$44,$7c  ; $24 Unocc. Home
            .byte $00,$00,$60,$fe,$82,$aa,$82,$fe  ; $25 Unocc. Business
            .byte $00,$10,$10,$38,$44,$10,$10,$10  ; $26 Wind Farm
            .byte $00,$18,$10,$7c,$ee,$fe,$aa,$aa  ; $27 School
            .byte $00,$0e,$0a,$fe,$fe,$8a,$ae,$ae  ; $28 Firehouse
            .byte $00,$18,$92,$fe,$ee,$c6,$ee,$fe  ; $29 Clinic
            .byte $00,$10,$38,$7c,$10,$7c,$fe,$10  ; $2a Park
            .byte $00,$00,$88,$cc,$ee,$cc,$88,$00  ; $2b End Turn
            .byte $00,$10,$38,$6c,$fe,$5c,$74,$74  ; $2c Home
            .byte $00,$00,$60,$fe,$aa,$fe,$aa,$fa  ; $2d Business
            .byte $00,$38,$6c,$d6,$fe,$dc,$ac,$78  ; $2e Lake
            .byte $00,$10,$18,$3c,$6e,$ee,$cc,$78  ; $2f Burned Down
                        
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
            .byte $00,$60,$78,$7e,$7e,$78,$60,$00  ; $3f Play Icon
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DETROIT SKYLINE FOR SPLASH PAGE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Skyline:    .byte $20,$6c,$79,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$20,$20,$e1
            .byte $e7,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$e1,$e7,$2e
            .byte $20,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$20,$20,$68
            .byte $79,$7b,$20,$e5,$a0,$bc,$79,$79
            .byte $d0,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$20,$20,$a3,$a0,$61
            .byte $ca,$ad,$a0,$ae,$a0,$a0,$e7,$20
            .byte $20,$20,$20,$20,$20,$20,$20,$20
            .byte $20,$20,$20,$da,$a0,$c8,$a7,$ba
            .byte $a0,$ae,$a0,$a0,$e7,$20,$20,$20
            .byte $20,$20,$2c,$20,$20,$20,$20,$20
            .byte $20,$e5,$a0,$a7,$ca,$ad,$a0,$a9
            .byte $a0,$a0,$e7,$20,$20,$20,$20,$68
            .byte $a3,$7b,$20,$20,$20,$20,$20,$a0
            .byte $a0,$a0,$e7,$a0,$a0,$bd,$a0,$a0
            .byte $e7,$20,$ae,$a0,$65,$cb,$fd,$61
            .byte $00,$f8,$76,$75,$20,$a0,$a0,$a0
            .byte $a7,$a0,$a0,$a0,$a0,$a0,$e7,$20
            .byte $a7,$a0,$80,$ba,$ba,$ca,$62,$bd
            .byte $fe,$a2,$62,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$e7,$64,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a7,$ba,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$66,$f9,$e8,$f0
            .byte $e8,$bd,$bd,$c0,$c0,$ae,$ba,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0,$a0,$a0
            .byte $a0,$a0,$a0,$a0,$a0,$a0
            
CodeEnd
