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
;     42 SYS4110
* = $1001
Launcher:   .byte $0b,$10,$2a,$00,$9e,$34,$31,$31
            .byte $30,$00,$00,$00,$00

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; LABEL DEFINITIONS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Configuration
FX_VOICE    = $900c             ; Sound effects voice
IN_YEAR     = 2021              ; Starting year on display
IN_TREASURY = 100               ; Starting treasury
MENU_ITEMS  = 10                 ; Number of actions in menu

; Character Constants
CHR_CURSOR  = $3f               ; Cursor
CHR_CANCEL  = $22               ; Action Cancel
CHR_BUS     = $2c               ; Business
CHR_ROAD    = $23               ; Road Placeholder

; Color Constants
COL_CURSOR  = 6                 ; Cursor, blue
COL_ROAD    = 0                 ; Road, black
COL_UNOCC   = 3                 ; Unoccupied properties, cyan
COL_OCC     = 6                 ; Occupied properties, blue
COL_SCHOOL  = 5                 ; School, green
COL_WIND    = 4                 ; Wind Farm, purple
COL_FIRE    = 2                 ; Firehouse, red
COL_CLINIC  = 2                 ; Clinic, red
COL_PARK    = 5                 ; Park, green
COL_FOREST  = 5                 ; Forest, green
COL_BURN    = 2                 ; Burned Down, red

; Index Constants
IDX_ROAD    = 1

; Constants
NORTH       = 0
EAST        = 1
SOUTH       = 2
WEST        = 3
FIRE        = 4

; Game Memory
UNDER       = $07               ; Character under pointer
PREV_X      = $08               ; Previous x coordinate
PREV_Y      = $09               ; Previous y coordinate
BUILD_IX    = $0a               ; Build index
ITERATOR    = $0b               ; Local iterator
COL_PTR     = $f3               ; Screen color pointer (2 bytes)
COOR_X      = $f9               ; x coordinate
COOR_Y      = $fa               ; y coordinate
PTR         = $fb               ; Pointer (2 bytes)
P_RAND      = $fd               ; Pseudorandom seed (non-zero)
TMP         = $fe               ; Subroutine temporary storage
TIME        = $ff               ; Jiffy counter

; Game State
TREASURY    = $1ffb             ; Treasury (2 bytes)
YEAR        = $1ffd             ; Year (2 bytes)

; System Resources - Memory
CINV        = $0314             ; ISR vector
;NMINV       = $0318             ; Release NMI vector
-NMINV     = $fffe             ; Development NMI non-vector (uncomment for dev)
SCREEN      = $1e00             ; Screen character memory (unexpanded)
BOARD       = SCREEN+44         ; Starting address of board
COLOR       = $9600             ; Screen color memory (unexpanded)
IRQ         = $eb15             ; System ISR return point
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

; System Resources - Routines
PRTSTR      = $cb1e             ; Print from data (Y,A)
PRTFIX      = $ddcd             ; Decimal display routine (A,X)
PLOT        = $fff0             ; Position cursor 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MAIN PROGRAM
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Welcome:    jsr Setup           ; Set up hardware and initialize game
                                ; Unlike most of my games, there's no welcome
                                ;   screen or "press fire to start" text. The
                                ;   game simply starts when the program does.

Main:       jsr Joystick        ; Wait for joystick movement
            bmi Main            ; ,,
            cpx #FIRE           ; Has fire been pressed?
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
;             
Action:     jsr Joystick        ; Debounce the fire button before entering
            cpx #FIRE           ; ,,
            beq Action          ; ,,
show:       ldx #0              ; Show the item being selected
            lda BUILD_IX        ; ,,
            clc                 ; ,,
            adc #CHR_CANCEL     ; ,,
            sta (PTR,x)         ; ,,
-wait:      jsr Joystick        ; Wait for the action
            bmi wait            ; ,,
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
            bne wait            ; ,,
            inc BUILD_IX        ; ,,
            lda BUILD_IX        ; ,,
            cmp #MENU_ITEMS     ; ,,
            bcc action_d        ; Or wrap around to the first item
            lda #0              ; ,,
            sta BUILD_IX        ; ,,
action_d:   jsr Joystick        ; Debounce joystick for action select
            bpl action_d        ; ,,
            jmp show            ; Go back to show item
action_r:   jmp Main

; Build Item
; In BUILD_IX
Build:      jsr Joystick        ; Debounce the fire button for build
            cpx #FIRE           ; ,,
            beq Build           ; ,,
            lda BUILD_IX        ; Get the built character
            beq cancel          ;
            cmp #IDX_ROAD       ; Has a road been placed?
            beq build_road      ; ,, 
            clc                 ; Show the item at the cursor
            adc #CHR_CANCEL     ; ,,
            sta UNDER           ; ,,
            jsr Place           ; ,,
            jmp Main
cancel:     ldx #0              ; Do nothing except show the cursor
            jsr OnlyCursor      ; ,,
            jmp Main
build_road: lda #$0f
            jsr Place
            jsr Roads
            jmp Main
            
; Interrupt Service Routine 
; Replaces the BASIC ISR and performs these functions
;   - Advances the timer, used for delays
;   - Handles Wind Farm rotation
;   - Services sound effects       
ISR:        inc TIME            ; Circumventing BASIC's clock, so advance it
            ;jsr FXService       ; Play any sound effects that are going on
            jmp IRQ             ; Return from interrupt

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
            stx TMP             ; TMP is the passageway bitfield (0000wsen)
            lda #WEST           ; Check West
            jsr CheckAdj        ; ,,
            rol TMP             ; ,,
            lda #SOUTH          ; Check South
            jsr CheckAdj        ; ,,
            rol TMP             ; ,,
            lda #EAST           ; Check East
            jsr CheckAdj        ; ,,
            rol TMP             ; ,,
            lda #NORTH          ; Check North
            jsr CheckAdj        ; ,,
            rol TMP             ; TMP now contains the bitfield of directions
            jsr Coor2Ptr        ; Convert pointer to screen address
            lda TMP             ; Get character
            ldx #0              ; Store in screen address
            sta (PTR,x)         ; ,,
            lda #COL_ROAD       ; Set color of color address
            jsr SetColor        ; ,,
next_cell:  inc COOR_Y          ; Continue iterating across all characters in   
            lda COOR_Y          ;   the maze
            cmp #21             ;   ,,
            bne loop            ;   ,,
            lda #$00            ;   ,,
            sta COOR_Y          ;   ,,
            inc COOR_X          ;   ,,
            lda COOR_X          ;   ,,
            cmp #22             ;   ,,
            bne loop            ;   ,,
            pla                 ; Restore cursor coordinates
            sta COOR_Y          ; ,,
            pla                 ; ,,
            sta COOR_X          ; ,,
            rts                 ; ,,
            
; Check Adjacent
; For roads
; Carry is clear if the specified adjacent cell is a road
CheckAdj:   pha
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
            eor #$00000010
            jsr MoveCoor
is_road:    sec
            rts        

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
            
; Read the Joystick
; Return the direction in X
; 0=North, 1=East, 2=South, 3=West, 5=Fire, $ff=None
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
found_dir:  dex                 ; dex to maybe set zero flag
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
            tay                 ; Y is the row index
-loop:      lda #22             ; Drop to the next line...
            clc                 ; ,,
            adc PTR             ; ,,
            sta PTR             ; ,,
            bcc next_y          ; ,,
            inc PTR+1           ; ,,
next_y:     dey                 ; ...Y times
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
            cmp #21             ; Check Y coordinate for >20
            bcs out             ; ,,
            rts                 ; Return with carry clear (in)
out:        sec                 ; Return with carry set (out)
            rts                 ; ,,

; Place Character
; In A, at PTR location
; Then set color from Colors table
Place:      ldx #0              ; Place the item at the screen location
            sta (PTR,x)         ; ,,
            cmp #$10            ; Handle color lookup, with special case
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP ROUTINES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Set up hardware
Setup:      lda #30             ; Set background color
            sta SCRCOL          ; ,,
            lda #0              ; Turn off sound registers
            sta NOISE           ; ,, 
            sta VOICEL          ; ,,
            sta VOICEM          ; ,,
            sta VOICEH          ; ,,
            lda #$ff            ; Set custom character location
            sta VICCR5          ; ,,
            lda #$7f            ; Set DDR to read East
            sta VIA2DD          ; ,,
            lda #$80            ; Disable Commodore-Shift
            sta CASECT          ; ,,
            lda #$0a            ; Set volume
            sta VOLUME          ; ,,
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
            sei                 ; Install the custom ISR
            lda #<ISR           ; ,,
            sta CINV            ; ,,
            lda #>ISR           ; ,,
            sta CINV+1          ; ,,
            cli                 ; ,,
            ; Temporary charset transfer - Remove this when the program
            ; gets long enough to use padding instead
            ldy #$00
-loop:      lda CharSet,y
            sta $1c00,y  
            lda CharSet+256,y
            sta $1d00,y
            iny
            bne loop
            ; Fall through to InitGame

; Initialize Game            
InitGame:   lda #<Header        ; Show Board Header
            ldy #>Header        ; ,,
            jsr PRTSTR          ; ,,
            lda #11             ; Set initial coordinates
            sta COOR_X          ; ,,
            sta COOR_Y          ; ,,
            lda #0              ; Reset build index to Cancel
            sta BUILD_IX        ; ,,
            lda #<IN_YEAR       ; Set initial year
            sta YEAR            ; ,,
            lda #>IN_YEAR       ; ,,
            sta YEAR+1          ; ,,
            lda #<IN_TREASURY   ; Set initial treasury amount
            sta TREASURY        ; ,,
            lda #>IN_TREASURY   ; ,,
            sta TREASURY+1      ; ,,
            jsr DrawCursor      ; Show cursor
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; DATA TABLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Game Board Header
Header:     .asc $93,$1f,"PQRSTU!!!!VW!!XYZ[",$5c,"]",$5e,$5f
            .asc ":9999!;9999!<99>!=2021",$00

; Direction tables
JoyTable:   .byte $00,$04,$80,$08,$10,$20          ; Corresponding direction bit
DirTable:   .byte $01,$02,$04,$08                  ; Index to bit value

; Wind farm animation
WFAnim1:    .byte $10,$10,$28,$44
WFAnim2:    .byte $44,$28,$10,$10

; Colors of things
Colors:     .byte COL_ROAD,COL_UNOCC,COL_UNOCC,COL_WIND
            .byte COL_SCHOOL,COL_FIRE,COL_CLINIC,COL_PARK
            .byte COL_OCC,COL_OCC,COL_FOREST,COL_BURN            

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
; Roads $00 - $0f
CharSet:    .byte $00,$fe,$92,$92,$92,$92,$fe,$00  ; 0000 Parking Lot 
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
            .byte $54,$54,$c7,$00,$d6,$10,$c7,$44  ; 1111 Intersection   

; Title $10 - $1f (PQRSTU___VW.__XYZ[Lb]UpLeft)
            .byte $08,$eb,$d8,$bb,$08,$ff,$ff,$ff  ; ZE
            .byte $46,$db,$c7,$df,$5f,$ff,$ff,$ff  ; PT
            .byte $33,$6d,$6d,$6d,$73,$ff,$ff,$ff  ; O
            .byte $1c,$6b,$1b,$7b,$7c,$ff,$ff,$ff  ; PO
            .byte $dd,$5d,$5d,$5d,$c5,$ff,$ff,$ff  ; LI
            .byte $8f,$7f,$9f,$ef,$1f,$ff,$ff,$ff  ; S
            .byte $08,$e9,$09,$39,$08,$ff,$ff,$ff  ; 20
            .byte $42,$7a,$42,$4e,$42,$ff,$ff,$ff  ; 21
            .byte $f8,$fd,$fd,$fd,$f3,$ff,$ff,$ff  ; J
            .byte $ce,$b5,$86,$b7,$b4,$ff,$ff,$ff  ; AS
            .byte $33,$ed,$6d,$ad,$73,$ff,$ff,$ff  ; SO
            .byte $b7,$97,$a7,$b7,$b7,$ff,$ff,$ff  ; N
            .byte $e2,$f6,$f6,$f6,$cf,$ff,$ff,$ff  ; JU
            .byte $d8,$d7,$d9,$de,$31,$ff,$ff,$ff  ; US
            .byte $8b,$da,$da,$da,$da,$ff,$ff,$ff  ; TIA
            .byte $36,$d2,$14,$d6,$d6,$ff,$ff,$ff  ; AN
            
; Board Pieces $20 - $2f
            .byte $00,$00,$00,$00,$00,$00,$00,$00  ; $20 Space
            .byte $ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff  ; $21 Reverse Space
            .byte $00,$44,$28,$10,$28,$44,$00,$00  ; $22 Cancel
            .byte $00,$00,$fe,$00,$6c,$00,$fe,$00  ; $23 Road Placeholder       
            .byte $10,$38,$6c,$fe,$ce,$ca,$fa,$00  ; $24 Unocc. House
            .byte $00,$00,$ff,$d5,$ff,$d5,$df,$00  ; $25 Unocc. Business
WindFarm:   .byte $10,$10,$28,$44,$00,$10,$10,$00  ; $26 Wind Farm
            .byte $18,$10,$7c,$ee,$fe,$aa,$aa,$00  ; $27 School
            .byte $0e,$0a,$fe,$fe,$8a,$ae,$ae,$00  ; $28 Firehouse
            .byte $18,$92,$fe,$ee,$c6,$ee,$fe,$00  ; $29 Clinic
            .byte $fe,$ff,$ff,$1f,$11,$b1,$bb,$fe  ; $2a Park
            .byte $00,$88,$cc,$ee,$cc,$88,$00,$00  ; $2b End Turn
            .byte $10,$38,$6c,$fe,$ce,$ca,$fa,$00  ; $2c House
            .byte $00,$00,$ff,$d5,$ff,$d5,$df,$00  ; $2d Business
            .byte $10,$38,$7c,$10,$7c,$fe,$10,$00  ; $2e Forest
            .byte $08,$18,$24,$44,$52,$5a,$5a,$3c  ; $2f Burned Down
            
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
            .byte $c7,$e7,$ed,$83,$47,$83,$01,$ff  ; $3a Population
            .byte $c7,$93,$31,$39,$19,$93,$c7,$ff  ; $3b Money
            .byte $cf,$d7,$d7,$11,$5d,$5b,$03,$ff  ; $3c Satisfaction
            .byte $83,$ff,$ab,$ff,$ab,$ff,$ab,$ff  ; $3d Calendar
            .byte $ff,$ff,$bb,$f7,$ef,$df,$bb,$ff  ; $3e Percent Sign >
            .byte $fc,$c4,$c8,$c4,$f2,$d9,$0e,$04  ; $3f Cursor
