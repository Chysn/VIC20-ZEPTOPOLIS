* = $1800

NORTH       = 0
EAST        = 1
SOUTH       = 2
WEST        = 3
COOR_X      = $f9
COOR_Y      = $fa
PTR         = $fb
START_X     = $fd
START_Y     = $fe
STEP_COUNT  = $05
STEPS       = $06
PATTERN     = $07
LAST_DIR    = $08
CURR_DIR    = $09

SCREEN      = $1e00

Start:      lda #12             ; Set starting coordinate (sort of in the
            sta START_X         ;   middle of the screen)
            sta START_Y         ;   ,,
            lda #%00000000      ; Initialize pattern
            sta PATTERN         ; ,,
            
next_patt:  lda START_X         ; Reset coordinate to origin for each
            sta COOR_X          ;   new pattern
            lda START_Y         ;   ,,
            sta COOR_Y          ;   ,,
            jsr Coor2Ptr        ; Show the starting point
            ldx #0              ; ,,
            lda #$2a            ; ,,
            sta (PTR,x)         ; ,,
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
            lsr STEPS           ; Shift to the next step, two bits right
            lsr STEPS           ; ,,
            dec STEP_COUNT      ; Decrement step count
            bne next_step       ; Do next step if not done with pattern
patt_done:  ldx #0              ; Increment the pointer location to see how
            lda (PTR,x)         ;   many times this location is visited
            cmp #$20            ; If it's a space, then it needs to be
            bne num             ;   initialized to "0"
            lda #$30            ;   ,,
num:        tay                 ; Increment the count
            iny                 ; ,,
            tya                 ; ,,
            sta (PTR,x)         ; And save it to the screen
            inc PATTERN         ; Move to the next pattern
            bne next_patt       ; Until all patterns have been done
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
Coor2Ptr:   lda #<SCREEN        ; Start at the upper left corner of the screen
            sta PTR             ; ,,
            lda #>SCREEN        ; ,,
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