[org 0x0100]

jmp start
; ---------------------------------------VARIABLES--------------------------------------------------
;------FOR STATS---------------
ScoreDisplay: db 'SCORE : $'
Score: dw 0
FuelDisplay: db 'FUEL : $'
Fuel: dw 1600
Distance: dw 0
;-------START SCREEN-----------
PlayGame: db '1. Play$'
ExitGame: db '2. Exit$'
;-------COMPARATORS------------
Comparator: dw 0x0000
RowComparator : dw 0x0000
Color : dw 0x0000
HurdleType: dw 0x0000
RoadBumpFactor: dw 50
PointsCarFactor: dw 100
LeftCarMoveFactor: dw 50
Won: db 0
;-------CAR POSITIONS----------
LeftCarPosition: dw 24
MainCarPosition: dw 35
MainCarPrevPosition: dw 35
MainCarCoordinates : dw 0x0000
;-------COLOR PALLETE----------
RoadBump: dw 0x70EC
PointsCar: dw 0x7208
PlayerCar: dw 0x7408
;-------CONSTANTS------
RoadStart : dw 20

; ---------------------------------RESET VIDEO MEMORY ADDRESS--------------------------------------
ResetScreenPointer:
mov ax, 0xb800  ; Standard base address of display memory
mov es, ax   ; point es to video base
ret
; --------------------------------------DELAY--------------------------------------------------
delay:
push cx
mov cx, 0x0005 ; change the values  to increase delay time
delay_loop1:
push cx
mov cx, 0x6FFF
delay_loop2:
loop delay_loop2

pop cx
loop delay_loop1
pop cx
ret
; -----------------------------------------CLEAR SCREEN------------------------------------------
ClearScreen:
	push ax
	push di
	push es
	call ResetScreenPointer
	mov ax, 0720h    ; Black Background
	mov di, 0
	ClearLoop:
		mov [es:di], ax
		add di, 2
		cmp di, 4000
		jne ClearLoop
	pop es
	pop di
	pop ax
	ret	
; ------------------------------------CALCULATE SCREEN COORDINATES------------------------------------

; Function that Finds the Position on screen (ax(80) * bx(Row) + cx(Column))*2
FindPosition:

; Finding Position using formula (80 * Row + Column)*2
mov ax, 80  ; Store 80 in ax
imul ax,bx   ; Multiply ax with row value
add ax,cx  ; Add column value 
shl ax, 1  ; Multiply by 2
mov di,ax  ; Move Position to di

ret  ; Return
; -----------------------------------FILL COLOR ON SPECIFIC COLUMN-----------------------------------

; Function that displays the color [bp + 4] at location (di)
DisplayColor:
push dx
mov dx,[bp+4] ; Color passed
mov word [es:di], dx
add di,2
pop dx
ret
; --------------------------------------FILL COLOR ON ENTIRE COLUMN---------------------------------

FillColor:;---  This function prints the given input color on the entire given column of the screen

push bp    
mov bp,sp

; Save
push ax
push bx
push cx
push dx
push di
push si

call ResetScreenPointer

mov bx , 0x0 ; Start Row -> Start Point

mov dx, 25 ; End Row -> End Point

MoveVerticalDown:

mov cx,[bp+6]    ; Starting Column Value cx(Passed from BackGround function)

; Function that Finds the Position on screen (ax(80) * bx(Row) + cx(Column))*2
; Stores coordinates in di, modifies ax only
call FindPosition

inc bx

; Function that displays the color [bp + 4] at location (di)
; modifies di only
call DisplayColor

cmp bx,dx	; if Start point is not equal to end point, loop again

jne MoveVerticalDown

; Restore
pop si
pop di 
pop dx
pop cx 
pop bx 
pop ax

pop bp
ret ; sp -> Return address
; ----------------------------------------DISPLAY OBJECTS------------------------------------------------

; Function that displays different objects e.g. water, road, boundaries
DisplayObject:

RenderLoop:

push cx  ; sp + 6 -> cx

mov ax, [Color]   ; Colour to render
push ax   ; sp + 4 -> ax

call FillColor  ; sp+2 -> Return address

pop ax

pop cx

inc cx

cmp cx, word[Comparator] ; Ending Column location for Object
jne RenderLoop

ret
; --------------------------------------RENDER GRASS & ROADLINES------------------------------------

; Function that displays each grass or road line with gap of 5 rows
Display_Grass_RoadLines:

call ResetScreenPointer

Render_Grass_RoadLines:

; (ax(80) * bx(Row) + cx(Column))*2
; Stores coordinates in di, modifies ax only
call FindPosition

mov dx, [Color]   ; Passed Object (GRASS OR ROADLINES)
mov word [es:di], dx 

add bx,5   ; Gap

cmp bx, [RowComparator]
jb Render_Grass_RoadLines

ret
; ---------------------------------------INITIALIZE BACKGROUND-------------------------------------
BackGround:

;-------------------LEFT ROAD | FLAGS | LEFT CAR---------------------------------
mov cx, 1
mov word[Color],0x0FB3   ; White border line
mov word[Comparator], 2
call DisplayObject

mov cx,2 ; Starting Column Location for Left most silver line 
mov word[Color],0x7720   ; Silver colour
mov word[Comparator], 5
call DisplayObject ; Silver Line

mov cx, 2      ; End Blue Flag
mov bx, 0  
call FindPosition
mov word [es:di], 0x790D   ; Blue colour

mov cx, 2         ; Start Yellow Flag
mov bx, 24  
call FindPosition
mov word [es:di], 0x7E0D  ; Yellow colour

mov cx, 3         ; Start Left Car Place
mov bx, 24  
call FindPosition
mov word [es:di], 0x7408  ; Red colour

mov cx, 5
mov word[Color],0x0FB3   ; White border line
mov word[Comparator], 6
call DisplayObject

;---------------------LIGHT YELLOW GROUND--------------------------------------------------------

mov cx,7 ; Next Column location

mov word[Color],0x1EDB   ; Light Yellow colour

mov word[Comparator], 18

call DisplayObject ; Yellow Ground


;---------------------LEFT White COLOR BOUNDARY--------------------------------------------
mov cx, 18

mov word[Color],0x0FB3   ; White colour

mov word[Comparator], 19

call DisplayObject ; Left White Boundary


;---------------------SILVER ROAD---------------------------------------------------------
mov cx, 19

mov word[Color],0x7720   ; Silver colour

mov word[Comparator], 55

call DisplayObject ; Silver Road


;---------------------RIGHT White COLOR BOUNDARY--------------------------------------------
mov cx, 55

mov word[Color],0x0FB3   ; White colour

mov word[Comparator], 56

call DisplayObject ; Right White Boundary


;-------------------BLUE COLOR WATER------------------------------------------------------
mov cx, 56

mov word[Color], 0x3FB0   ; Light Blue colour

mov word[Comparator], 59

call DisplayObject ; Water


mov cx, 59

mov word[Color], 0x09B1             ; Dark Blue colour

mov word[Comparator], 63

call DisplayObject ; Water

;-------------------GREEN GRASS----------------------------------------------------------

mov word[Color], 0x6ADF

mov word[RowComparator], 25

mov bx,5 ; Row

mov cx, 9  ; Column

call Display_Grass_RoadLines

;--------------------

mov bx,2 ; Row

mov cx, 11  ; Column

call Display_Grass_RoadLines

;--------------------

mov bx,4 ; Row

mov cx, 13  ; Column

call Display_Grass_RoadLines

;--------------------

mov bx,1 ; Row

mov cx, 15  ; Column

call Display_Grass_RoadLines

;--------------------ROAD LINES------------------------------------------------------------

mov word[Color], 0x7FDD

mov bx,1 ; Row

mov cx, 36  ; Column

call Display_Grass_RoadLines

;-----------------------------

mov bx,2 ; Row

mov cx, 36  ; Column

call Display_Grass_RoadLines

ret
; ------------------------------------------DISPLAY STATS-----------------------------------------------

; Function to display score
PrintStats: 

push bp
mov bp, sp

call ResetScreenPointer

mov cx, [bp+4] ; Column
mov bx, [bp+6]; Row


call FindPosition

mov si, [bp+8] ; point si to string prompt

mov ah, 0x07 ; normal attribute fixed in al

; Display Score/Fuel Prompt
NextChar: 
mov al, [si] ; load next char of string
mov [es:di], ax ; show this char on screen
add di, 2 ; move to next screen location
add si, 1 ; move to next char in string
cmp byte[si], '$'
jne NextChar

; Display Value
; Storing decimal value
mov ax, [bp+10] ; load number in ax
cmp ax, 0
je NoValue
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: 
mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
mov word[Comparator], dx
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again

NextPosition: 
pop dx ; remove a digit from the stack
mov dh, 0x07 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop NextPosition ; repeat for all digits on stack

mov word[es:di], 0x0000  ; Adjust if base change e.g Value changes from thousands to hundreds 
                         ; Then we turn off the last zero
NoValue:						 
pop bp
ret
;----------------------------------------SOUNDS----------------------------------------
Sound:
mov al, 182         ; Prepare the speaker for the note.
out 43h, al         
mov ax, 4560        ; Frequency number (in decimal) for middle C.
out 42h, al         ; Output low byte.
mov al, ah          ; Output high byte.
out 42h, al 
in  al, 61h         ; Turn on note (get value from port 61h).
or  al, 00000011b   ; Set bits 1 and 0.
out 61h, al         ; Send new value.
mov bx, 30          ; Pause for duration of Approx second
.pause1:
mov cx, dx   ; dx holds the frequency of sound
.pause2:    
dec cx
jne .pause2
dec bx
jne .pause1
in  al, 61h         ; Turn off note (get value from
                    ;  port 61h).
and al, 11111100b   ; Reset bits 1 and 0.
out 61h, al         ; Send new value.
ret 

;-----------------------------------------COLLISION------------------------------------------
; dx holds the collision parameter
Collision:
cmp dx, [RoadBump]   ; Check if collided with a road bump
je GotHit
add word[PointsCarFactor], 70        ; Readjust factors that control generation
add word[RoadBumpFactor], 70
add word[Score], 50  ; increase points
mov dx, 100   ; low frequency
call Sound
jmp Back

GotHit:   ; if collided with a road bump
sub word[Fuel], 100   ; decrease fuel
mov dx, 5000  ; high frequency
call Sound
Back:
mov word[es:di], 0x7720  ; Silver Color , delete the object that collided
ret
; ----------------------------------------MOVE SCREEN DOWN--------------------------------------

MoveScreenDown:

call ResetScreenPointer

MoveRight:
mov bx , [RowComparator] ; Last Row

call FindPosition

mov dx, [es:di]

push dx ; Saving Last Row

mov dx, 0 ; End Row -> End Point

MoveDown:

; Function that Finds the Position on screen (ax(80) * bx(Row) + cx(Column))*2
; Stores coordinates in di, modifies ax only
call FindPosition ; Upper Location
mov si , di  ; si holds the Upper location a[i + 1] -> 1
dec bx ; Downward location
call FindPosition ; Now di holds the Downward location a[i] -> 0

; Move each Location Down
push dx
mov dx, [es:di] ; Store Upper location in dx , dx = a[i]
mov [es:si], dx ; Move Upper location to Downward location , a[i + 1] = a[i]
pop dx

cmp bx,dx	; if Start point is not equal to end point, loop again
jne MoveDown  ; i= n-1;i >= 0; i--

call FindPosition  ; Finding Coordinates of first row
pop dx  ; Restore saved Last Row
mov [es:di], dx ; First Row = Previous Last Row

; Remove Hurdle if no collision
cmp dx, [RoadBump]       ; if 22 was a bump
jne PointCar
cmp dx, [MainCarCoordinates]
je Collision 
mov word[es:di], 0x7720  ; Silver Color
jmp Resume

; Remove Point Car if no collision
PointCar:
cmp dx, [PointsCar]
jne Resume
cmp dx, [MainCarCoordinates]
je Collision
mov word[es:di], 0x7720  ; Silver Color

Resume:
inc cx
cmp cx, word[Comparator]
jne MoveRight

ret
; ----------------------------------------MOVE SCREEN UP-------------------------------------------------
MoveScreenUp:

call ResetScreenPointer

mov bx, [LeftCarPosition]  ; Moving last Position of Car to bx

; Function that Finds the Position on screen (ax(80) * bx(Row) + cx(Column))*2
; Stores coordinates in di, modifies ax only
call FindPosition ; Upper Location
mov si , di  ; si holds the Upper location a[i + 1] -> 1
dec bx ; Downward location
call FindPosition ; Now di holds the Downward location a[i] -> 0

; Move each Location Up by Swapping
push dx
mov dx, [es:di]
push dx
mov dx, [es:si] ; Store Upper location in dx , dx = a[i]
mov [es:di], dx ; Move Upper location to Downward location , a[i] = a[i+1]
pop dx
mov [es:si], dx
pop dx

mov word[LeftCarPosition], bx  ; Adjust Car Position

ret

;------------------------------------CREATE CARS & HURDLES--------------------------------------
GenerateRandomHurdles:   
mov ah, 00h  ; interrupts to get system time        
int 1ah      ; CX:DX now hold number of clock ticks since midnight      

mov  ax, dx
xor  dx, dx
mov  cx, 30    
div  cx       ; here dx contains the remainder of the division - from 0 to 29

mov cx, [RoadStart]   ; Starting Point of Main Road
add cx, dx   ; Add the random remainder we got to generate column

mov bx, 1      ; Row 
call FindPosition

mov dx, [HurdleType] ; Make this dynamic to create different hurdles
mov word[es:di], dx  

ret 
;------------------------------------MOVE LEFT CAR UPWARDS--------------------------------------
MoveLeftCar:
add word[LeftCarMoveFactor], 50  ; Readjust left car move factor
mov cx, 3   ; The column is constant for left car
call MoveScreenUp  ; Moves car up by one row
ret
;-------------------------------------PRINT MAIN CAR--------------------------------------------
PrintCar:
mov cx, [MainCarPosition]  ; Column
mov bx, 23  ; Row

call FindPosition      
mov word[es:di], 0x7408  ; Car Color

cmp cx, [MainCarPrevPosition] ; if prev position is same as current position then
je NoMove

; Get Car Coordinates by one row up for collision detection
mov bx, 22          
call FindPosition
mov dx, [es:di]
mov word[MainCarCoordinates], dx

; Clear previous car position
mov bx, 23          
mov cx,[MainCarPrevPosition]
call FindPosition
mov word[es:di], 0x7720  ; Silver Color

NoMove:
ret
;--------------------------------------MOVE MAIN CAR LEFT--------------------------------------------
; For Moving car left
MoveMainCarLeft:

cmp word[MainCarPosition], 20   ; Left Road Bound
jl Continue

; Middle row holds road lines that move down so avoiding them
AvoidMiddleLeft:   
cmp word[MainCarPosition], 37  ; Middle is 36 so we dec two times to get 35
jne NormalLeft
dec word[MainCarPosition]
dec word[MainCarPosition]
jmp Continue

NormalLeft:       
dec word[MainCarPosition]
jmp Continue

;----------------------------------------MOVE MAIN CAR RIGHT----------------------------------------
; For Moving car right
MoveMainCarRight:

cmp word[MainCarPosition], 53   ; Right Road Bound
jg Continue

; Middle row holds road lines that move down so avoiding them
AvoidMiddleRight:   
cmp word[MainCarPosition], 35 ; Middle is 36 so we inc two times to get 37
jne NormalRight
inc word[MainCarPosition]
inc word[MainCarPosition]
jmp Continue

NormalRight:       
inc word[MainCarPosition]
jmp Continue
;---------------------------------------INPUT DIRECTION----------------------------------------
; Get Input Key 'a' OR 'd'
GetMainCarInput:
; Input Direction Key
mov ah, 01
int 16h
jz Continue   ; if no key pressed continue

xor ah, ah   ; Remove last keystroke from buffer
int 16h

mov dx, [MainCarPosition]     ; Store Previous Position 
mov [MainCarPrevPosition], dx

; Move according to keystroke
cmp al, 61h ; ascii for 'a'   
je MoveMainCarLeft

cmp al, 64h ; ascii for 'd'
je MoveMainCarRight

Continue:
ret
;---------------------------------------MOVING BACKGROUND-------------------------------------------------
MoveBackground:

; Moves trees
mov cx, 9
mov word[Comparator], 19
mov word[RowComparator], 24
call MoveScreenDown  

; For Road Bumps
ChooseBump:
mov ax, [RoadBump]
mov word[HurdleType], ax
mov ax, [RoadBumpFactor]
cmp word[Score], ax   ; if score has reached the generation factor for road bump
jne ChoosePointCar    ; we only generate the road bump then
call GenerateRandomHurdles
add word[RoadBumpFactor], 70
jmp RoadLogic

; For Point Cars
ChoosePointCar:
mov ax, [PointsCar]
mov word[HurdleType], ax
mov ax, [PointsCarFactor]
cmp word[Score], ax   ; if score has reached the generation factor of point car 
jne RoadLogic         ; we only generate the point car then
call GenerateRandomHurdles
add word[PointsCarFactor], 70

RoadLogic:
mov cx, 19
mov word[Comparator], 36
mov word[RowComparator], 22
call MoveScreenDown  ; Moves Left Half of Road down

mov cx, 36
mov word[Comparator], 37
mov word[RowComparator], 24
call MoveScreenDown  ; Moves roadline

mov cx, 37
mov word[Comparator], 55
mov word[RowComparator], 22
call MoveScreenDown  ; Moves Right half of road down

call delay

mov ax, [LeftCarMoveFactor]   ; Animation for left side car
cmp word[Distance], ax
je MoveLeftCar

ret
;------------------------------------------SHOW STATS---------------------------------------
ShowStats:

; Adjusting Stats
dec word[Fuel]
inc word[Score]
inc word[Distance]

; For Display Stats
mov ax, [Score]   ; Value
push ax
mov si, ScoreDisplay  ; Prompt
push si
mov ax, 10   ; Row
push ax
mov ax, 65   ; Column
push ax
call PrintStats  ; Print Score
pop ax
pop ax
pop si
pop ax

mov ax, [Fuel]  ; Value
push ax
mov si, FuelDisplay  ; Prompt
push si
mov ax, 12   ; Row
push ax
mov ax, 65   ; Column
push ax
call PrintStats   ; Print Fuel
pop ax
pop ax
pop si
pop ax

ret
;-----------------------------------------START SCREEN------------------------------------------
StartScreen:

mov ax, 0   ; No Value
push ax
mov si, PlayGame  ; Prompt
push si
mov ax, 10   ; Row
push ax
mov ax, 35   ; Column
push ax
call PrintStats  ; Print Score
pop ax
pop ax
pop si
pop ax

mov ax, 0   ; No Value
push ax
mov si, ExitGame  ; Prompt
push si
mov ax, 14   ; Row
push ax
mov ax, 35   ; Column
push ax
call PrintStats  ; Print Score
pop ax
pop ax
pop si
pop ax

ret
;-----------------------------------------END SCREEN--------------------------------------------
; -----------------------------------------MAIN-------------------------------------------------
start:

call ClearScreen 

SelectOption: 

call StartScreen 

; Get Option Key
mov ah, 01
int 16h
jz SelectOption   ; if no key pressed 

xor ah, ah   ; Remove last keystroke from buffer
int 16h

; Choose According to keystroke
cmp al, 31h ; ascii for '1'   
je PlayChoosed

cmp al, 32h ; ascii for '2'
je Terminate

jmp SelectOption

PlayChoosed:

call BackGround 

; Game loop
mov_Again:

call PrintCar   ; Print Main Car

call ShowStats  ; Print Fuel & Score

call MoveBackground   ; Moving background
call GetMainCarInput  ; Input for car

cmp word[Fuel], 0     ; if no fuel left exit
jbe Terminate

cmp word[LeftCarPosition], 0   ; if left car reached end line, exit
jne mov_Again

Terminate:
call ClearScreen 
mov ax, 0x4c00 ; terminate program
int 21h 