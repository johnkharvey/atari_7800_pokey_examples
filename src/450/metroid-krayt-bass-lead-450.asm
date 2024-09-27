;	Armor Attack II

	processor 6502

;  HARDWARE EQUATES
VBLANK			equ	$01							;VERTICAL BLANK?
INPTCTL			equ $01							;Input control
AUDC0T			equ $15							;Audio Control Channel 0
AUDC1T			equ $16							;Audio Control Channel 1
AUDF0T			equ $17							;Audio Frequency Channel 0
AUDF1T			equ $18							;Audio Frequency Channel 1
AUDV0T			equ $19							;Audio Volume Channel 0
AUDV1T			equ $1A							;Audio Volume Channel 1
INPT0			equ $08							;Paddle Control Input 0
INPT1			equ $09							;Paddle Control Input 1
INPT2			equ $0A							;Paddle Control Input 2
INPT3			equ $0B							;Paddle Control Input 3
INPT4			equ $0C							;Player 0 Fire Button Input
INPT5			equ $0D							;Player 1 Fire Button Input

INPT4B			equ $08							;Player 0, Right Fire Button (D7=1 WHEN PUSHED)
INPT4A			equ $09							;Player 0, Left Fire Button  (D7=1 WHEN PUSHED)
INPT5B			equ $0A							;Player 1, Right Fire Button (D7=1 WHEN PUSHED)
INPT5A			equ $0B							;Player 1, Left Fire Button  (D7=1 WHEN PUSHED)

;	MARIA EQUATES
BACKGND			equ $20							;Background Color
Z0C1			equ $21							;Palette 0 - Color 1
Z0C2			equ $22							;Palette 0 - Color 2
Z0C3			equ $23							;Palette 0 - Color 3
WSYNC			equ $24							;Wait For Sync
Z1C1			equ $25							;Palette 1 - Color 1
Z1C2			equ $26							;Palette 1 - Color 2
Z1C3			equ $27							;Palette 1 - Color 3
MSTAT			equ $28							;Maria Status
Z2C1			equ $29							;Palette 2 - Color 1
Z2C2			equ $2A							;Palette 2 - Color 2
Z2C3			equ $2B							;Palette 2 - Color 3
DPPH			equ $2C							;Display List List Pointer High
Z3C1			equ $2D							;Palette 3 - Color 1
Z3C2			equ $2E							;Palette 3 - Color 2
Z3C3			equ $2F							;Palette 3 - Color 3
DPPL			equ $30							;Display List List Pointer Low
Z4C1			equ $31							;Palette 4 - Color 1
Z4C2			equ $32							;Palette 4 - Color 2
Z4C3			equ $33							;Palette 4 - Color 3
CHRBASE			equ $34							;Character Base Address
Z5C1			equ $35							;Palette 5 - Color 1
Z5C2			equ $36							;Palette 5 - Color 2
Z5C3			equ $37							;Palette 5 - Color 3
OFFSET			equ $38							;Unused - Store zero here
Z6C1			equ $39							;Palette 6 - Color 1
Z6C2			equ $3A							;Palette 6 - Color 2
Z6C3			equ $3B							;Palette 6 - Color 3
CTRL			equ $3C							;Maria Control Register
Z7C1			equ $3D							;Palette 7 - Color 1
Z7C2			equ $3E							;Palette 7 - Color 2
Z7C3			equ $3F							;Palette 7 - Color 3

;	CONSOLE SWITCHES & CONTROLLERS
SWCHA			equ $280						;P0, P1 Joystick Directional Input - RLDURLDU
SWCHB			equ $282						;Console Switches
CTLSWA			equ $281						;I/O Control for SCHWA
CTLSWB			equ $283						;I/O Control for SCHWB

;	SWCHB----------------------------------
;	Data Bit 	Switch 			Bit Meaning
;	D7 			P1 difficulty 	0 = amateur (B), 1 = pro (A)
;	D6 			P0 difficulty 	0 = amateur (B), 1 = pro (A)
;	D5/D4 		(not used)
;	D3 			color - B/W 	0 = B/W, 1 = color
;	D2 			(not used)
;	D1 			game select 	0 = switch pressed
;	D0 			game reset 		0 = switch pressed

POKEYADR       equ     $450
AUDF0          equ     POKEYADR+$00
AUDC0          equ     POKEYADR+$01
AUDF1          equ     POKEYADR+$02
AUDC1          equ     POKEYADR+$03
AUDF2          equ     POKEYADR+$04
AUDC2          equ     POKEYADR+$05
AUDF3          equ     POKEYADR+$06
AUDC3          equ     POKEYADR+$07
AUDCTL         equ     POKEYADR+$08    ; Audio Control
RANDOM         equ     POKEYADR+$0A    ; Random number (read-only)
SKCTLS         equ     POKEYADR+$0F    ; Serial Port control

	SEG.U	RAM

; <-=========================================================================->
	ORG	$0040									;TO $00FF

SOUNDZP			DS	2							;2 BYTES - SOUND ADDRESSES
RTLOCAL			DS	2							;2 BYTES - TIMER
SCRNSTAT		DS	1							;SCREEN STATE, OFF IF $FF
DLIFLG			DS	1							;FLAG FOR WHICH DLI WE ARE EXECUTING

; <-=========================================================================->
	ORG	$1800									;TO $203F

; <-=========================================================================->
	ORG	$2100									;TO $2180 (PART OF STACK WHICH STARTS AT $21FF BACKWARDS)

; <-=========================================================================->
	ORG	$2200									;TO $27FF

;	TUNE VARIABLES - $34 BYTES
TUNEAREA		EQU	$2200
TUNESOFF		EQU	TUNEAREA+$0000				;1 BYTE  - FLAG FOR ALL TUNES OFF
TUNNUM			EQU	TUNEAREA+$0001				;1 BYTE  - CURRENT TUNE NUMBER BEING PROCESSED
TUNCHANNEL		EQU	TUNEAREA+$0002				;1 BYTE  - CONVERTED FOR INDEX INTO POKEY CHANNEL REGISTERS
TUNFILLER		EQU	TUNEAREA+$0003				;1 BYTE  - NOT USED YET
TUNON			EQU	TUNEAREA+$0004				;4 BYTES - FLAG FOR TUNE PLAYING BY CHANNEL
NOTELO			EQU	TUNEAREA+$0008				;4 BYTES - INDEX INTO NOTE TABLE LOW
NOTEHI			EQU	TUNEAREA+$000C				;4 BYTES - INDEX INTO NOTE TABLE HI
CTLVOL			EQU	TUNEAREA+$0010				;4 BYTES - CONTROL / VOLUME VALUE BY CHANNEL
TUNINDEX		EQU	TUNEAREA+$0014				;4 BYTES - TUNE NUMBER BY CHANNEL
TUNFRM			EQU	TUNEAREA+$0018				;4 BYTES - NUMBER OF FRAMES (DURATION COUNT)
TUNPRIOR		EQU	TUNEAREA+$001C				;4 BYTES - TUNE PRIORITY BY CHANNEL
DURNLO			EQU	TUNEAREA+$0020				;4 BYTES - INDEX INTO DURATION TABLE LOW
DURNHI			EQU	TUNEAREA+$0024				;4 BYTES - INDEX INTO DURATION TABLE HI
DCYSTOR			EQU	TUNEAREA+$0028				;4 BYTES - FOR NOTE DECAY BY CHANNEL
FREQCNT			EQU	TUNEAREA+$002C				;4 BYTES - FOR NOTE DECAY BY CHANNEL
CTLSAV			EQU	TUNEAREA+$0030				;4 BYTES - TO SAVE THE CONTROL VALUE

; <-=========================================================================->

;	KERNEL DEFINITIONS
STACKPTR		EQU	$FF							;WHERE STACK IS ON PAGE 1
GRAPHON			EQU	$50							;CTL VALUE FOR 160A MODE GRAPHICS.
;GRAPHON		EQU	$5A							;CTL VALUE FOR 320B MODE GRAPHICS. $5E TO TURN KANGAROO MODE ON
GRAPHOFF		EQU	$7F							;CTL VALUE FOR GRAPHICS OFF

;	SCREEN MODE (THE VALUE TO BE BUT INTO 'CTRL')
;
;					 COLOR KILL (0=NORMAL COLOR, 1=NO COLOR BURST)
;					 |
;					 |DMA CONTROL (0=DO NOT USE, 1=DO NOT USE, 2=NORMAL DMA, 3=NO DMA)
;					 |||
;					 |||CHARACTER WIDTH (0=ONE BYTE, 1=TWO BYTES)
;					 ||||
;					 ||||BORDER CONTROL (0=BLACK, 1=BACKGROUND COLOR)
;					 |||||
;					 |||||KANGAROO MODE (0=TRANSPARENCY, 1=NO TRANSPARENCY)
;					 ||||||
;					 ||||||READ MODE (0=160X2 OR 160X4, 1=N/A, 2=320B OR 320D, 3=320A OR 320C)
;					 ||||||||
CTRLVAL			EQU	%01010000

	SEG ROM

	ORG $C000

;	BEGIN PROGRAM CODE ------------------------------------------------------------------------------------------------------->
START
	SEI
	CLD
	LDA #$17									;LOCK IN MARIA MODE
	STA INPTCTL
	LDA #GRAPHOFF
	STA CTRL
	LDA #$14									;SET JOYSTICK DIRECTION REGISTER
	STA CTLSWB									;TO SELECT TWO BUTTON MODE
	LDA #$00
	STA SWCHB
	STA OFFSET									;FOR FUTURE EXPANSION
	STA INPTCTL									;TO MAKE JOYSTICKS NOT FREEZE
	STA SWCHB
	LDX #STACKPTR
	TXS											;SET STACK POINTER

	JSR CLEARALL								;INITIAL CLEARING OF ALL MEMORY
	JSR OURINIT

	JMP MAINPROGRAM								;GO TO GAME

;	CLEARALL - CLEAR ALL OF RAM
CLEARALL
	LDA #$00
	TAX
CALOOP1
	STA $1800,X									;CLEAR $1800-$1FFF
	STA $1900,X
	STA $1A00,X
	STA $1B00,X
	STA $1C00,X
	STA $1D00,X
	STA $1E00,X
	STA $1F00,X
	STA $2200,X									;CLEAR $2200-$27FF
	STA $2300,X
	STA $2400,X
	STA $2500,X
	STA $2600,X
	STA $2700,X
	DEX
	CPX #$00
	BNE CALOOP1
	LDX #$3F
CALOOP2
	STA $2000,X									;CLEAR $2000-$203F
	DEX
	CPX #$00
	BPL CALOOP2
	LDX #$40
CALOOP3
	STA $00,X									;CLEAR ZERO PAGE ($40-$FF)
	INX
	CPX #$00
	BNE CALOOP3

	LDX #$7F
CALOOP4
	STA $2100,X
	DEX
	CPX #$00
	BPL CALOOP4
      
	RTS

WAITVBL
	BIT MSTAT									;IS VBLANK STARTED YET?
	BMI WAITVBL
WAITVBL2
	BIT MSTAT									;IS VBLANK STILL STARTED?
	BPL WAITVBL2
	RTS

;	INITIALIZE STATE OUR WAY - USED ALSO AFTER HSC STUFF
OURINIT
	JSR SCREENNO								;TURN SCREEN OFF

	LDA #>($C000)								;(DUMMY VALUE FOR THIS MUSIC DEMO)
	STA CHRBASE									;INSTALL THE CHARACTERS
	LDA #<(DLLISTNTSC)
	STA DPPL									;SET DPPL AND DPPH TO DLLIST
	LDA #>(DLLISTNTSC)
	STA DPPH
 
	LDA #$FF
	STA DLIFLG

	JSR SCREENON								;TURN SCREEN DISPLAY ON   
	RTS

;	TURN THE SCREEN OFF WITHOUT ZEROING THE SCREEN
SCREENNO
	JSR WAITVBL									;WAIT TILL VBLANK STARTED
	LDA #$7F									;TURN GRAPHICS OFF
	STA CTRL
	LDA #$00
	STA SCRNSTAT								;ZERO SOME STATE
	RTS

;	TURN THE SCREEN ON
SCREENON
	LDA SCRNSTAT								;SEE IF SCREEN WAS EVEN OFF
	BNE SOOUT

	JSR WAITVBL
	INC SCRNSTAT								;SAY THE SCREEN IS ON

	LDA #GRAPHON
	STA CTRL									;TURN GRAPHICS ON
SOOUT
	RTS

;	MAIN LOOP BEGIN ---------------------------------------------------------------------------------------------------------->

MAINPROGRAM
	JSR PLAYTUNE								;START THE TUNE PLAYING
MAINLOOP
	JSR WAITVBL									;WAIT FOR VBLANK
DOCONSOLE
	JSR SEEBALL									;CHECK CONSOLE SWITCHES
	JMP MAINLOOP

PLAYTUNE 
	LDA #$50
	STA AUDCTL
	
	;LDA #$00
    ;JSR DOTUNE
	;LDA #$01
	;JSR DOTUNE
	;LDA #$02
	;JSR DOTUNE
	;LDA #$03
	;JSR DOTUNE
	LDA #$04
	JSR DOTUNE
	LDA #$05
	JSR DOTUNE
	LDA #$06
	JSR DOTUNE
	LDA #$07
	JSR DOTUNE
	RTS

;	CONSOLE BUTTONS ---------------------------------------------------------------------------------------------------------->
          
;	SEEBALL - CHECK TO SEE IF ANY CONSOLE BUTTONS WERE HIT
;	INPUT: NONE
;	USES: A
SEEBALL
	LDA SWCHB
	AND #$01									;ISOLATE 'RESET'
	EOR #$01									;INVERT IT
	BEQ SBEXIT
	JSR PLAYTUNE
SBEXIT
	RTS

;	SOUND -------------------------------------------------------------------------------------------------------------------->

;	TUNES - THESE ROUTINES HANDLE ALL OF THE SOUNDS

;	RESET POKEY CHIP
RSTPOKEY:
	LDX #$0F 
	LDA #$00
RSTLP:
	STA AUDF0,X									;CLEAR POKEY REGISTERS
	DEX 
	BPL RSTLP 
	LDA #$03
	STA SKCTLS									;TURN IT ON
	RTS

;	TURN OFF ALL SOUNDS
STOPTUN
	LDA #$00
	STA AUDC0
	STA AUDC1
	STA AUDC2
	STA AUDC3
	LDA #$01
	STA TUNESOFF
	RTS

;	TURN ON ALL SOUNDS
STARTTUN
	LDA #$00
	STA TUNESOFF
	RTS

;	THIS ROUTINE ERASES ALL TUNES
;	X AND Y ARE PRESERVED
CLEARTUN
	TXA											;STACK REGISTERS
	PHA
	TYA
	PHA
	LDX #$03
CTLOOP
	JSR ENDTUNE									;ERASE CURRENT TUNE
	DEX
	BPL CTLOOP
	PLA											;UNSTACK REGISTERS
	TAY
	PLA
	TAX
	RTS

;	ROUTINE TO KILL A PARTICULAR TUNE - IF IT IS RUNNING
;	INPUT: TUNE NUMBER IN A
;	X AND Y ARE PRESERVED
KILLTUNE
	STA TUNNUM									;SAVE IT
	TXA											;STACK REGISTERS
	PHA
	TYA
	PHA
	LDX #$03									;CHECK ALL CHANNELS
KTLOOP
	LDA TUNON,X									;SEE IF CHANNEL ON
	BEQ KTNEXT
	LDA TUNINDEX,X								;SEE IF HAS TUNE TO BE KILLED
	CMP TUNNUM
	BNE KTNEXT
	JSR ENDTUNE									;ERASE IT
KTNEXT
	DEX
	BPL KTLOOP
	PLA											;UNSTACK REGISTERS
	TAY
	PLA
	TAX
	RTS

;	THIS ROUTINE CLEARS OUT A TUNE CHANNEL
;	INPUT: X IS CHANNEL
ENDTUNE
	LDA #$00
	STA TUNON,X									;INDICATE CHANNEL CLEAR
	STA TUNINDEX,X								;CLEAR TUNE INDEX
	STA DCYSTOR,X
	STA FREQCNT,X
	RTS

;	THIS ROUTINE ENTERS A TUNE INTO ONE OF THE SOUND CHANNELS IF IT CAN
;	INPUT:  TUNE NUMBER IN A
;	X AND Y ARE PRESERVED
DOTUNE
	STA TUNNUM									;SAVE IT
	;LDA AUTOPLAY								;IF IN AUTOPLAY - NO SOUND
	;BEQ DTCONT
	;RTS
DTCONT
	TXA											;STACK REGISTERS
	PHA
	TYA
	PHA
	LDY TUNNUM									;SEE IF WE CAN PUT IT IN
	LDX CHANNLTBL,Y								;GET WHAT CHANNEL TO TRY TO PUT IT IN
	LDA TUNON,X									;SEE IF CHANNEL OPEN
	BEQ DTDOIT
	LDA PRIRTYTBL,Y								;SEE IF WE CAN BUMP CHANNEL
	CMP TUNPRIOR,X
	BMI DTOUT
DTDOIT
	LDA TUNNUM
	TAY											;PUT TUNE IN Y
	STA TUNINDEX,X								;SET THE TUNE INDEX
	LDA #$00									;TURN TUNE OFF WHILE CHANGING IT
	STA TUNON,X
	LDA CNTVOLTBL,Y								;GET TUNE CONTROL / VOLUME
	STA CTLVOL,X
	STA CTLSAV,X								;USED TO RESTORE AFTER DECAY
	LDA NOTETBLLO,Y								;GET TUNE FREQUENCY LOW ADDRESS
	STA NOTELO,X
	LDA NOTETBLHI,Y								;GET TUNE FREQUENCY HIGH ADDRESS
	STA NOTEHI,X
	LDA DURNTBLLO,Y								;GET TUNE DURATION LOW ADDRESS
	STA DURNLO,X
	LDA DURNTBLHI,Y								;GET TUNE DURATION HIGH ADDRESS
	STA DURNHI,X
	LDA PRIRTYTBL,Y								;SET PRIORITY
	STA TUNPRIOR,X
	LDA #$01									;SET FREQ, CTL, AND VOL TO BE SET
	STA TUNFRM,X
	STA TUNON,X									;AND TURN THE TUNE ON!
DTOUT
	PLA											;UNSTACK REGISTERS
	TAY
	PLA
	TAX
	RTS

;  THIS ROUTINE IS CALLED EVERY VBLANK TO TAKE CARE OF TUNES
;  REGISTERS ARE NOT SAVED
TUNER
	LDX #$03									;FOUR TUNES CHANNELS, START WITH LAST
	LDA TUNESOFF
	BEQ TUNLOOP
	RTS
TUNLOOP
	TXA
	ASL
	STA TUNCHANNEL								;CHANNELS ARE OFFSET 0, 2, 4, 6 IN THE POKEY
	TAY
	LDA TUNON,X
	BNE TUNBODY
	STA AUDC0,Y									;CHANNEL OFF - MAKE SURE VOLUME OFF
	JMP TUNNEXT
TUNBODY
	DEC TUNFRM,X								;SEE IF WE'RE DONE WITH THIS SOUND
	BEQ TUNFRMFRQ								;YES - GET NEXT NOTE / DURATION
	DEC FREQCNT,X                               ;REDUCE THE NUMBER OF FRAMES UNTIL NEXT DECAY
	BEQ DEC_VOLUME
	JMP TUNNEXT                                 ;IF WE AREN'T AT ZERO YET, DON'T DECAY

DEC_VOLUME
	LDA DCYSTOR,X                               ;RESET THE DECAY FOR THE NEXT COUNT
	STA FREQCNT,X

	LDA CTLVOL,X                                ;IF VOLUME ALREADY 0 DO NOT DECREMENT
	AND #$0F
	BEQ TUNNEXT
	
	DEC CTLVOL,X                                ;DECREMENT THE VOLUME
	LDA CTLVOL,X
	LDY TUNCHANNEL
	;AND MUTEMASK,Y
	STA AUDC0,Y
	JMP TUNNEXT                                 ;GO TO NEXT CHANNEL
TUNFRMFRQ
	LDA DURNLO,X								;GET THE CURRENT DURATION
	STA SOUNDZP
	LDA DURNHI,X
	STA SOUNDZP+1
	LDY #$00
	LDA (SOUNDZP),Y
	BEQ TUNEND									;$00 IN DURATION MEANS TUNE IS OVER
	STA TUNFRM,X
	TAY
	LDA DECAYTBL,Y								;GET THE CURRENT DECAY VALUE INDEXED BY NOTE
	STA DCYSTOR,X								;STORE IT HERE TO REFRESH THE COUNTER FOR THE NEXT DECAY
	STA FREQCNT,X								;ALSO STORE IT HERE FOR TUNER
	LDY TUNCHANNEL
	LDA CTLSAV,X
	STA CTLVOL,X								;RESTORE THE ORIGINAL CONTROL AND VOLUME FOR NEXT NOTE
	STA AUDC0,Y
	LDA NOTELO,X								;GET THE CURRENT FREQUENCY
	STA SOUNDZP
	LDA NOTEHI,X
	STA SOUNDZP+1
	LDY #$00
	LDA (SOUNDZP),Y
	LDY TUNCHANNEL
	STA AUDF0,Y
	INC NOTELO,X
	BNE TUNNEXTNOTE
	INC NOTEHI,X
TUNNEXTNOTE
	INC DURNLO,X
	BNE TUNNEXT
	INC DURNHI,X
TUNNEXT
	DEX
	CPX #$00
	BMI TUNEXIT
	JMP TUNLOOP
TUNEXIT
	RTS

TUNEND
	LDA NOTELO,X								;SEE IF WE SHOULD REPEAT
	STA SOUNDZP
	LDA NOTEHI,X
	STA SOUNDZP+1
	LDY #$00
	LDA (SOUNDZP),Y
	BMI TUNRESTART
	JSR ENDTUNE
	JMP TUNNEXT
TUNRESTART
	LDA TUNINDEX,X								;GET TUNE NUMBER
	TAY
	LDA CNTVOLTBL,Y								;GET TUNE CONTROL / VOLUME
	STA CTLVOL,X
	STA CTLSAV,X								;USED TO RESTORE AFTER DECAY
	LDA NOTETBLLO,Y								;GET TUNE FREQUENCY LOW ADDRESS
	STA NOTELO,X
	LDA NOTETBLHI,Y								;GET TUNE FREQUENCY HIGH ADDRESS
	STA NOTEHI,X
	LDA DURNTBLLO,Y								;GET TUNE DURATION LOW ADDRESS
	STA DURNLO,X
	LDA DURNTBLHI,Y								;GET TUNE DURATION HIGH ADDRESS
	STA DURNHI,X
	LDY TUNCHANNEL
	LDA CTLVOL,X
	STA AUDC0,Y									;STORE THE CONTROL / VOLUME IN THE CHANNEL
	LDA #$01									;SET FREQ, CTL, AND VOL TO BE SET
	STA TUNFRM,X
	JMP TUNNEXT

;	DATA FOR TUNES

;	CNTVOLTBL - HIGH NYBBLE FOR VOICE CONTROL (7654XXXX), AND LOW NYBBLE FOR VOLUME (XXXX3210)
CNTVOLTBL:
	.byte $00,$a7,$00,$00 	;$B7 silences volume output on channel 1 to stop interference
	.byte $00,$a7,$a7,$a3,$AB,$AB,$AB,$AB	;$EB,$4B,$27

;	PRIORITY TABLE
PRIRTYTBL:
	.byte $02,$02,$02,$02,$00,$02,$02,$02		
	.byte $00,$02,$02,$02,$00,$02,$02,$02
	
;	CHANNEL TABLE ($00 - $03 ARE VALID)
CHANNLTBL:
	.byte $00,$01,$02,$03,$00,$01,$02,$03 ; must set $00 and $01 to use 16-bit combined channel
	.byte $00,$00,$00,$00,$00,$00,$00,$00

;	DECAY TABLE - DEFINES DECAY COUNT INDEXED BY NOTE DURATION
DECAYTBL:
	.byte $01,$01,$01,$01,$02,$02,$02,$02
	.byte $02,$03,$03,$03,$03,$03,$03,$03
	.byte $04,$04,$04,$04,$04,$04,$04,$04
	.byte $05,$05,$05,$05,$05,$05,$05,$05
	.byte $05,$05,$06,$06,$06,$06,$06,$06
	.byte $06,$06,$06,$06,$07,$07,$07,$07
	.byte $07,$07,$07,$07,$07,$07,$07,$07
	.byte $07,$08,$08,$08,$08,$08,$08,$08

;	FREQUENCY TABLE
NOTETBLHI:
	.byte >(TUNE00),>(TUNE01),>(TUNE02),>(TUNE03)
	.byte >(TUNE04),>(TUNE05),>(TUNE06),>(TUNE07)
	.byte >(TUNE08),>(TUNE09),>(TUNE0A),>(TUNE0B)
	.byte >(TUNE0C),>(TUNE0D),>(TUNE0E),>(TUNE0F)

NOTETBLLO:
	.byte <(TUNE00),<(TUNE01),<(TUNE02),<(TUNE03)
	.byte <(TUNE04),<(TUNE05),<(TUNE06),<(TUNE07)
	.byte <(TUNE08),<(TUNE09),<(TUNE0A),<(TUNE0B)
	.byte <(TUNE0C),<(TUNE0D),<(TUNE0E),<(TUNE0F)

;	DURATION TABLE
DURNTBLHI:
	.byte >(DURN00),>(DURN01),>(DURN02),>(DURN03)
	.byte >(DURN04),>(DURN05),>(DURN06),>(DURN07)
	.byte >(DURN08),>(DURN09),>(DURN0A),>(DURN0B)
	.byte >(DURN0C),>(DURN0D),>(DURN0E),>(DURN0F)

DURNTBLLO:
	.byte <(DURN00),<(DURN01),<(DURN02),<(DURN03)
	.byte <(DURN04),<(DURN05),<(DURN06),<(DURN07)
	.byte <(DURN08),<(DURN09),<(DURN0A),<(DURN0B)
	.byte <(DURN0C),<(DURN0D),<(DURN0E),<(DURN0F)

;TEMPOS
T1	=	$03
T2      =       (T1*2)
T3      =       (T1*3)
T4      =       (T1*4)
T5      =       (T1*5)
T6      =       (T1*6)
T7      =       (T1*7)
T8      =       (T1*8)
T9      =       (T1*9)
TA      =       (T1*10)
TB      =       (T1*11)
TC      =       (T1*12)
TD      =       (T1*13)
TE      =       (T1*14)
TF      =       (T1*15)
T0      =       (T1*16)



;	TUNE 0 - COMBINED 16-BIT CHANNEL (low order bytes stored in AUDF0)

TUNE00:

  
	
	.byte $2f,0,$49,$dd,0,$b2,$de,0,$c8,$49,0,$f8
	.byte $2f,0,$49,$dd,0,$b2,$de,0,$c8,$49,0,$f8
	.byte $2f,0,$49,$dd,0,$b2,$de,0,$c8,$49,0,$f8
	.byte $2f,0,$49,$dd,0,$b2,$de,0,$c8,$49,0,$f8
	
	.byte $cf,0,$c8,$2f,0,$49,$de,0,$c8,$dd,0,$2f
	.byte $cf,0,$c8,$2f,0,$49,$de,0,$c8,$dd,0,$2f
	
	.byte $cf,0,$49,$cf,$49,$cf,$49,$F8,$2F,$F8,$2F,$F8,$2F
	.BYTE $dd,$b2,$dd,$b2,$dd,$b2,$dd,$72,$de,$72,$21,$72
	.byte $cf,0,$49,$cf,$49,$cf,$49,$F8,$2F,$F8,$2F,$F8,$2F
	.BYTE $dd,$b2,$dd,$b2,$dd,$b2,$dd,$72,$de,$72,$21,$72
	
	.byte $cf,0,$49,$cf,$49,$cf,$49,$F8,$2F,$F8,$2F,$F8,$2F
	.BYTE $dd,$b2,$dd,$b2,$dd,$b2,$dd,$72,$de,$72,$21,$72
	.byte $cf,0,$49,$cf,$49,$cf,$49,$F8,$2F,$F8,$2F,$F8,$2F
	.BYTE $dd,$b2,$dd,$b2,$dd,$b2,$dd,$72,$de,$72,$21,$72
	
	.byte $de,0,$cf,$dd,$21,$e0,$21,$6c,$e0,$dd,$cf,$de,$21
	.byte $eb,$6c,$e0,$dd,$cf,$de,$2f,$cf,$dd,$e0,$94,0
	.byte $de,$cf,$dd,$21,$e0,$21,$6c,$e0,$dd,$cf,$de,$21
	.byte $eb,$6c,$e0,$dd,$cf,$de,$2f,$cf,$dd,$e0,$94,0
	
	.byte $eb,0,$e4,0,$6c,0,$94,0,$6c,0,$e4,0
	.byte $eb,0,$e4,0,$6c,0,$94,0,$6c,0,$e4,0
	.byte $eb,0,$e4,0,$6c,0,$94,0,$6c,0,$e4,0
	.byte $eb,0,$e4,0,$6c,0,$94,0,$6c,0,$e4,0
	
	
	
	
	
	
	
	.byte 0

    
	
	
	
DURN00:

	 
	 
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	   .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	    
	  
	  
	 
	 
	 
	 
	 
	 .byte 0
	
TUNE01:
	
	
	 
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	
	.byte $11,0,$17,$15,0,$1c,$12,0,$17,$0f,0,$15
	.byte $11,0,$17,$15,0,$1c,$12,0,$17,$0f,0,$15
	
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	
	.byte $12,0,$11,$0f,$0e,$0b,$0e,$09,$0b,$0f,$11,$12,$0e
	.byte $07,$09,$0b,$0f,$11,$12,$15,$11,$0f,$0b,$0a,0
	.byte $12,$11,$0f,$0e,$0b,$0e,$09,$0b,$0f,$11,$12,$0e
	.byte $07,$09,$0b,$0f,$11,$12,$15,$11,$0f,$0b,$0a,0
	
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	
	
	
	
	
	
	
	
	
	.byte 0
	
	

    
    
	
	
	
	
	

   
	
	
DURN01:
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	   .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	 
	 .byte 0
	
	
	
	
	

TUNE02:
	 .byte $2d,0,$47,$db,0,$b0,$dc,0,$c6,$47,0,$f6
	.byte $2d,0,$47,$db,0,$b0,$dc,0,$c6,$47,0,$f6
	.byte $2d,0,$47,$db,0,$b0,$dc,0,$c6,$47,0,$f6
	.byte $2d,0,$47,$db,0,$b0,$dc,0,$c6,$47,0,$f6
	
	.byte $cd,0,$c6,$2d,0,$47,$dc,0,$c6,$db,0,$2d
	.byte $cd,0,$c6,$2d,0,$47,$dc,0,$c6,$db,0,$2d
	
	.byte $cd,0,$47,$cd,$47,$cd,$47,$F6,$2d,$F6,$2d,$F6,$2d
	.BYTE $db,$b0,$db,$b0,$db,$b0,$db,$70,$dc,$70,$1f,$70
	.byte $cd,0,$47,$cd,$47,$cd,$47,$F6,$2d,$F6,$2d,$F6,$2d
	.BYTE $db,$b0,$db,$b0,$db,$b0,$db,$70,$dc,$70,$1f,$70
	
	.byte $cd,0,$47,$cd,$47,$cd,$47,$F6,$2d,$F6,$2d,$F6,$2d
	.BYTE $db,$b0,$db,$b0,$db,$b0,$db,$70,$dc,$70,$1f,$70
	.byte $cd,0,$47,$cd,$47,$cd,$47,$F6,$2d,$F6,$2d,$F6,$2d
	.BYTE $db,$b0,$db,$b0,$db,$b0,$db,$70,$dc,$70,$1f,$70
	
	.byte $dc,0,$cd,$db,$1f,$de,$1f,$6a,$de,$db,$cd,$dc,$1f
	.byte $ed,$6a,$de,$db,$cd,$dc,$2d,$cd,$db,$de,$92,0
	.byte $dc,$cd,$db,$1f,$de,$1f,$6a,$de,$db,$cd,$dc,$1f
	.byte $ed,$6a,$de,$db,$cd,$dc,$2d,$cd,$db,$de,$92,0
	
	.byte $ed,0,$e2,0,$6a,0,$92,0,$6a,0,$e2,0
	.byte $ed,0,$e2,0,$6a,0,$92,0,$6a,0,$e2,0
	.byte $ed,0,$e2,0,$6a,0,$92,0,$6a,0,$e2,0
	.byte $ed,0,$e2,0,$6a,0,$92,0,$6a,0,$e2,0
	
	
	
	 	.byte 0
	
DURN02:
   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   
	    .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	    .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 0
	
	
	
	
	
	

;
;	TUNE 1 - COMBINED 16-BIT CHANNEL (High order bytes stored in AUDF1)

TUNE03:
   .byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	.byte $15,0,$1c,$0f,0,$1a,$12,0,$17,$1c,0,$1d
	
	.byte $11,0,$17,$15,0,$1c,$12,0,$17,$0f,0,$15
	.byte $11,0,$17,$15,0,$1c,$12,0,$17,$0f,0,$15
	
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	.byte $11,0,$1c,$11,$1c,$11,$1c,$0e,$15,$0e,$15,$0e,$15
	.BYTE $0f,$1a,$0f,$1a,$0f,$1a,$0f,$16,$12,$16,$0e,$16
	
	.byte $12,0,$11,$0f,$0e,$0b,$0e,$09,$0b,$0f,$11,$12,$0e
	.byte $07,$09,$0b,$0f,$11,$12,$15,$11,$0f,$0b,$0a,0
	.byte $12,$11,$0f,$0e,$0b,$0e,$09,$0b,$0f,$11,$12,$0e
	.byte $07,$09,$0b,$0f,$11,$12,$15,$11,$0f,$0b,$0a,0
	
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	.byte 7,0,8,0,9,0,10,0,9,0,8,0
	
	
	
	
	
	
	
	
	.byte 0

	
DURN03:	
	
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   
	   .byte 31,1,16,31,1,16,31,1,16,31,1,16
	  .byte 31,1,16,31,1,16,31,1,16,31,1,16
	   
	   .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	   .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 7,1,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	
	.byte 0
	 
  
   
  
   
   
   
   
   
   
   
	
	
	

;	TUNE 4 - bass
TUNE04:

  
	
	.byte $64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f
	.byte $64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f
	.byte $64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f
	.byte $64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f,$64,$2f,$2f
	
	.byte $6b,$b2,$b2,$6b,$b2,$b2,$96,$c8,$c8,$96,$c8,$c8
	.byte $6b,$b2,$b2,$6b,$b2,$b2,$96,$c8,$c8,$96,$c8,$c8
	
	.byte $64,$2f,$94,$c4,$de,$6c,$03,$Fe,$fb,$99,$49,$21
	.byte $64,$2f,$94,$c4,$de,$6c,$03,$Fe,$fb,$99,$49,$21
	.byte $64,$2f,$94,$c4,$de,$6c,$03,$Fe,$fb,$99,$49,$21
	.byte $64,$2f,$94,$c4,$de,$6c,$03,$Fe,$fb,$99,$49,$21
	
	.byte $64,$49,$b2,$49,$6b,$a5,$c0,$a5
	.byte $64,$49,$b2,$49,$6b,$a5,$c0,$a5
	
	
	
	.byte $d0,0
	.byte $d0,0
	
	
	
	
	
	
	
	.byte 0

    
	
	
	
DURN04:

	 
	 
	   .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	 .byte 32,16,32,16,32,16,32,16
	 .byte 32,16,32,16,32,16,32,16
	  
	 
	  
	 .byte 176,16
	 .byte 176,16
	    
	  
	  
	 
	 
	 
	 
	 
	 .byte 0
	
TUNE05:
	
	
	 
	.byte $2a,$15,$15,$2a,$15,$15,$2a,$15,$15,$2a,$15,$15
	.byte $2a,$15,$15,$2a,$15,$15,$2a,$15,$15,$2a,$15,$15
	.byte $2a,$15,$15,$2a,$15,$15,$2a,$15,$15,$2a,$15,$15
	.byte $2a,$15,$15,$2a,$15,$15,$2a,$15,$15,$2a,$15,$15
	
	.byte $35,$1a,$1a,$35,$1a,$1a,$2f,$17,$17,$2f,$17,$17
	.byte $35,$1a,$1a,$35,$1a,$1a,$2f,$17,$17,$2f,$17,$17
	
	.byte $2a,$15,$0a,$25,$12,$09,$28,$13,$09,$38,$1c,$0e
	.byte $2a,$15,$0a,$25,$12,$09,$28,$13,$09,$38,$1c,$0e
	.byte $2a,$15,$0a,$25,$12,$09,$28,$13,$09,$38,$1c,$0e
	.byte $2a,$15,$0a,$25,$12,$09,$28,$13,$09,$38,$1c,$0e
	
	.byte $2a,$1c,$1a,$1c,$36,$23,$1f,$23
	.byte $2a,$1c,$1a,$1c,$36,$23,$1f,$23
	
	.byte $54,0
	.byte $54,0
	
	
	

    
    
	.byte 0
	
	
	
	

   
	
	
DURN05:
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	 .byte 16,16,16,16,16,16,16,16,16,16,16,16
	  
	  .byte 32,16,32,16,32,16,32,16
	 .byte 32,16,32,16,32,16,32,16
	  
	 .byte 176,16
	 .byte 176,16
	    
	  
	  
	 
	 
	 
	 
	 
	 .byte 0
	
	
	
	
	

TUNE06:
	 .byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	
	.byte 81,108,96,128,85,108,72,96
	.byte 81,108,96,128,85,108,72,96
	
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	
	.byte 85,81,72,64,53,64,42,53,72,81,85,64
	.byte 35,42,53,72,81,85,96,81,72,53,47,0
	.byte 85,81,72,64,53,64,42,53,72,81,85,64
	.byte 35,42,53,72,81,85,96,81,72,53,47,0
	
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	
	
	
	 	.byte 0
	
DURN06:
   .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  
	  .byte 32,16,32,16,32,16,32,16
	   .byte 32,16,32,16,32,16,32,16
	   
	   .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	 .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 0
	
	
	
	
	
	

;
;	TUNE 1 - COMBINED 16-BIT CHANNEL (High order bytes stored in AUDF1)

TUNE07:

	.byte 0
	
   .byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	.byte 96,128,72,121,85,108,128,136
	
	.byte 81,108,96,128,85,108,72,96
	.byte 81,108,96,128,85,108,72,96
	
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	.byte 81,128,81,128,81,128,68,96,68,96,68,96
	.BYTE 72,120,72,120,72,120,72,102,85,102,64,102
	
	.byte 85,81,72,64,53,64,42,53,72,81,85,64
	.byte 35,42,53,72,81,85,96,81,72,53,47,0
	.byte 85,81,72,64,53,64,42,53,72,81,85,64
	.byte 35,42,53,72,81,85,96,81,72,53,47,0
	
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40,0
	.byte 35,0,40,0,42,0,47,0,42,0,40
	
	
	
	
	
	
	
	
	.byte 0

	
DURN07:	
		.byte 8
	
	   .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  .byte 32,16,32,16,32,16,32,16
	  
	  .byte 32,16,32,16,32,16,32,16
	   .byte 32,16,32,16,32,16,32,16
	   
	   .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	 .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8,8
	  .byte 8,8,8,8,8,8,8,8,8,8,8
	
	.byte 0
	 

;
;	TUNE 8 - THROW JEWEL
TUNE08:
	.byte $2D,$00,$00
DURN08:
	.byte $01,$01,$00

;
;	TUNE 9 - HEALTH BOOST
TUNE09:
	.byte $5B,$3C,$5B,$48,$00
DURN09:
	.byte $0A,$0A,$0A,$0A,$00

;
;	TUNE A - ENEMY HIT
TUNE0A:
	.byte $3C,$2F,$28,$2F,$00
DURN0A:
	.byte $05,$05,$0A,$05,$00

;
;	TUNE B - PICKED UP WHITE JEWEL
TUNE0B:
	.byte $20,$1B,$00
DURN0B:
	.byte $05,$05,$00

;
;	TUNE C - PICKED UP GREEN JEWEL
TUNE0C:
	.byte $1D,$18,$00
DURN0C:
	.byte $05,$05,$00

;
;	TUNE D - PICKED UP TAN JEWEL
TUNE0D:
	.byte $1A,$15,$10,$00
DURN0D:
	.byte $05,$05,$05,$00

;
;	TUNE E - PICKED UP RED JEWEL
TUNE0E:
	.byte $17,$12,$0F,$00
DURN0E:
	.byte $05,$05,$05,$00

;
;	TUNE F - 
TUNE0F:
	.byte $2D,$00
DURN0F:
	.byte $02,$00

;	DLI, DL, AND DLLS -------------------------------------------------------------------------------------------------------->

;	KERNAL - MAINTAIN THE ON-SCREEN DISPLAY
DLI  
	PHA											;STACK REGISTERS
	TXA
	PHA
	TYA
	PHA
	CLD

	LDA DLIFLG
	BMI DLITOP
	JMP DLIBOT
DLITOP
	INC DLIFLG									;DO THE MIDDLE DLI NEXT
	JMP DLIOUT

;  THIS ROUTINE TAKES CARE OF THE LAST DLI ON THE SCREEN, THE 'VERTICAL BLANK
;  ROUTINE'.
DLIBOT
	LDA #$FF
	STA DLIFLG									;RESET FOR TOP DLI
DLIWAIT
	BIT MSTAT									;WAIT FOR VBLANK
	BPL DLIWAIT
	JSR TUNER									;DO TUNES
DLIOUT
	PLA											;UNSTACK AND LEAVE
	TAY
	PLA											;THIS IS WHERE MOST DLI'S LEAVE
	TAX
	PLA
NULLRTI
	RTI

;	DISPLAY LIST INFORMATION

;  DEFAULT TEMPLATE FOR DISPLAY LIST
NULDLST
	.byte $00,$00,$00,$00,$00

;	THIS IS THE DISPLAY LIST LIST.  THIS WILL BE DROPPED INTO RAM.
DLLISTPAL
	.byte $0F,>(NULDLST),<(NULDLST)				;25 ADDITIONAL BLANK LINES FOR PAL
	.byte $08,>(NULDLST),<(NULDLST)
DLLISTNTSC
	.byte $0F,>(NULDLST),<(NULDLST)				;21 BLANK LINES 
	.byte $04,>(NULDLST),<(NULDLST)

	.byte $C7,>(NULDLST),<(NULDLST)				;PF SCORE ZONE - 8 LINES WITH INTERRUPT SCDLIST
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 00 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 01 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 02 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 03 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 04 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 05 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 06 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 07 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 08 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 09 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 10 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 11 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 12 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 13 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 14 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 15 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 16 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 17 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 18 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 19 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 20 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 21 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 22 - 8 LINES
	.byte $47,>(NULDLST),<(NULDLST)				;PF ZONE 23 - 8 LINES

	.byte $8F,>(NULDLST),<(NULDLST)				;22 BLANK LINES - WITH INTERRUPT
	.byte $05,>(NULDLST),<(NULDLST)
DLLENDNTSC
	.byte $0F,>(NULDLST),<(NULDLST)				;25 ADDITIONAL BLANK LINES FOR PAL
	.byte $08,>(NULDLST),<(NULDLST)
DLLENDPAL

;	RESERVED AREA FOR CART SIGNATURE ----------------------------------------------------------------------------------------->
	ORG $FF80
	.byte $FF

;	CART RESET VECTOR -------------------------------------------------------------------------------------------------------->
	ORG	$FFF8
	.byte $FF									;REGION VERIFICATION
	.byte $C7									;ROM STARTS AT $C000
	.word #DLI
	.word #START
	.word #NULLRTI