	;==================================
        ; The most recent version of the
        ; a7800 header can be found here:
        ; http://7800.8bitdev.org/index.php/A78_Header_Specification
        ;==================================

	processor 6502
	org $F000

	; This module inserts the 128-byte field for
	; use on the 7800 emulators

	; 0      Header version     - 1 byte
	dc.b	$01

	; 1..16  "ATARI7800       "  - 16 bytes
        dc.b	"ATARI7800       "

	; 17..48 Cart title         - 32 bytes
	dc.b	"16 kilobytes header             "

	; 49..52 data length        - 4 bytes
	; 16K header:
	;dc.b	$00,$00,$40,$00
	; 48K Header:
	dc.b	$00,$00,$c0,$00
	; 32K Header:
	;dc.b	$00,$00,$80,$00

	; 53..54 cart type          - 2 bytes
  	; bit 0     = pokey at $4000
  	; bit 1     = supergame bank switched
  	; bit 2     = supergame ram at $4000
  	; bit 3     = rom at $4000
  	; bit 4     = bank 6 at $4000
  	; bit 5     = banked ram
  	; bit 6     = pokey at $450
  	; bit 7     = mirror ram at $4000
  	; bit 8     = activision banking
  	; bit 9     = absolute banking
  	; bit 10    = pokey at $440
  	; bit 11    = ym2151 at $460/$461
  	; bit 12    = souper
  	; bit 13    = banksets
  	; bit 14    = halt banked ram
  	; bit 15    = pokey@800
	dc.b	%00000000, %00000001

	; 55     controller 1 type  - 1 byte
	.byte $01

	; 56     controller 2 type  - 1 byte
	;     0 = None
	;     1 = Joystick
	;     2 = Light Gun
	.byte $01

	; 57     cartridge region  - 1 byte
	;     0/$FF = NTSC
	;     1 = PAL
	.byte 0

	; 58..99 Not used          - 42 bytes
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF
	.byte $FF,$FF,$FF,$FF,$FF,$FF

	; 100..127 "ACTUAL CART DATA STARTS HERE" - 28 bytes
	.byte $41,$43,$54,$55,$41,$4c		; Actual
	.byte $20					; (space)
	.byte $43,$41,$52,$54			; Cart
	.byte $20					; (space)
	.byte $44,$41,$54,$41			; Data
	.byte $20					; (space)
	.byte $53,$54,$41,$52,$54,$53		; Starts
	.byte $20					; (space)
	.byte $48,$45,$52,$45			; Here
