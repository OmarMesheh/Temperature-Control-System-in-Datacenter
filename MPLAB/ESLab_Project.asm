#INCLUDE "P16F877A.INC"

__CONFIG _DEBUG_OFF&_CP_OFF&_WRT_HALF&_CPD_OFF&_LVP_OFF&_BODEN_OFF&_PWRTE_OFF&_WDT_OFF&_XT_OSC

cblock 0x20
	TEMP	;Temporary register
	SPD
	lsd		;lsd and msd are used in LCD delay loop calculation
	msd
	wasHOLD ;We will use the least significant bit in this register to check if the last state was hold
	TEMPWR	;Saves the working register value when an interrupt occurs
	TEMPST	;Saves the status register value when an interrupt occurs
endc
TMR0VAR EQU 0XAA		;Counter for timer0
TMR2VAR EQU 0XAB		;Counter for timer2
;**********************************************************************
; Start of executable code
	ORG  0x00
	nop
	GOTO Initial

; Interrupt vector
	ORG  0x04
	GOTO ISR

;**********************Initialize Register Values**********************
Initial
	Banksel	TRISA       	;Select bank 1
  	Movlw   b'00100000' 	;PORTA bit Number5 is INPUT
  	Movwf   TRISA
	BSF 	TRISE,0
	Movlw 	b'10000001'		;PORTB bits RB0, RB7 are inputs (push button+ switch)
	Movwf	TRISB
	Clrf	TRISD			;LCD data
	CLRF 	TRISC			;Set port c as output
	CLRF 	TMR2VAR		
	CLRF 	TMR0VAR
	Movlw   4BH
	Movwf   ADCON1
	BANKSEL PORTC
	Movlw   61H
	Movwf   ADCON0			;Left justified, fosc/8, AD enabled, RA5 input(AUTO mode), Vref are RA3 and RA2
	Movlw	b'11110000'		;Enable RB0 interrupt
	Movwf	INTCON
	BSF     SPD,0			;Set speed to zero initially
	CLRF	PORTC
	Clrf	PORTD			;LCD configuration:
	nop  
	BANKSEL OPTION_REG
	CLRF 	OPTION_REG		;Set prescale to 1 
	Bsf 	OPTION_REG, INTEDG
	BANKSEL PR2
	MOVLW 	.124					
	MOVWF 	PR2				;Set PR2 value to 124
	BSF 	PIE1,TMR2IE		;Enable Timer2 interrupt
	BANKSEL TMR0VAR
	MOVLW 	1
	MOVWF 	TMR0VAR			;Set TMR0 counter to 1 to test the input state after one interrupt
	CALL 	SETTMR0			;Sets the intial conditions for TMR0
	BANKSEL T2CON
	MOVLW 	B'00111000'		;Sets the TMR2 postscale to 8
	MOVWF 	T2CON
	Movlw 	0x38			;8-bit mode, 2-line display, 5x7 dot format
	Call 	send_cmd
	Movlw 	0x0c			;Display on, Cursor Underline off, Blink off
	Call 	send_cmd	
	
;****************************Main program******************************
Main
	BANKSEL   PORTB
	btfss	  PORTB,7 		;if RB7(switch)=0 -> Hold state
	Call 	  display_HOLD	;enter the hold state
recheck
	btfss	  PORTB,7		;infinite loop until the switch is released
	goto	  recheck

	btfsc	  wasHOLD,0		;if the last previous mode was Hold, clear the LCD 
	Call	  clear_LCD		;(if wasHOLD[0] == 1)

	Bsf       ADCON0,GO		;Startup ADC divert

Wait
	Btfss     PIR1,ADIF		;Checks if conversion is done
	Goto      Wait			;Wait for conversion to finish
	Bcf		  PIR1, ADIF	;Clear the A/D flag


	Btfss	  ADCON0, 3		;Checks mode of operation
	Call	  Check_temp	;Check temperature to choose appropriate 
	Btfsc	  ADCON0, 3
	Call	  Check_knob	;Check knob to choose appropriate speed
	
	Movwf	  SPD			;Save speed mode in first two bits of GPR SPD, 
							;SPD = 0001 -> 0000 RPM, SPD =0010 -> 1000 RPM, 
							;SPD = 0100 -> 2000 RPM, SPD =1000 -> 3000 RPM

	
	Call	  display_SPEED	;Display speed & temperature
	
	Goto	  Main

;**********************Temperature Check subroutine********************
Check_temp
	bcf		wasHOLD,0 		;Clear the bit associated with the hold state (wasHOLD[0] = 0)
	Call	display_AUTO
	Movf	ADRESH, W
	Movwf	TEMP
	Addlw	b'01010101'
	Btfsc	STATUS, C
	Retlw	8				;SPD = 1000 (3000 RPM)

	Movf	TEMP, W
	Addlw	b'10000100'
	Btfsc	STATUS, C
	Retlw	4				;SPD = 0100 (2000 RPM)

	Movf	TEMP, W
	Addlw	b'10110010'
	Btfsc	STATUS, C
	Retlw	2				;SPD = 0010 (1000 RPM)

	Retlw	1				;SPD = 0001 (0000 RPM)

;**************************Knob Check subroutine***********************
Check_knob
	bcf		wasHOLD,0		;Clear the bit associated with the hold state (wasHOLD[0] = 0)
	Call	display_MANUAL
	Movf	ADRESH, W
	Movwf	TEMP
	Addlw	b'10000000'
	Btfsc	STATUS, C
	Retlw	8				;SPD = 1000 (3000 RPM)

	Movf	TEMP, W
	Addlw	b'10111111'
	Btfsc	STATUS, C
	Retlw	4				;SPD = 0100 (2000 RPM)

	Movf	TEMP, W
	Addlw	b'11111111'
	Btfsc	STATUS, C
	Retlw	2				;SPD = 0010 (1000 RPM)

	Retlw	1				;SPD = 0001 (0000 RPM)

;****************************Display Auto******************************
display_AUTO
	Movlw 	0x38 		;8-bit mode, 2-line display, 5x7 dot format
	Call	send_cmd	
	movlw	0x86		;start at location 6
	Call	send_cmd
	movlw 	'A'
	Call  	send_char
	movlw 	'U'
	Call  	send_char
	movlw 	'T'
	Call  	send_char	
	movlw 	'O'
	Call  	send_char
return
;****************************Display MANU******************************
display_MANUAL
	Movlw 	0x38 		;8-bit mode, 2-line display, 5x7 dot format
	Call 	send_cmd
	movlw 	0x86		;start at location 6
	Call  	send_cmd
	movlw	'M'
	Call	send_char
	movlw	'A'
	Call	send_char
	movlw 	'N'
	Call  	send_char	
	movlw 	'U'
	Call  	send_char
return
;****************************Display Speed*****************************
display_SPEED
	movlw 	0xc2		;Start at location 2 of the 2nd line
	Call  	send_cmd
	btfsc	SPD,0		;The following four tests will choose what speed should be displayed
	movlw	'0'
	btfsc	SPD,1
	movlw	'1'
	btfsc	SPD,2
	movlw	'2'
	btfsc	SPD,3
	movlw	'3'
	Call	send_char
	
	movlw 	'0'
	Call  	send_char
	movlw 	'0'
	Call  	send_char
	movlw 	'0'
	Call  	send_char
	movlw 	'R'
	Call  	send_char
	movlw 	'P'
	Call  	send_char
	movlw 	'M'
	Call  	send_char
	movlw 	0x20
	Call  	send_char

	btfsc	SPD,0		;The following four tests will choose what temperature should be displayed
	movlw	'0'
	btfsc	SPD,1
	movlw	'1'
	btfsc	SPD,2
	movlw	'2'
	btfsc	SPD,3
	movlw	'3'
	Call	send_char
	
	movlw 	'0'
	Call  	send_char
	movlw 	'C'
	Call  	send_char
	movlw 	0xdf		;(') sign
	Call  	send_char
return

;***************************Display Hold*******************************
display_HOLD

	bsf		wasHOLD,0	;The previous mode was Hold(wasHOLD[0] = 1)
	movlw 	0x01
	Call 	send_cmd
	movlw 	0x81 		;Start at location 01
	Call 	send_cmd
	movlw 	'S'
	Call  	send_char
	movlw 	'y'
	Call  	send_char
	movlw 	's'
	Call  	send_char	
	movlw 	't'
	Call  	send_char	
	movlw 	'e'
	Call  	send_char
	movlw 	'm'
	Call  	send_char
	movlw 	0x20
	Call  	send_char
	movlw 	'i'
	Call  	send_char
	movlw 	'n'
	Call  	send_char
	movlw 	0x20
	Call  	send_char
	movlw 	'H'
	Call  	send_char
	movlw 	'o'
	Call  	send_char
	movlw 	'l'
	Call  	send_char
	movlw 	'd'
	Call  	send_char
	movlw	0x01
	movwf	SPD
return

;****************************Clear the LCD*****************************
clear_LCD
	movlw	0x01
	Call	send_cmd
	return
;***************************Send LCD command***************************
send_cmd
	movwf	PORTD 
	bcf 	PORTB, 1
	bsf 	PORTB, 3
	nop

	bcf 	PORTB, 3
	bcf 	PORTB, 2
  	call 	LCD_delay
return

;***************************Send LCD character*************************
send_char
	movwf 	PORTD 
	bsf 	PORTB, 1
	bsf 	PORTB, 3
	nop

	bcf 	PORTB, 3
	bcf 	PORTB, 2
return

;*******************************LCD delay*******************************
LCD_delay
	movlw 	0x10
	movwf	msd
	clrf 	lsd
loop2
	decfsz 	lsd,f
	goto 	loop2
	decfsz 	msd,f
endLcd
	goto 	loop2
return

;*********************************ISR**********************************
ISR
	BANKSEL	TEMPWR
	MOVWF 	TEMPWR
	SWAPF 	STATUS,0
	MOVWF 	TEMPST
	btfsc	INTCON, INTF
	goto 	external_interrupt ;Mode button pressed
	BANKSEL PIR1
	BTFSS 	PIR1,TMR2IF    	;Check if its a timer2 or timer0 interrupt
	GOTO 	TMR0INT
	GOTO 	TMR2INT


external_interrupt
	Banksel	PORTA			;Select bank 0
	Movf	ADCON0, W
	Xorlw	b'00001000'
	Movwf	ADCON0			;Change mode from AUTO to MANU or vice versa
	Bcf		INTCON, INTF
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF	TEMPWR,0
	Retfie
	

;********************TMR2 Interrupt Subroutine********************
TMR2INT
	BANKSEL PIR1 
	BCF 	PIR1,TMR2IF
	BANKSEL TMR2VAR
	DECFSZ 	TMR2VAR,1
	GOTO	TMR2RET
	GOTO	TMR2CONT

TMR2RET
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

TMR2CONT
	BANKSEL T2CON
	BCF 	T2CON,TMR2ON
	BSF 	INTCON,T0IE
	BCF 	INTCON,T0IF
	CALL 	SETTMR0			;After TMR2 is finished set up TMR0	

CASE0T2						;Speed=0000RPM
	BANKSEL SPD
	BTFSS 	SPD,0
	GOTO 	CASE1T2
	BANKSEL PORTC
	BCF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.44
	MOVWF 	TMR0VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF	TEMPWR,0
	RETFIE

CASE1T2						;Speed=1000RPM
	BANKSEL SPD
	BTFSS 	SPD,1
	GOTO 	CASE2T2
	BANKSEL PORTC
	BCF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.29
	MOVWF 	TMR0VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

CASE2T2						;Speed=2000RPM
	BANKSEL SPD
	BTFSS 	SPD,2
	GOTO 	CASE3T2
	BANKSEL PORTC
	BCF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.14
	MOVWF 	TMR0VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
RETFIE

CASE3T2						;Speed=3000RPM
	BANKSEL PORTC
	BSF 	PORTC,0
	BANKSEL T2CON
	BCF 	INTCON,T0IE
	BSF 	T2CON,TMR2ON
	CLRF 	TMR2
	BANKSEL TMR0VAR
	MOVLW 	.44
	MOVWF 	TMR2VAR			;Switch off TMR0 and turn TMR2 on again
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF	TEMPWR,0
	RETFIE


;*************************TMR0 Interrupt Subroutine********************
TMR0INT
	BCF		INTCON,T0IF
	CALL 	SETTMR0
	DECFSZ 	TMR0VAR,1
	GOTO 	TMR0RET
	GOTO	TMR0CONT

TMR0RET
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

TMR0CONT
	BCF		INTCON,T0IE
	BANKSEL T2CON
	BSF 	T2CON,TMR2ON
	BANKSEL PIR1
	BCF 	PIR1,TMR2IF
	CLRF 	TMR2		;After TMR0 is finished set up TMR2

CASE0T0					;Speed=0000RPM
	BANKSEL SPD
	BTFSS 	SPD,0
	GOTO 	CASE1T0
	BANKSEL PORTC
	BCF 	PORTC,0
	BSF 	INTCON,T0IE
	CALL 	SETTMR0
	BANKSEL T2CON
	BCF 	T2CON,TMR2ON
	BANKSEL TMR0VAR
	MOVLW 	.44
	MOVWF 	TMR0VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

CASE1T0					;Speed=1000RPM
	BANKSEL SPD
	BTFSS 	SPD,1
	GOTO 	CASE2T0
	BANKSEL PORTC
	BSF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.15
	MOVWF 	TMR2VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

CASE2T0					;Speed=2000RPM
	BANKSEL SPD
	BTFSS 	SPD,2
	GOTO 	CASE3T0
	BANKSEL PORTC
	BSF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.30
	MOVWF 	TMR2VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE

CASE3T0					;Speed=3000RPM
	BANKSEL PORTC
	BSF 	PORTC,0
	BANKSEL TMR0VAR
	MOVLW 	.44
	MOVWF 	TMR2VAR
	BANKSEL TEMPWR
	SWAPF 	TEMPST,0
	MOVWF 	STATUS
	MOVF 	TEMPWR,0
	RETFIE				;Switch off TMR2 and turn TMR0 on again


;************************Setting up TMR0 subroutine********************
SETTMR0					
	BANKSEL TMR0
	MOVLW 	.134
	MOVWF 	TMR0
	BANKSEL OPTION_REG
	MOVLW 	B'01000010'
	MOVWF 	OPTION_REG
	RETURN
;**********************************************************************
END