        errorlevel -302
;------------------------------------------------------------------------------
; PROCESSOR DECLARATION
;------------------------------------------------------------------------------

     LIST      P=12F629              ; list directive to define processor
     #INCLUDE <P12F629.INC>          ; processor specific variable definitions
     #INCLUDE <basic.inc>      ; 
     #INCLUDE <vtimer.inc>           ; 

;------------------------------------------------------------------------------
;
; CONFIGURATION WORD SETUP
;
; The 'CONFIG' directive is used to embed the configuration word within the 
; .asm file. The lables following the directive are located in the respective 
; .inc file.  See the data sheet for additional information on configuration 
; word settings.
;
;------------------------------------------------------------------------------

	    __CONFIG   _CP_OFF & _CPD_OFF & _BODEN_ON & _MCLRE_OFF & _WDT_OFF & _PWRTE_ON & _INTRC_OSC_NOCLKOUT 

;------------------------------------------------------------------------------
; VARIABLE DEFINITIONS
;------------------------------------------------------------------------------

BUTTON1							EQU		GPIO1
BUTTON2							EQU		GPIO3
LED1							EQU		GPIO2
LED2							EQU		GPIO4

LEDOFFTHRESHOLD					EQU		H'B0'
LEDLOWTHRESHOLD					EQU		H'FB'

LEDHIGHOFFSET					EQU		H'10'
LEDLOWOFFSET					EQU		H'90'

VTIMER1_LED1_L_OFF				EQU		H'00'
VTIMER1_LED1_H_OFF				EQU		H'B0'
VTIMER1_LED1_L_ON				EQU		H'FF'
VTIMER1_LED1_H_ON				EQU		H'FF'
VTIMER1_LED2_L_OFF				EQU		H'00'
VTIMER1_LED2_H_OFF				EQU		H'B0'
VTIMER1_LED2_L_ON				EQU		H'FF'
VTIMER1_LED2_H_ON				EQU		H'FF'

BUTTON_DEBOUNCE_DELAY			EQU		H'00'
BUTTON_DEBOUNCE_DELAY_EXT		EQU		H'FF'
BUTTON_FADE_DELAY				EQU		H'A0'
BUTTON_FADE_DELAY_EXT			EQU		H'FF'
BUTTON_SWITCH_DELAY				EQU		H'00'
BUTTON_SWITCH_DELAY_EXT			EQU		H'F7'

; example of using Shared Uninitialized Data Section
INT_VAR     					UDATA_SHR   			;
W_TEMP      					RES		1				; variable used for context saving 
STATUS_TEMP 					RES		1				; variable used for context saving
TEMPF							RES		1

CONST_00						RES		1


LED1CON							RES		1
LED1OFF							EQU		H'00'
LED1FADE						EQU		H'01'
LED1ON							EQU		H'02'
LED1BUTTONLOCK					EQU		H'03'
LED1BUTTONSWITCHDELAY			EQU		H'04'
LED1FADEOUT						EQU		H'05'

LED2CON							RES		1
LED2OFF							EQU		H'00'
LED2FADE						EQU		H'01'
LED2ON							EQU		H'02'
LED2BUTTONLOCK					EQU		H'03'
LED2BUTTONSWITCHDELAY			EQU		H'04'
LED2FADEOUT						EQU		H'05'

VPWMCON							RES		1
VPWMID0							EQU		H'00'
VPWMID1							EQU 	H'01'

BUTTON1_NEW						RES		1
BUTTON1_OLD						RES		1
BUTTON2_NEW						RES		1
BUTTON2_OLD						RES		1

BUTTON_DEBOUNCE_DELAY_RELOAD	RES		1
BUTTON_SWITCH_DELAY_RELOAD		RES		1
BUTTON_FADE_DELAY_RELOAD		RES		1

VTIMER0_LED1_RELOAD				RES		1
VTIMER0_LED2_RELOAD				RES		1
VTIMER0_EXT00					RES		1
VTIMER0_EXT01					RES		1
VTIMER0_EXT02					RES		1
VTIMER0_EXT03					RES		1

VTIMER1_LED1_RELOAD_L_OFF		RES		1
VTIMER1_LED1_RELOAD_H_OFF		RES		1
VTIMER1_LED1_RELOAD_L_ON		RES		1
VTIMER1_LED1_RELOAD_H_ON		RES		1
VTIMER1_LED2_RELOAD_L_OFF		RES		1
VTIMER1_LED2_RELOAD_H_OFF		RES		1
VTIMER1_LED2_RELOAD_L_ON		RES		1
VTIMER1_LED2_RELOAD_H_ON		RES		1

			VTIMER0 4
			VTIMER1 2

;------------------------------------------------------------------------------
; EEPROM INITIALIZATION
;
; The 12F629 has 128 bytes of non-volatile EEPROM, starting at address 0x2100
; 
;------------------------------------------------------------------------------

;DATAEE    CODE  0x2100
;    DE    "MCHP"          ; Place 'M' 'C' 'H' 'P' at address 0,1,2,3

;------------------------------------------------------------------------------
; OSCILLATOR CALIBRATION VALUE
;------------------------------------------------------------------------------

OSC       CODE    0x03FF
		  RETLW   H'B0'


; Internal RC calibration value is placed at location 0x3FF by Microchip as
; a 0xADDLW K instruction, where the K is a literal value to be loaded into 
; the OSCCAL register.  

;------------------------------------------------------------------------------
; RESET VECTOR
;------------------------------------------------------------------------------

RESET_VECTOR  CODE      0x0000 ; processor reset vector
        GOTO    INIT           ; go to beginning of program


;------------------------------------------------------------------------------
; MAIN PROGRAM
;------------------------------------------------------------------------------
			CODE			
INIT
;------------------------------------------------------------------------------
; OSCCAL RESTORE (not required if internal OSC is not used)
;------------------------------------------------------------------------------

        BSF     STATUS,RP0    ; set file register bank to 1 
        CALL    0x3FF         ; retrieve factory calibration value
        MOVWF   OSCCAL        ; update register with factory cal value 
        BCF     STATUS,RP0    ; set file register bank to 0
;------------------------------------------------------------------------------
; MEMORY INIT
;------------------------------------------------------------------------------
		clrf	CONST_00

		clrf	LED1CON
		bsf		LED1CON,LED1OFF

		clrf	LED2CON
		bsf		LED2CON,LED2OFF

		clrf	VPWMCON
		clrf	BUTTON1_NEW
		clrf	BUTTON1_OLD
		bsf		BUTTON1_OLD,BUTTON1
		clrf	BUTTON2_NEW
		clrf	BUTTON2_OLD
		bsf		BUTTON2_OLD,BUTTON2
		
		movlw	BUTTON_SWITCH_DELAY
		movwf	BUTTON_SWITCH_DELAY_RELOAD
		movlw	BUTTON_FADE_DELAY
		movwf	BUTTON_FADE_DELAY_RELOAD
		movlw	BUTTON_DEBOUNCE_DELAY
		movwf	BUTTON_DEBOUNCE_DELAY_RELOAD
;------------------------------------------------------------------------------
; PROGRAMM INIT
;------------------------------------------------------------------------------
		movlw	VTIMER1_LED1_H_OFF
		movwf	VTIMER1_LED1_RELOAD_H_OFF
		movlw	VTIMER1_LED1_L_OFF
		movwf	VTIMER1_LED1_RELOAD_L_OFF
		movlw	VTIMER1_LED1_H_ON
		movwf	VTIMER1_LED1_RELOAD_H_ON
		movlw	VTIMER1_LED1_L_ON
		movwf	VTIMER1_LED1_RELOAD_L_ON
		movlw	VTIMER1_LED2_H_OFF
		movwf	VTIMER1_LED2_RELOAD_H_OFF
		movlw	VTIMER1_LED2_L_OFF
		movwf	VTIMER1_LED2_RELOAD_L_OFF
		movlw	VTIMER1_LED2_H_ON
		movwf	VTIMER1_LED2_RELOAD_H_ON
		movlw	VTIMER1_LED2_L_ON
		movwf	VTIMER1_LED2_RELOAD_L_ON

;------------------------------------------------------------------------------
; GPIO
;-----------------------------------------------------------------------------
		banksel		OPTION_REG
		bcf			OPTION_REG,PSA
		bsf			OPTION_REG,PS0
		bsf			OPTION_REG,PS1
		bsf			OPTION_REG,PS2
		

		banksel		GPIO
		clrf		GPIO

		movlw		07h			;set GP<0:2>	
		movwf		CMCON		;to digital IO
		IFDEF 		__12F675
			banksel 	ANSEL
			clrf		ANSEL
		ENDIF
		banksel		TRISIO
		clrf		TRISIO
		bsf			TRISIO,BUTTON1
		bsf			TRISIO,BUTTON2
		clrf		WPU
		
;------------------------------------------------------------------------------
; VTIMER0
;------------------------------------------------------------------------------
		banksel	TMR0
		clrf	TMR0
		VTIMER0_INIT
;------------------------------------------------------------------------------
; VTIMER1
;------------------------------------------------------------------------------
		clrf	TMR1L
		clrf	TMR1H
		clrf	T1CON

		VTIMER1_INIT
		bsf		T1CON,TMR1ON
;------------------------------------------------------------------------------
; INTERRUPT
;------------------------------------------------------------------------------
		banksel	IOC
		clrf	IOC
		bsf		IOC,BUTTON1
		bsf		IOC,BUTTON2
		clrf	PIE1
		bsf		PIE1,TMR1IE
		
		clrf	INTCON
		bsf		INTCON,T0IE
		bsf		INTCON,GPIE
		bsf		INTCON,PEIE
		bsf		INTCON,GIE
		
        
;------------------------------------------------------------------------------
; PLACE USER PROGRAM HERE
;------------------------------------------------------------------------------
mainloop 
		GOTO mainloop




;------------------------------------------------------------------------------
; INTERRUPT SERVICE ROUTINE
;------------------------------------------------------------------------------

INT_VECTOR    CODE    0x0004  ; interrupt vector location
        MOVWF   W_TEMP        ; save off current W register contents
        MOVF    STATUS,w      ; move status register into W register
        MOVWF   STATUS_TEMP   ; save off contents of STATUS register

; isr code can go here or be located as a call subroutine elsewhere
;------------------------------------------------------------------------------
;       VTIMER1 interrupt
;------------------------------------------------------------------------------
		banksel	PIR1
		_if		_BTF,PIR1,TMR1IF,_S
			bcf		T1CON,TMR1ON

			VTIMER1_CHECK
			_if		_BTF,VTMR1INT,VTMR10ID,_S
				bcf	VTMR1INT,VTMR10ID
				_if _BTF,VPWMCON,VPWMID0,_S
					bcf GPIO,LED1
					bcf VPWMCON,VPWMID0
					VTIMER1_ON	VTMR10ID,VTIMER1_LED1_RELOAD_L_OFF,VTIMER1_LED1_RELOAD_H_OFF
				_else
					bsf GPIO,LED1
					bsf VPWMCON,VPWMID0
					VTIMER1_ON	VTMR10ID,VTIMER1_LED1_RELOAD_L_ON,VTIMER1_LED1_RELOAD_H_ON
				_endif
			_endif
			_if		_BTF,VTMR1INT,VTMR11ID,_S
				bcf	VTMR1INT,VTMR11ID
				_if _BTF,VPWMCON,VPWMID1,_S
					bcf GPIO,LED2
					bcf VPWMCON,VPWMID1
					VTIMER1_ON	VTMR11ID,VTIMER1_LED2_RELOAD_L_OFF,VTIMER1_LED2_RELOAD_H_OFF
				_else
					bsf GPIO,LED2
					bsf VPWMCON,VPWMID1
					VTIMER1_ON	VTMR11ID,VTIMER1_LED2_RELOAD_L_ON,VTIMER1_LED2_RELOAD_H_ON
				_endif
			_endif

			bcf		PIR1,TMR1IF
			VTIMER1_START
			bsf		T1CON,TMR1ON
VTIMER1_END:
		_endif

;------------------------------------------------------------------------------
;       GPIO interrupt
;------------------------------------------------------------------------------
		_if		_BTF,INTCON,GPIF,_S
			banksel	OPTION_REG
			bsf		OPTION_REG,T0CS
			banksel	VTMR0CON
			VTIMER0_SUSPEND
			MOVF	GPIO,w

			_if _BTF,LED1CON,LED1BUTTONLOCK,_C				;continue only, if button1 not locked
				bsf	LED1CON,LED1BUTTONLOCK
				movlw	BUTTON_DEBOUNCE_DELAY_EXT
				movwf	VTIMER0_EXT00
				VTIMER0_ON	VTMR00ID,BUTTON_DEBOUNCE_DELAY_RELOAD
			_endif
			_if _BTF,LED2CON,LED2BUTTONLOCK,_C				;continue only, if button2 not locked
				bsf	LED2CON,LED2BUTTONLOCK
				movlw	BUTTON_DEBOUNCE_DELAY_EXT
				movwf	VTIMER0_EXT01
				VTIMER0_ON	VTMR01ID,BUTTON_DEBOUNCE_DELAY_RELOAD
			_endif
			bcf		INTCON,GPIF
			VTIMER0_START
			banksel	OPTION_REG
			bcf		OPTION_REG,T0CS
		_endif


;------------------------------------------------------------------------------
;       VTIMER0 interrupt
;------------------------------------------------------------------------------
		_if		_BTF,INTCON,T0IF,_S
			banksel	OPTION_REG
			bsf		OPTION_REG,T0CS
			banksel	VTMR0CON	
			VTIMER0_CHECK

			_if		_BTF,VTMR0INT,VTMR00ID,_S				;VTIMER 00 interrupted?
				bcf	VTMR0INT,VTMR00ID						;clear VTIMER interrupt
				incf	VTIMER0_EXT00,f						;increase VTIMER EXTENSION
				_if	_BTF,STATUS,Z,_S						;overflow?
					VTIMER0_OFF VTMR00ID						;switch vtimer off
					bcf LED1CON,LED1BUTTONLOCK					;clear lock

					MOVF	GPIO,w
					MOVWF	BUTTON1_NEW

					_if		_BTF,BUTTON1_NEW,BUTTON1,_S			;new button is high (released)
						_if	_BTF,BUTTON1_OLD,BUTTON1,_C			;old button was low	(pressed)
							bsf	BUTTON1_OLD,BUTTON1				;we captured a "button release", remember new button state
							VTIMER0_OFF VTMR02ID				;stop fade timer
							bcf	VTMR0INT,VTMR02ID

							_if _BTF,LED1CON,LED1BUTTONSWITCHDELAY,_S	;we are in switch mode
								VTIMER1_OFF	VTMR10ID
								bcf	VTMR1INT,VTMR10ID
								_if	_BTF,LED1CON,LED1OFF,_S			;LED is off
									bsf	GPIO,LED1					;turn LED on
									bsf	LED1CON,LED1ON
									bcf	LED1CON,LED1OFF
									bsf	LED1CON,LED1FADEOUT
									movlw	H'FF'
									movwf	VTIMER1_LED1_RELOAD_H_OFF
									movwf	VTIMER1_LED1_RELOAD_L_OFF
								_else								;LED is on
									bcf	GPIO,LED1					;turn LED off
									bcf	LED1CON,LED1ON
									bcf	LED1CON,LED1FADE
									bsf	LED1CON,LED1OFF
									bcf	LED1CON,LED1FADEOUT
									movlw	VTIMER1_LED1_H_OFF
									movwf	VTIMER1_LED1_RELOAD_H_OFF
									clrf	VTIMER1_LED1_RELOAD_L_OFF
								_endif
							_endif
						_endif
					_else										;new button is low (pressed)
						_if	_BTF,BUTTON1_OLD,BUTTON1,_S			;old button was high (released)
							bcf	BUTTON1_OLD,BUTTON1				;we captured a "button press", remember new button state
							bsf	LED1CON,LED1BUTTONSWITCHDELAY
							movlw	BUTTON_SWITCH_DELAY_EXT
							movwf	VTIMER0_EXT02
							VTIMER0_ON	VTMR02ID,BUTTON_SWITCH_DELAY_RELOAD
						_endif									;endif old button button was high
					_endif										;endif new button is low
				_else											;else VTIMER EXTENSION
					VTIMER0_ON	VTMR00ID,CONST_00				;if VTIMER extension not overflowed,restart VTIMER
				_endif											;endif VTIMER EXTENSION
			_endif												;endif VTMR00ID interrupt


			_if		_BTF,VTMR0INT,VTMR01ID,_S				;VTIMER 01 interrupted?
				bcf	VTMR0INT,VTMR01ID						;clear VTIMER interrupt
				incf	VTIMER0_EXT01,f						;increase VTIMER EXTENSION
				_if	_BTF,STATUS,Z,_S						;overflow?
					VTIMER0_OFF VTMR01ID						;switch vtimer off
					bcf LED2CON,LED2BUTTONLOCK					;clear lock

					MOVF	GPIO,w
					MOVWF	BUTTON2_NEW

					_if		_BTF,BUTTON2_NEW,BUTTON2,_S			;new button is high (released)
						_if	_BTF,BUTTON2_OLD,BUTTON2,_C			;old button was low	(pressed)
							bsf	BUTTON2_OLD,BUTTON2				;we captured a "button release", remember new button state
							VTIMER0_OFF VTMR03ID				;stop fade timer
							bcf	VTMR0INT,VTMR03ID

							_if _BTF,LED2CON,LED2BUTTONSWITCHDELAY,_S	;we are in switch mode
								VTIMER1_OFF	VTMR11ID
								bcf	VTMR1INT,VTMR11ID
								_if	_BTF,LED2CON,LED2OFF,_S			;LED is off
									bsf	GPIO,LED2					;turn LED on
									bsf	LED2CON,LED2ON
									bcf	LED2CON,LED2OFF
									bsf	LED2CON,LED2FADEOUT
									movlw	H'FF'
									movwf	VTIMER1_LED2_RELOAD_H_OFF
									movwf	VTIMER1_LED2_RELOAD_L_OFF
								_else								;LED is on
									bcf	GPIO,LED2					;turn LED off
									bcf	LED2CON,LED2ON
									bcf	LED2CON,LED2FADE
									bsf	LED2CON,LED2OFF
									bcf	LED2CON,LED2FADEOUT
									movlw	VTIMER1_LED2_H_OFF
									movwf	VTIMER1_LED2_RELOAD_H_OFF
									clrf	VTIMER1_LED2_RELOAD_L_OFF
								_endif
							_endif
						_endif
					_else										;new button is low (pressed)
						_if	_BTF,BUTTON2_OLD,BUTTON2,_S			;old button was high (released)
							bcf	BUTTON2_OLD,BUTTON2				;we captured a "button press", remember new button state
							bsf	LED2CON,LED2BUTTONSWITCHDELAY
							movlw	BUTTON_SWITCH_DELAY_EXT
							movwf	VTIMER0_EXT03
							VTIMER0_ON	VTMR03ID,BUTTON_SWITCH_DELAY_RELOAD
						_endif									;endif old button button was high
					_endif										;endif new button is low
				_else											;else VTIMER EXTENSION
					VTIMER0_ON	VTMR01ID,CONST_00				;if VTIMER extension not overflowed,restart VTIMER
				_endif											;endif VTIMER EXTENSION
			_endif												;endif VTMR00ID interrupt



			_if		_BTF,VTMR0INT,VTMR02ID,_C
				_if _BTF,VTMR0INT,VTMR03ID,_C
					goto NO_PWMCHANGE_EVENT
				_endif
			_endif

PWMCHANGE_EVENT:
			bcf		T1CON,TMR1ON
			VTIMER1_SUSPEND
			_if		_BTF,VTMR0INT,VTMR02ID,_S
				bcf	VTMR0INT,VTMR02ID
				_if	_INCF,VTIMER0_EXT02,f,_Z						;increase VTIMER EXTENSION -> overflow?
					bcf	LED1CON,LED1BUTTONSWITCHDELAY

					movf	VTIMER1_LED1_RELOAD_H_OFF,w
					sublw	LEDLOWTHRESHOLD
					_if	_BTF,STATUS,C,_C
						movlw	LEDHIGHOFFSET
					_else
						movlw	LEDLOWOFFSET
					_endif
					_if	_BTF,LED1CON,LED1FADEOUT,_S
						subwf	VTIMER1_LED1_RELOAD_L_OFF,f
						_if	_BTF,STATUS,C,_C
							decf	VTIMER1_LED1_RELOAD_H_OFF,F
						_endif
					_else
						addwf	VTIMER1_LED1_RELOAD_L_OFF,f
						_if	_BTF,STATUS,C,_S
							incf	VTIMER1_LED1_RELOAD_H_OFF,F
						_endif
					_endif
					
					_if _TSTF,VTIMER1_LED1_RELOAD_H_OFF,,_Z				;switch LED on
						movlw	H'FF'
						movwf	VTIMER1_LED1_RELOAD_H_OFF
						movwf	VTIMER1_LED1_RELOAD_L_OFF

						bcf LED1CON,LED1OFF
						bsf LED1CON,LED1ON
						bcf	LED1CON,LED1FADE
						bsf	LED1CON,LED1FADEOUT
						bcf	VPWMCON,VPWMID0
						bsf GPIO,LED1
						VTIMER0_OFF	VTMR02ID
						VTIMER1_OFF	VTMR10ID
					_else
						movlw	LEDOFFTHRESHOLD
						_if	_CPF,VTIMER1_LED1_RELOAD_H_OFF,,_LT		;switch LED off
							movlw	VTIMER1_LED1_H_OFF
							movwf	VTIMER1_LED1_RELOAD_H_OFF
							clrf	VTIMER1_LED1_RELOAD_L_OFF

							bsf LED1CON,LED1OFF
							bcf LED1CON,LED1ON
							bcf	LED1CON,LED1FADE
							bcf	LED1CON,LED1FADEOUT
							bcf	VPWMCON,VPWMID0
							bcf GPIO,LED1
							VTIMER0_OFF	VTMR02ID
							VTIMER1_OFF	VTMR10ID
						_else											;LED fade mode
							bcf LED1CON,LED1OFF
							bcf LED1CON,LED1ON
							bsf	LED1CON,LED1FADE
							movlw	BUTTON_FADE_DELAY_EXT
							movwf	VTIMER0_EXT02
							VTIMER0_ON	VTMR02ID,BUTTON_FADE_DELAY_RELOAD
							_if	_BTF,VTMR1CON,VTMR10ID,_C				;if PWM timer is off
								bcf	VPWMCON,VPWMID0
								VTIMER1_ON	VTMR10ID,VTIMER1_LED1_RELOAD_L_OFF,VTIMER1_LED1_RELOAD_H_OFF
							_endif
						_endif
					_endif
				_else
					VTIMER0_ON	VTMR02ID,CONST_00				;if VTIMER extension not overflowed,restart VTIMER
				_endif
			_endif
			

			_if		_BTF,VTMR0INT,VTMR03ID,_S
				bcf	VTMR0INT,VTMR03ID
				_if	_INCF,VTIMER0_EXT03,f,_Z				;increase VTIMER EXTENSION -> overflow?
					bcf	LED2CON,LED2BUTTONSWITCHDELAY

					movf	VTIMER1_LED2_RELOAD_H_OFF,w
					sublw	LEDLOWTHRESHOLD
					_if	_BTF,STATUS,C,_C
						movlw	LEDHIGHOFFSET
					_else
						movlw	LEDLOWOFFSET
					_endif
					_if	_BTF,LED2CON,LED2FADEOUT,_S
						subwf	VTIMER1_LED2_RELOAD_L_OFF,f
						_if	_BTF,STATUS,C,_C
							decf	VTIMER1_LED2_RELOAD_H_OFF,F
						_endif
					_else
						addwf	VTIMER1_LED2_RELOAD_L_OFF,f
						_if	_BTF,STATUS,C,_S
							incf	VTIMER1_LED2_RELOAD_H_OFF,F
						_endif
					_endif
					
					_if _TSTF,VTIMER1_LED2_RELOAD_H_OFF,,_Z				;switch LED on
						movlw	H'FF'
						movwf	VTIMER1_LED2_RELOAD_H_OFF
						movwf	VTIMER1_LED2_RELOAD_L_OFF

						bcf LED2CON,LED2OFF
						bsf LED2CON,LED2ON
						bcf	LED2CON,LED2FADE
						bsf	LED2CON,LED2FADEOUT
						bcf	VPWMCON,VPWMID1
						bsf GPIO,LED2
						VTIMER0_OFF	VTMR03ID
						VTIMER1_OFF	VTMR11ID
					_else
						movlw	LEDOFFTHRESHOLD
						_if	_CPF,VTIMER1_LED2_RELOAD_H_OFF,,_LT		;switch LED off
							movlw	VTIMER1_LED2_H_OFF
							movwf	VTIMER1_LED2_RELOAD_H_OFF
							clrf	VTIMER1_LED2_RELOAD_L_OFF

							bsf LED2CON,LED2OFF
							bcf LED2CON,LED2ON
							bcf	LED2CON,LED2FADE
							bcf	LED2CON,LED2FADEOUT
							bcf	VPWMCON,VPWMID1
							bcf GPIO,LED2
							VTIMER0_OFF	VTMR03ID
							VTIMER1_OFF	VTMR11ID
						_else											;LED fade mode
							bcf LED2CON,LED2OFF
							bcf LED2CON,LED2ON
							bsf	LED2CON,LED2FADE
							movlw	BUTTON_FADE_DELAY_EXT
							movwf	VTIMER0_EXT03
							VTIMER0_ON	VTMR03ID,BUTTON_FADE_DELAY_RELOAD
							_if	_BTF,VTMR1CON,VTMR11ID,_C				;if PWM timer is off
								bcf	VPWMCON,VPWMID1
								VTIMER1_ON	VTMR11ID,VTIMER1_LED2_RELOAD_L_OFF,VTIMER1_LED2_RELOAD_H_OFF
							_endif
						_endif
					_endif
				_else
					VTIMER0_ON	VTMR03ID,CONST_00				;if VTIMER extension not overflowed,restart VTIMER
				_endif
			_endif


			VTIMER1_START
			bsf		T1CON,TMR1ON
NO_PWMCHANGE_EVENT:

			bcf		INTCON,T0IF
			VTIMER0_START
			banksel	OPTION_REG
			bcf		OPTION_REG,T0CS
		_endif
		

		MOVF    STATUS_TEMP,w ; retrieve copy of STATUS register
        MOVWF   STATUS        ; restore pre-isr STATUS register contents
        SWAPF   W_TEMP,f
        SWAPF   W_TEMP,w      ; restore pre-isr W register contents
        RETFIE                ; return from interrupt

        END                       ; directive 'end of program'

        errorlevel +302
