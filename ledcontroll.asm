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

SWITCH1						EQU		GPIO1
SWITCH2						EQU		GPIO3
LED1						EQU		GPIO2
LED2						EQU		GPIO4


; example of using Shared Uninitialized Data Section
INT_VAR     				UDATA_SHR   			;0x20   
W_TEMP      				RES		1				; variable used for context saving 
STATUS_TEMP 				RES		1				; variable used for context saving
TEMPF						RES		1


LED1CON						RES		1
LED1OFF						EQU		H'00'
LED1DIMM					EQU		H'01'
LED1ON						EQU		H'02'
LED1BUTTONLOCK				EQU		H'03'

LED2CON						RES		1
LED2OFF						EQU		H'04'
LED2DIMM					EQU		H'05'
LED2ON						EQU		H'06'
LED2SWITCHLOCK				EQU		H'07'

VPWMCON						RES		1
VPWMID0						EQU 	H'00'
VPWMID1						EQU 	H'01'


SWITCH_NEW	RES		1
SWITCH_OLD	RES		1

VTIMER0_LED1_RELOAD			RES		1
VTIMER0_LED2_RELOAD			RES		1

VTIMER1_LED1_RELOAD_L_OFF	RES		1
VTIMER1_LED1_RELOAD_H_OFF	RES		1
VTIMER1_LED1_RELOAD_L_ON	RES		1
VTIMER1_LED1_RELOAD_H_ON	RES		1
VTIMER1_LED2_RELOAD_L_OFF	RES		1
VTIMER1_LED2_RELOAD_H_OFF	RES		1
VTIMER1_LED2_RELOAD_L_ON	RES		1
VTIMER1_LED2_RELOAD_H_ON	RES		1

			VTIMER0 2
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
; PROGRAMM INIT
;-----------------------------------------------------------------------------
		banksel	VTIMER0_LED1_RELOAD
		clrf	VTIMER0_LED1_RELOAD
		clrf	VTIMER0_LED2_RELOAD
		MOVLW	H'A0'
		MOVWF	VTIMER0_LED1_RELOAD
		MOVLW	H'C0'
		MOVWF	VTIMER0_LED2_RELOAD

		clrf	VTIMER1_LED1_RELOAD_L_OFF
		clrf	VTIMER1_LED2_RELOAD_L_OFF
		clrf	VTIMER1_LED1_RELOAD_H_OFF
		clrf	VTIMER1_LED2_RELOAD_H_OFF
		clrf	VTIMER1_LED1_RELOAD_L_ON
		clrf	VTIMER1_LED2_RELOAD_L_ON
		clrf	VTIMER1_LED1_RELOAD_H_ON
		clrf	VTIMER1_LED2_RELOAD_H_ON
		MOVLW	H'01'
		MOVWF	VTIMER1_LED1_RELOAD_H_OFF
		MOVLW	H'01'
		MOVWF	VTIMER1_LED1_RELOAD_L_OFF
		MOVLW	H'01'
		MOVWF	VTIMER1_LED2_RELOAD_H_OFF
		MOVLW	H'02'
		MOVWF	VTIMER1_LED2_RELOAD_L_OFF
;------------------------------------------------------------------------------
; GPIO
;-----------------------------------------------------------------------------
		banksel		OPTION_REG
		bcf			OPTION_REG,NOT_GPPU

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
		bsf			TRISIO,SWITCH1
		bsf			TRISIO,SWITCH2
		banksel		WPU
		clrf		WPU
		bsf			WPU,SWITCH1
		bsf			WPU,SWITCH2
		
;------------------------------------------------------------------------------
; VTIMER0
;------------------------------------------------------------------------------
		banksel	TMR0
		clrf	TMR0
		VTIMER0_INIT
		VTIMER0_ON	VTMR00ID,VTIMER0_LED1_RELOAD
		VTIMER0_ON	VTMR01ID,VTIMER0_LED2_RELOAD
		VTIMER0_CHECK
		VTIMER0_START
		banksel	OPTION_REG
;		bcf		OPTION_REG,T0CS
;------------------------------------------------------------------------------
; VTIMER1
;------------------------------------------------------------------------------
		banksel	TMR1L
		clrf	TMR1L
		clrf	TMR1H
		clrf	T1CON
		VTIMER1_INIT
		VTIMER1_ON	VTMR10ID,VTIMER1_LED1_RELOAD_L_OFF,VTIMER1_LED1_RELOAD_H_OFF
		VTIMER1_ON	VTMR11ID,VTIMER1_LED2_RELOAD_L_OFF,VTIMER1_LED2_RELOAD_H_OFF
		VTIMER1_CHECK
		VTIMER1_START
		banksel	T1CON
		bsf		T1CON,TMR1ON
;------------------------------------------------------------------------------
; INTERRUPT
;------------------------------------------------------------------------------
		banksel	IOC
		clrf	IOC
		bsf		IOC,SWITCH1
		bsf		IOC,SWITCH2
		banksel	PIE1
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
;       VTIMER1 interupt
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

			VTIMER1_START
			bcf		PIR1,TMR1IF
			bsf		T1CON,TMR1ON
		_endif

;------------------------------------------------------------------------------
;       GPIO interupt
;------------------------------------------------------------------------------
		banksel	INTCON
		_if		_BTF,INTCON,GPIF,_S
			banksel	GPIO
			MOVF	GPIO,w
			MOVWF	SWITCH_NEW
			_if		_BTF,SWITCH_NEW,SWITCH1,_S			;new button is high (released)
				_if	_BTF,SWITCH_OLD,SWITCH1,_C			;old button was low	(pressed)
					bsf	SWITCH_OLD,SWITCH1				;we captured a "button release", remember new button state

					banksel OPTION_REG
					bcf	OPTION_REG,T0CS

					banksel	GPIO
					bsf	GPIO,LED1
				_endif
			_else										;new button is low (pressed)
				_if	_BTF,SWITCH_OLD,SWITCH1,_S			;old button was high (released)
					bcf	SWITCH_OLD,SWITCH1				;we captured a "button press", remember new button state

					_if _BTF,LED1CON,LED1BUTTONLOCK,_C	;continue only, if button not locked
						banksel OPTION_REG
						bsf	OPTION_REG,T0CS

						banksel	GPIO					
						bcf	GPIO,LED1
					_endif
				_endif
			_endif

			_if		_BTF,SWITCH_NEW,SWITCH2,_S
				_if	_BTF,SWITCH_OLD,SWITCH2,_C
					bsf	SWITCH_OLD,SWITCH2
					bsf	GPIO,LED2
				_endif
			_else
				_if	_BTF,SWITCH_OLD,SWITCH2,_S
					bcf	SWITCH_OLD,SWITCH2
					bcf	GPIO,LED2
				_endif
			_endif

			bcf		INTCON,GPIF
		_endif
        
		_if		_BTF,INTCON,T0IF,_S
			bsf		OPTION_REG,T0CS
			VTIMER0_CHECK
			_if		_BTF,VTMR0INT,VTMR00ID,_S
				bcf	VTMR0INT,VTMR00ID
				VTIMER0_ON	VTMR00ID,VTIMER0_LED1_RELOAD
			_endif
			_if		_BTF,VTMR0INT,VTMR01ID,_S
				bcf	VTMR0INT,VTMR01ID
				VTIMER0_ON	VTMR01ID,VTIMER0_LED2_RELOAD
			_endif

			VTIMER0_START
			banksel	INTCON
			bcf		INTCON,T0IF
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
