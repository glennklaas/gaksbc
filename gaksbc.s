;GAKMON v1.0 - 6502 Monitor

.debuginfo +
.setcpu "6502"

;-------------------------------------------------------------------------
;  ACIA ADDRESSES
;-------------------------------------------------------------------------

ACIA 		:= $A000	;BASE ADDRESS OF THE MC68B50P
ACIAControl	:= ACIA+0	; Set operational parameters (w)     
ACIAStatus 	:= ACIA+0	; Indicates if char has been received (r)
ACIAData 	:= ACIA+1	; Data being recieved (r) or sent (w)

;-------------------------------------------------------------------------
;  VARIABLES
;-------------------------------------------------------------------------

XAML		= $10		;LAST "OPENED" LOCATION LOW
XAMH		= $11		;LAST "OPENED" LOCATION HIGH
STL		    = $12		;STORE ADDRESS LOW
STH		    = $13		;STORE ADDRESS HIGH
L		    = $14		;HEX VALUE PARSING LOW
H		    = $15		;HEX VALUE PARSING HIGH
YSAV		= $16		;USED TO SEE IF HEX VALUE IS GIVEN
MODE		= $17		;$00=XAM, $7F=STOR, $AE=BLOCK XAM
STRL		= $18		;STRING START ADDRESS
STRH		= $19		;STRING END ADDRESS
IN		    = $200		;INPUT BUFFER

;-------------------------------------------------------------------------
;  CONSTANTS 
;-------------------------------------------------------------------------

BS		    = $08		;CODE-BACKSPACE KEY
CR		    = $0D		;CODE-CARRIAGE RETURN
LF		    = $0A 		;CODE-LINE FEED
ESC		    = $1B		;CODE-ESC KEY
PROMPT		= '*'		;PROMPT
STACK_TOP	= $FF		;TOP OF THE STACK

;-------------------------------------------------------------------------
;  INITIAL SETUP   
;-------------------------------------------------------------------------
.segment "CODE"

RESET:	
    CLD 			    ;CLEAR DECIMAL MODE
	LDX #STACK_TOP		;SET THE TOP OF STACK VALUE
	TXS			        ;INITIALIZE STACK
	LDA #$95      		;Set ACIA:CLK/16,8-bits,1 stop
				        ;  RTS low TX INT disabled
	STA ACIAControl		;INITIALIZE THE ACIA

    ;DISPLAY WELCOME MESSAGE
	LDA #<MSG1		    ;DETERMINE LOW
	STA STRL		    ;SET LOW
	LDA #>MSG1		    ;DETERMINE HI
	STA STRH		    ;SET HI
   	JSR SHWMSG		    ;SHOW WELCOME

;-------------------------------------------------------------------------
;  COMMAND INPUT PROCESSING 
;-------------------------------------------------------------------------

SFTRST:	
    LDA #ESC       	    ;AUTO ESCAPE
NOTCR:	
    CMP #BS        	    ;BACKSPACE KEY?
	BEQ BAKSPACE   		;YES
	CMP #ESC       		;ESC?
	BEQ ESCAPE     		;YES
	INY            		;ADVANCE TEXT INDEX
	BPL NEXTCHAR   		;IF LINE LONGER THAN 127 FALL THRU

ESCAPE:	
    LDA #PROMPT    	    ;PRINT PROMPT CHARACTER
	JSR ECHO       		;OUTPUT IT

GETLINE: 
    JSR CRLF		    ;SEND CRLF TO SCREEN

	LDY #$01       		;INITIALIZE TEXT INDEX
BAKSPACE: 
    DEY
	BMI GETLINE    		;REINIT BEYOND START OF LINE
	LDA #' '       		;SPACE, OVERWITE CHAR
	JSR ECHO
	LDA #BS        		;BACKSPACE AGAIN
	JSR ECHO

NEXTCHAR: 
    JSR RDKEY    	    ;WAIT FOR KEYPRESS
	STA IN,Y       		;ADD TO TEXT BUFFER
	JSR ECHO       		;DISPLAY CHARACTER
	;JSR PRBYTE    		;DEBUG - PRINT HEX KEYVALUE
	CMP #CR        		;CR?
	BNE NOTCR      		;NO.

;-------------------------------------------------------------------------
;  COMMAND EXECUTION PROCESSING
;-------------------------------------------------------------------------

	LDY #$FF       		;RESET TEXT INDEX
	LDA #$00       		;DEFAULT TO EXAMINE MODE
	TAX            		;0->X

SETSTOR: 
    ASL			        ;LEAVES $74 IF SETTING STOR MODE

SETMODE: 
    STA MODE		    ;SET MODE FLAGS
    CMP #'.'			;ARE WE SETTING BLOCK XAM?
	BNE BLSKIP			;NO, SKIP WORK AROUND
	ORA #$80			;YES, SET THE HIGH BIT
	STA MODE			;UPDATE MODE

BLSKIP:	
    INY			        ;ADVACE TEXT INDEX

NEXTITEM: 
    LDA IN,Y		    ;GET CHARACTER
	CMP #CR
	BEQ GETLINE		    ;WE'RE DONE IF IT'S CR!
	CMP #'.'		    ; $2E 00101110
	BCC BLSKIP		    ;IGNORE EVERYTHING LESS THAN "."!
	BEQ SETMODE		    ;IF EQ, SET BLOCK XAM MODE ("."=$2E)
	CMP #':'		    ; $3A 00111010
	BEQ SETSTOR		    ;SET STOR MODE! #$3A WILL BECOM $74
	CMP #'R'
	BEQ RUN			    ;RUN THE PROGRAM! FORGET THE REST
	STX L			    ;CLEAR INPUT VALUE (X=0)
	STX H
	STY YSAV		    ;SAVE Y FOR COMPARISON

; HERE WE'RE TRYING TO PARSE A NEW HEX VALUE

NEXTHEX: 
    LDA IN,Y		    ;GET CHARACTER FOR HEX TEST
	EOR #$30		    ;MAP DIGITS 0-9
	CMP #$0A		    ;IS IT A DECIMAL DIGIT?
	BCC DIG			    ;YES, PROCESS THE DIGIT
	ADC #$88		    ;MAP LETTER "A"-"F" TO $FA-FF
	CMP #$FA		    ;IS IT A HEX LETTER?
	BCC NOTHEX		    ;NO, CHARACTER NOT HEX OR CONTINUE 

DIG:	
    ASL
	ASL			        ;HEX DIGIT TO MSD OF A
	ASL
	ASL

	LDX #$04		    ;SHIFT COUNT
HEXSHIFT: 
    ASL			        ;HEX DIGIT LEFT, MSB TO CARRY
	ROL L			    ;ROTATE INTO LSD
	ROL H			    ;ROTATE INTO MSD'S
	DEX			        ;DONE 4 SHIFTS?
	BNE HEXSHIFT		;NO, LOOP
	INY			        ;ADVANCE TEXT INDEX
	BNE NEXTHEX		    ;ALWAYS TAKEN
  
NOTHEX:	
    CPY YSAV		    ;WAS AT LEAST 1 HEX DIGIT GIVEN?
	BNE NOESCAPE		;NO! IGNORE ALL,START FROM SCRATCH
	JMP ESCAPE

NOESCAPE: 
    BIT MODE
	BVC NOTSTOR
	LDA L			    ;LSD'S OF HEX DATA
	STA (STL,X)		    ;STORE CUR 'STORE INDEX'(X=0)
	INC STL			    ;INCREMENT STORE INDEX
	BNE NEXTITEM		;NO CARRY!
	INC STH			    ;ADD CARRY TO 'STORE INDEX' HIGH
TONXTITM: 
    JMP NEXTITEM	    ;GET NEXT COMMAND ITEM

;-------------------------------------------------------------------------
;  RUN USERS PROGRAM FROM THE LAST OPENED LOCATION 
;-------------------------------------------------------------------------

RUN:   
    JSR ACTRUN		    ;RUN USER'S PRORAM
	JMP SFTRST		    ;SOFT RESET ON RETURN

ACTRUN:	
    JMP (XAML)

;-------------------------------------------------------------------------
;  We're not in Store mode
;-------------------------------------------------------------------------

NOTSTOR:	
    BMI XAMNEXT	        ;B7=0 FOR XAM, 1 FOR BLOCK XAM

;WE'RE IN XAM MODE NOW

	LDX #$02		    ;COPY 2 BYTES
SETADR:	
    LDA L-1,X		    ;COPY HEX DATA TO
	STA STL-1,X		    ;store index'
	STA XAML-1,X		;AND TO 'XAM INDEX'
	DEX			        ;NEXT OF 2 BYTES
	BNE SETADR		    ;LOOP UNLESS X=0

; PRINT ADDR & DATA FROM THIS ADDR, FALL THRU NEXT BNE

NXTPRNT:	
    BNE PRDATA	        ;NE MEANS NO ADDRESS TO PRINT
	JSR CRLF
	LDA XAMH		    ;OUTPUT HIGH-ORDER BYTE OF ADDR
	JSR PRBYTE
	LDA XAML		    ;OUTPUT LOW-ORDER BYTE OF ADDR
	JSR PRBYTE
	LDA #':'		    ;PRINT COLON
	JSR ECHO

PRDATA:	
    LDA #' '		    ;PRINT SPACE
	JSR ECHO
	LDA (XAML,X)		;GET DATA FROM ADDRESS(X=0)
	JSR PRBYTE		    ;OUTPUT IT IN HEX FORMAT
XAMNEXT:	
    STX MODE	        ;0->MODE(XAM MODE)
	LDA XAML		    ;SEE IF THERE'S MORE TO PRINT
	CMP L
	LDA XAMH
	SBC H
	BCS TONXTITM		;NOT LESS! NO MORE DATA TO OUTPUT

	INC XAML		    ;INCREMENT 'EXAMING INDEX'
	BNE MOD8CHK		    ;NO CARRY!
	INC XAMH

MOD8CHK:	
    LDA XAML	        ;IF ADDRESS MOD8=0 START NEW LINE
	AND #%00000111		;8 VALUES PER ROW
	BPL NXTPRNT		    ;ALWAYS TAKEN

;-------------------------------------------------------------------------
;  Subroutine to print a byte in A in hex form (destructive)
;-------------------------------------------------------------------------

PRBYTE:	
    PHA			        ;SAVE A FOR LSD
	LSR
	LSR
	LSR			        ;MSD TO LSD
	LSR
	JSR PRHEX		    ;OUTPUT HEX DIGIT
	PLA			        ;RESTORE A

; FALL THROUGH TO PRINT HEX ROUTING

;-------------------------------------------------------------------------
;  Subroutine to print a hexadecimal digit
;-------------------------------------------------------------------------

PRHEX:	
    AND #$0F		    ;MASK LSD FOR HEX PRINT
	ORA #'0'		    ;ADD "0"
	CMP #'9'+1		    ;DIGIT?
	BCC ECHO		    ;YES, OUTPUT IT
	ADC #$06		    ;ADD OFFSET FOR LETTER

; Fall through to print routine

;-------------------------------------------------------------------------
;  Subroutine to print a character to the terminal
;-------------------------------------------------------------------------

ECHO:
    PHA			        ;PUSH A TO STACK
	JSR COUT		    ;COUT
  	PLA			        ;PULL A FROM STACK
	RTS

SHWMSG:	
    LDY #$0			    ;INITIALIZE COUNTER
PRINT:	
    LDA (STRL),Y	    ;LOAD NEXT CHAR
	BEQ DONE		    ;GOTO DONE IF 0, ELSE
	JSR ECHO		    ;SEND CHAR TO SCREEN
	INY			        ;INCREMENT COUNTER
	BNE PRINT		    ;LOOP FOR NEXT CHAR
DONE:	
    RTS			        ;RETURN TO CALLER

CRLF:	
    LDA #LF       
	JSR ECHO		    ;SEND LF
	LDA #CR
	JSR ECHO            ;SEND CR
	RTS

MSG1:	
    .byte CR,LF
	.byte "WELCOME TO GAKMON V1.0"
	.byte CR,LF
	.byte "    By Glenn Klaas"
	.byte CR,LF,CR,LF,0

;-------------------------------------------------------------------------
; KRUSADER
;-------------------------------------------------------------------------
;.segment "KRUSADER"

;-------------------------------------------------------------------------
; CORE IO HANDLING ROUTINES
;-------------------------------------------------------------------------
.segment "IO"

COUT:	
    PHA
ACIAWAIT:	
    LDA ACIAStatus
	AND #2              ;MASK TDRE 
	CMP #2
	BNE ACIAWAIT
	PLA
	STA ACIAData
	RTS

MONRDKEY: 
    LDA ACIAStatus
	AND #1			    ;MASK RDRF
	CMP #1
	BNE NoDataIn
	LDA ACIAData
	SEC		    	    ; Carry set if key available
	RTS
NoDataIn:	
    CLC		            ; Carry clear if no key pressed
	RTS

RDKEY:	
    JSR MONRDKEY        ;Check if key was pressed
	BCC RDKEY		    ;If not, check again
	RTS

;-------------------------------------------------------------------------
;  Vector area
;-------------------------------------------------------------------------
.segment "VEC"

	.word RESET		    ;NMI 
	.word RESET		    ;RESET 
	.word RESET		    ;IRQ 
