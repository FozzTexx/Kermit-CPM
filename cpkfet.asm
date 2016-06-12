        ORG 100H	;Begin assembling code at 100
	LXI H,2FEH	;Where to store in memory
	SHLD 200H	;Keep pointer there
	MVI E,0DH	;Get a CR
	MVI C,4		;Output to PUNCH {send to HOST}
	CALL 5
	MVI C,3		;Input from READER {read from HOST}
	CALL 5
	ANI 7FH		;Strip parity bit
	PUSH PSW	;save a and flags
	MOV E,A		;move char to e for echo
	MVI C,2		;Output to screen
	CALL 5
	POP PSW		;restore a and flags
	CPI 40H		;Is it our DEC-20 prompt?
	JZ 127H		;Yes, have whole file in memory
	CALL 17AH	;no , store another byte
	JMP 10DH	;read another byte
	MVI A,1AH	;Get a Control-Z {CP/M EOF mark}
	CALL 17AH	;store it in memory
	LXI H,300H	;Get memory pointer
	SHLD 202H	;Store as DMA pointer
	LDA 201H	;Get 'HI' byte of memory pointer
	STA 200H	;and store it as 'LO' one
	XRA A
	STA 201H	;Zero 'HI' byte {slow *256}
	MVI C,16H	;Make NEW file
	LXI D,5CH	;With FCB1
	CALL 5
	CALL 15EH	;Write 128 bytes {sector}
	CALL 15EH	;Write another sector
	LXI H,0FFFFH	;Get a 16-bit Minus One
	XCHG		;into DE
	LHLD 200H	;Get 256-byte counter
	DAD D		;decrement
	SHLD 200H	;and store back
	MVI A,2		;Check if
	CMP L		; 256-byte counter down to offset
	JZ 183H		;Yes, wer'e done
	JMP 144H	;Keep writing..
	LHLD 202H	;Get file-pointer
	XCHG		;into DE
	MVI C,1AH	;Set DMA-address
	CALL 5
	MVI C,15H	;Write sector {128 bytes}
	LXI D,5CH	;using FCB1
	CALL 5
	LHLD 202H	;Get file-pointer
	LXI D,80H	;128-bytes
	DAD D		;added to file-pointer
	SHLD 202H	;and save
	RET		;and return
	LHLD 200H	;Get Memory-pointer
	MOV M,A		;store character
	INX H		;Increment Pointer
	SHLD 200H	;and save
	RET		;and return
	MVI C,10H	;CLOSE file
	LXI D,5CH	;using FCB1
	CALL 5
	JMP 0		;Force WARM BOOT
