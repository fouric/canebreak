	.text
	.global _start
_start:
	.equ NUM, 4
	LDR R1, =MULTIPLICANDS
	...
	MOV R4, #NUM
NEXT:
	LDRH R6, [R1]
	...
	BNE NEXT
	NOP
	
	.data
MULTIPLICANDS:	 .HWORD 0x1111 0x2222 0x3333 0x4444
	.END
