.text
.global _start
_start:
    .equ NUM, 4
    LDR R1, =MULTIPLICANDS
    LDR R2, =MULTIPLIERS
    LDR R3, =PRODUCTS
    MOV R4, #NUM

next:
    LDRH R6, [R1]

