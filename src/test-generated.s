.text
.global _start
_start:
    .equ NUM, 4
    LDR R1, =multiplicands
    LDR R2, =multipliers
    LDR R3, =products
    MOV R4, #num

next:
    LDRH R6, [R1]
    LDRH R7, [R2]
    MUL R8, R6, R7
    STR R8, [R3]
    ADD R1, R1, #2
    ADD R2, R2, #2
    ADD R3, R3, #4
    SUBS R4, #1
    BNE next
    NOP 

.data
    multiplicands: .HWORD 0x1111, 0x2222, 0x3333, 0x4444
    multipliers: .HWORD 0x1111, 0x2222, 0x3333, 0x4444
    products: .WORD 0x0, 0x0, 0x0, 0x0
.end
