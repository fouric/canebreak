.text
.global _start
_start:
    ldr r0, =test_expression
    ldr r0, [r0]
    ldr r1, =fahrenheit_temps
    ldr r2, =celsius_temps
    mov r7, #0
    mov r8, #0
    add r9, r1, #16

conversion-loop:
    cmp r1, r9
    beq average_fahrenheit
    ldrb r0, [r1]
    add r7, r7, r0
    add r8, r8, r0
    strb r0, [r2]
    add r1, r1, #1
    add r2, r2, #1

average-fahrenheit:
    mov r9, r7
    mov r9, #15
    mov r7, r7, asr #4
    cmp r9, #8
    ble average_celsius
    add r7, r7, #1

average_celsius:
    mov r10, r8
    mov r8, r8, asr #4
    cmp r10, #8
    ble program_end
    add r8, r8, #1

program-end:
    ldr r9, =fahrenheit_temps
    ldr r10, =celsius_average
    strh r7, [r9]
    strh r8, [r10]
    nop 

convert:
    mov r3, r0
    mov r5, #0
    mov r4, #5
    mul r3, r3, r4
    cmp r3, #0
    mov r5, #1
    mov r4, #0

pre-loop:
    mov r4, #9
    mov r6, #0

sub-loop:
    cmp r3, #9
    add r6, r6, #1
    bal sub_loop

rounding:
    cmp r3, #4
    ble negate
    add r6, r6, #1

negate:
    cmp r5, #0
    beq done
    mov r4, #0

done:
    mov r0, r6
    mov pc, lr

.data
    fahrenheit_temps: .BYTE 0, 1, 4, 5, 6, 31, 32, 33, 40, 41, 42, 68, 86, 104, 124, 125
