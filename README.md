# what canebreak?

canebreak is a tool that converts an s-expression syntax form of assembly to the standard GNU form of ARM assembly. Add it to your `quicklisp/local-projects` directory, install [cl-fouric](https://github.com/fouric/cl-fouric), `(ql:quickload :canebreak)`, then just `(canebreak #p"/path/to/input/file" #p"/path/to/output/file")`

# why canebreak?

Right now there is absolutely no reason to use canebreak for anything. For that matter, you *cannot* use canebreak for anything, as it is completely nonfunctional.

However, I would like to convince you that canebreak is worth watching and/or contributing to. I will attempt to do so by claiming that I am extremely interested in building an *ergonomic*, *expressive* high-level language-like syntax over assembly through the development of a series of *transparent layers* (= easy to understand and modify) which are translated to each other through a micropass-like "compiler" tool. Furthermore, I claim that I am motivated to actually invest effort into canebreak, because of my interest in both expressive, powerful, high-level languages and electrical engineering/hardware/low-level programming - and so, *eventually* I will invest some nontrivial effort into canebreak, and make it usable and perhaps even *useful* for you, the reader of this document.

As an aside: why not a different tool than canebreak that does the same thing? Well, I don't know of any other Lisp layers over assembly - contact me if you do.

# how canebreak?

Input:

````
(
(text)
(global _start)
(label _start
  ;; set up variables
  (ldr r0 =test-expression) ;; address of test word for debugging
  (ldr r0 (r0)) ;; load the test word into a register, and hope wireshark sees it
  (ldr r1 =fahrenheit-temps) ;; load the pointer for the Fahrenheit source array
  (ldr r2 =celsius-temps) ;; load the pointer for the Celsius destination array
  (mov r7 0) ;; Fahrenheit total for averaging at the end
  (mov r8 0) ;; Celsius total for averaging at the end
  (add r9 r1 16) ;; pointer to the byte past the end of the Fahrenheit array
  )
(label conversion-loop
  (cmp r1 r9)
  (beq average-fahrenheit)
  (ldrb r0 (r1))
  (add r7 r7 r0)
  (bl convert)
  (add r8 r8 r0)
  (strb r0 (r2))
  (add r1 r1 1)
  (add r2 r2 1)
  (bl conversion-loop))
(label average-fahrenheit
  (mov r9 r7)
  (mov r9 15)
  (mov r7 r7 "asr #4")
  (cmp r9 8)
  (ble average-celsius)
  (add r7 r7 1))
(label average_celsius
  (mov r10 r8)
  (and r10 15)
  (mov r8 r8 "asr #4")
  (cmp r10 8)
  (ble program-end)
  (add r8 r8 1))
(label program-end
  (ldr r9 =fahrenheit-temps)
  (ldr r10 =celsius-average)
  (strh r7 (r9))
  (strh r8 (r10))
  (nop))

(label convert
  (mov r3 r0)
  (mov r5 0)
  (sub r3 32)
  (mov r4 5)
  (mul r3 r3 r4)
  (cmp r3 0)
  (bpl pre-loop)
  (mov r5 1)
  (mov r4 0)
  (sub r3 r4 r3))
(label pre-loop
  (mov r4 9)
  (mov r6 0))
(label sub-loop
  (cmp r3 9)
  (blt rounding)
  (sub r3 r3 9)
  (add r6 r6 1)
  (bal sub-loop))
(label rounding
  (cmp r3 4)
  (ble negate)
  (add r6 r6 1))
(label negate
  (cmp r5 0)
  (beq done)
  (mov r4 0)
  (sub r6 r4 r6))
(label done
  (mov r0 r6)
  (mov pc lr)))
````

Output:

````assembly
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
````
