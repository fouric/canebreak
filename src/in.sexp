(
(text)
(global _start)

(label _start
  (equ num 4)
  (ldr r1 =multiplicands)
  (ldr r2 =multipliers)
  (ldr r3 =products)
  (mov r4 "#NUM"))

(label next
  (ldrh r6 "[R1]"))
)