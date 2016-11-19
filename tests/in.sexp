(
(text)
(global _start)

(label _start
  (equ num 4)
  (ldr r1 =multiplicands)
  (ldr r2 =multipliers)
  (ldr r3 =products)
  (mov r4 \#num)
  (label inner
    (mov r1 r0)))

(label next
  (ldrh r6 (r1)))
)