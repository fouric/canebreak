(program
  (section foo
    add r1 r1 r2
    (lisp
      (print "hello world"))
    jr ra))