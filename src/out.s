.file "test.c"
.intel_syntax noprefix
.text
.global main
.type main, @function
main:
    push rbp
    mov rbp, rsp
    mov DWORD PTR [rbp-4], 42
    mov eax, DWORD PTR [rbp-4]
    pop rbp
    ret 
0
NIL
