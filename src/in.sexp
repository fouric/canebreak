(file "test.c")
(intel-syntax noprefix)
(text)
(globl main)
(type main @function)

(section main
    (push rbp)
    (mov rbp rsp)
    (mov "DWORD PTR [rbp-4]" 42)
    (mov eax "DWORD PTR [rbp-4]")
    (pop rbp)
    (ret))

(size main .-main)