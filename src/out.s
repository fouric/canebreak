                    .file "test.c"
                    .intel_syntax noprefix
                    .text
                    .global main
                    .type main, @function
                        rbp
                        pushrbp
                        rbp
                        rsp
                        movrbprsp
                        DWORD PTR [rbp-4]
                        NIL
                        movDWORD PTR [rbp-4]
                        NIL
                        DWORD PTR [rbp-4]
                        movDWORD PTR [rbp-4]
                        rbp
                        poprbp
                        ret
                    5
                    NIL
