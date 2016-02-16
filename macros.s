        .macro assert_equals expected actual
        .if (\expected != \actual)
        .error "Assertion failed: \expected \actual"
        .endif
        .endm

        .macro call_fn fn arg1 arg2 arg3 arg4 arg5 arg6
        .ifnb \arg1
        mov    \arg1, %rdi
        .endif
        .ifnb \arg2
        mov    \arg2, %rsi
        .endif
        .ifnb \arg3
        mov    \arg3, %rdx
        .endif
        .ifnb \arg4
        mov    \arg4, %rcx
        .endif
        .ifnb \arg5
        mov    \arg5, %r8
        .endif
        .ifnb \arg6
        mov   \arg6, %r9
        .endif
        call \fn
        .endm

        .macro enter_fn locals
        push    %rbp
        mov     %rsp, %rbp
        .ifnb \locals
        sub     $((8 + \locals * 8) & -16), %rsp
        .endif
        .endm

        .macro return value
        .ifnb \value
        mov     \value, %rax
        .endif
        leave
        ret
        .endm

        .macro arraycopy from to size
        mov     \from, %rsi
        mov     \to, %rdi
        mov     \size, %rcx

        lea     (%rsi, %rcx), %rsi
        lea     (%rdi, %rcx), %rdi
        neg     %rcx
1:
        mov     (%rsi, %rcx), %rax
        mov     %rax, (%rdi, %rcx)
        inc     %rcx
        jnz 1b
        .endm
