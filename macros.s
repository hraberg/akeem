        .include "constants.s"

        .macro assert_equals expected actual
        .if (\expected != \actual)
        .error "Assertion failed: \expected \actual"
        .endif
        .endm

        .macro call_fn fn arg1 arg2 arg3 arg4 arg5 arg6
        .ifnb \arg6
        mov   \arg6, %r9
        .endif
        .ifnb \arg5
        mov    \arg5, %r8
        .endif
        .ifnb \arg4
        mov    \arg4, %rcx
        .endif
        .ifnb \arg3
        mov    \arg3, %rdx
        .endif
        .ifnb \arg2
        mov    \arg2, %rsi
        .endif
        .ifnb \arg1
        mov    \arg1, %rdi
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
        movq    \value, %rax
        .endif
        leave
        ret
        .endm

        .macro unbox_int_internal int
        movsx   \int, %rax
        .endm

        .macro unbox_pointer_internal ptr
        mov     $PAYLOAD_MASK, %rax
        and     \ptr, %rax
        .endm

        .macro is_int_internal value tmp=%r11
        mov     $TAG_MASK, \tmp
        and     \value, \tmp
        mov     $TAG_INT, %rax
        cmp     %rax, \tmp
        sete    %al
        and     $C_TRUE, %rax
        .endm

        .macro box_int_internal value tmp=%r11
        mov     \value, %eax
        mov     $(NAN_MASK | TAG_INT), \tmp
        or      \tmp, %rax
        .endm

        .macro tagged_jump table
        enter_fn
        mov     %rdi, %rbx
        call    is_double
        mov     %rbx, %rdi
        xor     %r11, %r11
        test    $C_TRUE, %rax
        cmovnz  %r11, %rbx
        mov     $TAG_MASK, %rax
        and     %rbx, %rax
        shr     $TAG_SHIFT, %rax
        call    *\table(,%rax,POINTER_SIZE)
        return
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
