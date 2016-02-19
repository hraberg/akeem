        .include "constants.s"

        .macro assert_equals expected actual
        .if (\expected != \actual)
        .error "Assertion failed: \expected \actual"
        .endif
        .endm

        .macro mov_reg from to
        .ifnb \from
        .ifnc \from, \to
        movq   \from, \to
        .endif
        .endif
        .endm

        .macro call_fn fn arg1 arg2 arg3 arg4 arg5 arg6
        mov_reg \arg6, %r9
        mov_reg \arg5, %r8
        mov_reg \arg4, %rcx
        mov_reg \arg3, %rdx
        mov_reg \arg2, %rsi
        mov_reg \arg1, %rdi
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
        mov_reg \value, %rax
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
        push    %rdi
        call    is_double
        pop     %rdi
        xor     %r11, %r11
        test    $C_TRUE, %rax
        cmovz   %rdi, %r11
        mov     $TAG_MASK, %rax
        and     %r11, %rax
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
