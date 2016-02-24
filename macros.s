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

        .macro local_variables local:req locals:vararg
        .equ \local, local_offset
        .equ local_offset, local_offset + POINTER_SIZE
        .ifnb \locals
        local_variables \locals
        .endif
        .endm

        .macro prologue locals:vararg
        .equ local_offset, 0
        .equ callee_saved_size, POINTER_SIZE * 1
        .ifnb \locals
        local_variables \locals
        .endif
        .equ stack_frame_size, (8 + (callee_saved_size + 8 + local_offset) & -16)
        sub     $stack_frame_size, %rsp
        mov     %rbx, local_offset(%rsp)
        .endm

        .macro return value1=%rax value2=%rdx
        mov_reg \value1, %rax
        mov_reg \value2, %rdx
        mov     local_offset(%rsp), %rbx
        add     $stack_frame_size, %rsp
        ret
        .endm

        .macro unbox_pointer_internal ptr to=%rax
        mov     $PAYLOAD_MASK, \to
        and     \ptr, \to
        .endm

        .macro eq_internal x y
        cmp     \x, \y
        sete    %al
        and     $C_TRUE, %rax
        .endm

        .macro box_int_internal value tmp=%r11
        .ifnc \value, %eax
        mov     \value, %eax
        .endif
        tag     TAG_INT, %rax
        .endm

        .macro box_boolean_internal value
        tag     TAG_BOOLEAN, \value
        .endm

        .macro tag tag value target=%rax tmp=%r11
        .ifnc \value, \target
        mov     \value, \target
        .endif
        mov     $(NAN_MASK | \tag << TAG_SHIFT), \tmp
        or      \tmp, \target
        .endm

        .macro has_tag tag value
        mov     \value, %rax
        shr     $TAG_SHIFT, %rax
        eq_internal $(\tag | NAN_MASK >> TAG_SHIFT), %eax
        .endm

        .macro is_double_internal value tmp=%r11
        mov     \value, \tmp
        btr     $SIGN_BIT, \tmp
        mov     $NAN_MASK, %rax
        cmp     %rax, \tmp
        setle   %al
        and     $C_TRUE, %rax
        .endm

        .macro tagged_jump table
        is_double_internal %rdi
        mov     $0, %rax
        cmovz   %rdi, %rax
        shr     $TAG_SHIFT, %rax
        and     $TAG_MASK, %rax
        call    *\table(,%rax,POINTER_SIZE)
        .endm

        .macro binary_op name double_op integer_op
        has_tag TAG_INT, %rdi
        mov     %rax, %rdx
        has_tag TAG_INT, %rsi
        shl     %rax
        or      %rdx, %rax
        shl     $4, %rax
        lea     \name\()_double_double(%rax), %rax
        jmp     *%rax
1:      \double_op %xmm1, %xmm0
        movq    %xmm0, %rax
        ret
        .align 16
\name\()_double_double :
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        jmp     1b
        .align 16
\name\()_int_double :
        cvtsi2sd %edi, %xmm0
        movq    %rsi, %xmm1
        jmp     1b
        .align 16
\name\()_double_int :
        movq    %rdi, %xmm0
        cvtsi2sd %esi, %xmm1
        jmp     1b
        .align 16
\name\()_int_int :
        mov     %edi, %eax
        \integer_op %esi, %eax
        box_int_internal %eax
        ret

        .endm

        .macro lookup_global_symbol_internal symbol_id
        mov     symbol_table_values(,\symbol_id,POINTER_SIZE), %rax
        .endm
