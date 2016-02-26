        .include "constants.s"

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
        .equ stack_frame_size, (POINTER_SIZE + (callee_saved_size + POINTER_SIZE + local_offset) & -(2 * POINTER_SIZE))
        .if stack_frame_size > POINTER_SIZE
        sub     $stack_frame_size, %rsp
        .endif
        mov     %rbx, local_offset(%rsp)
        .endm

        .macro minimal_prologue
        .equ stack_frame_size, POINTER_SIZE
        sub     $stack_frame_size, %rsp
        .endm

        .macro return value1=%rax value2=%rdx
        .equ callee_saved_size, POINTER_SIZE * 1
        mov_reg \value1, %rax
        mov_reg \value2, %rdx
        .if stack_frame_size > POINTER_SIZE
        mov     local_offset(%rsp), %rbx
        .endif
        add     $stack_frame_size, %rsp
        ret
        .endm

        .macro unbox_pointer_internal ptr to=%rax tmp=%r11
        .ifc \ptr, \to
        mov     $PAYLOAD_MASK, \tmp
        and     \ptr, \tmp
        mov     \tmp, \to
        .else
        mov     $PAYLOAD_MASK, \to
        and     \ptr, \to
        .endif
        .endm

        .macro eq_internal x y store=true
        cmp     \x, \y
        .ifc \store, true
        sete    %al
        and     $C_TRUE, %rax
        .endif
        .endm

        .macro box_int_internal value=%eax tmp=%r11
        mov     \value, %eax
        tag     TAG_INT, %rax, %rax, \tmp
        .endm

        .macro box_boolean_internal value=%rax
        tag     TAG_BOOLEAN, \value
        .endm

        .macro tag tag value=%rax target=%rax tmp=%r11
        mov_reg \value, \target
        mov     $(NAN_MASK | \tag << TAG_SHIFT), \tmp
        or      \tmp, \target
        .endm

        .macro has_tag tag value=%rax store=true
        mov_reg \value, %rax
        shr     $TAG_SHIFT, %rax
        .if (\tag >= TAG_SYMBOL)
        and     $(~(TAG_SYMBOL - 1)), %al
        .endif
        eq_internal $(\tag | NAN_MASK >> TAG_SHIFT), %eax, \store
        .endm

        .macro is_double_internal value tmp=%r11
        mov     \value, \tmp
        btr     $SIGN_BIT, \tmp
        mov     $NAN_MASK, %rax
        cmp     %rax, \tmp
        setle   %al
        and     $C_TRUE, %rax
        .endm

        .macro store_pointer idx ptr at=%rbx
        mov     \idx, %ecx
        movq    \ptr, (\at,%rcx,POINTER_SIZE)
        .endm

        .macro tagged_jump table
        is_double_internal %rdi
        mov     $0, %rax
        cmovz   %rdi, %rax
        shr     $TAG_SHIFT, %rax
        and     $TAG_MASK, %rax
        mov     $(~(TAG_SYMBOL - 1)), %r11b
        mov     $0xff, %cl
        test    %r11b, %al
        cmovnz  %r11, %rcx
        and     %cl, %al
        call    *\table(,%rax,POINTER_SIZE)
        .endm

        .macro binary_op_jump name
        has_tag TAG_INT, %rdi
        mov     %al, %dl
        has_tag TAG_INT, %rsi
        shl     %al
        or      %edx, %eax
        shl     $BINARY_OP_SHIFT, %al
        lea     \name\()_double_double(%eax), %rax
        jmp     *%rax
        .endm

        .macro binary_op_moves name
        .align  (1 << BINARY_OP_SHIFT)
\name\()_double_double:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        jmp     \name\()_op
        .align  (1 << BINARY_OP_SHIFT)
\name\()_int_double:
        cvtsi2sd %edi, %xmm0
        movq    %rsi, %xmm1
        jmp     \name\()_op
        .align  (1 << BINARY_OP_SHIFT)
\name\()_double_int:
        movq    %rdi, %xmm0
        cvtsi2sd %esi, %xmm1
        jmp     \name\()_op
        .align  (1 << BINARY_OP_SHIFT)
        .endm

        .macro binary_op name double_op integer_op
        binary_op_jump \name
\name\()_op:
        \double_op %xmm1, %xmm0
        movq    %xmm0, %rax
        ret
        binary_op_moves \name
        .ifnb \integer_op
\name\()_int_int:
        mov     %edi, %eax
        \integer_op %esi, %eax
        box_int_internal
        ret
        .endif
        .endm

        .macro binary_comparsion name double_setter integer_setter
        binary_op_jump \name
\name\()_op:
        xor     %eax, %eax
        comisd  %xmm1, %xmm0
        \double_setter %al
        box_boolean_internal %rax
        ret
        binary_op_moves \name
\name\()_int_int:
        xor     %eax, %eax
        cmp     %esi, %edi
        \integer_setter %al
        box_boolean_internal
        ret
        .endm

        .macro integer_division
        has_tag TAG_INT, %rdi, store=false
        je      1f
        movd    %rdi, %xmm0
        cvtsd2si %xmm0, %rdi
1:      has_tag TAG_INT, %rsi, store=false
        je      2f
        movq    %rsi, %xmm0
        cvtsd2si %xmm0, %rsi
2:      mov     %edi, %eax
        cdq
        idiv    %esi
        .endm

        .macro maybe_round_to_int from=%xmm0 tmp=%xmm1
        roundsd $ROUNDING_MODE_TRUNCATE, \from, \tmp
        ucomisd \from, \tmp
        je      1f
        movq    \from, %rax
        jmp     2f
1:      cvtsd2si \tmp, %rax
        box_int_internal
2:      nop
        .endm

        .macro math_library_unary_call name round=false
        movq    %rdi, %xmm0
        has_tag TAG_INT, %rdi, store=false
        jne     \name\()_double
\name\()_int:
        cvtsi2sd %edi, %xmm0
\name\()_double:
        minimal_prologue
        call_fn \name
        movq    %xmm0, %rax
        .ifc \round, true
        maybe_round_to_int
        .endif
        return
        .endm

        .macro math_library_binary_call name round=false
        binary_op_jump \name
\name\()_op:
        minimal_prologue
        call_fn \name
        .ifc \round, true
        maybe_round_to_int
        .else
        movq    %xmm0, %rax
        .endif
        return
        binary_op_moves \name
\name\()_int_int:
        cvtsi2sd %edi, %xmm0
        cvtsi2sd %esi, %xmm1
        jmp     \name\()_op
        .endm

        .macro call_with_file_template name
        prologue port
        unbox_pointer_internal %rsi, %rbx
        call_fn open_\name\()_file, %rdi
        mov     %rax, port(%rsp)
        call_fn *%rbx, %rax
        mov     %rax, %rbx
        call_fn close_\name\()_port, port(%rsp)
        return  %rbx
        .endm

        .macro with_file_io_template name stream
        prologue previous_port
        unbox_pointer_internal %rsi, %rbx
        mov     \stream, %rax
        mov     %rax, previous_port(%rsp)
        call_fn open_\name\()_file, %rdi
        mov     %rax, \stream
        call_fn *%rbx
        mov     %rax, %rbx
        call_fn close_\name\()_port, \stream
        mov     previous_port(%rsp), %rax
        mov     %rax, \stream
        return  %rbx
        .endm

        .macro register_for_gc ptr=%rax
        nop
        .endm

        .macro lookup_global_symbol_internal symbol_id
        mov     symbol_table_values(,\symbol_id,POINTER_SIZE), %rax
        .endm

        .macro define symbol_name, value, tmp=%rbx
        .data
symbol_string_\@:
        .string "\symbol_name"
        .text
        mov     \value, \tmp
        call_fn string_to_symbol, $symbol_string_\@
        call_fn set, %rax, \tmp
        mov     (symbol_next_id), %rax
        dec     %rax
        .endm

        .macro perror success=jg
        cmp    $NULL, %rax
        \success 1f
        call_fn perror, $NULL
        call_fn exit, $1
1:      nop
        .endm
