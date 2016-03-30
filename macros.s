        .include "constants.s"

        .macro mov_reg from, to
        .ifnb \from
        .ifnc \from, \to
        movq   \from, \to
        .endif
        .endif
        .endm

        .macro call_fn fn, arg1, arg2, arg3, arg4, arg5, arg6
        mov_reg \arg6, %r9
        mov_reg \arg5, %r8
        mov_reg \arg4, %rcx
        mov_reg \arg3, %rdx
        mov_reg \arg2, %rsi
        mov_reg \arg1, %rdi
        call \fn
        .endm

        .macro local_variables local:req, locals:vararg
        .equ \local, local_offset
        .equ local_offset, local_offset + POINTER_SIZE
        .ifnb \locals
        local_variables \locals
        .endif
        .endm

        .macro prologue locals:vararg
        .equ local_offset, 0
        .equ callee_saved_size, POINTER_SIZE * 2
        .ifnb \locals
        local_variables \locals
        .endif
        .equ stack_frame_size, (POINTER_SIZE + (callee_saved_size + POINTER_SIZE + local_offset) & -(2 * POINTER_SIZE))
        .if stack_frame_size > POINTER_SIZE
        sub     $stack_frame_size, %rsp
        .endif
        mov     %rbx, local_offset(%rsp)
        mov     %r12, local_offset+POINTER_SIZE(%rsp)
        .endm

        .macro minimal_prologue
        .equ stack_frame_size, POINTER_SIZE
        .equ callee_saved_size, 0
        sub     $stack_frame_size, %rsp
        .endm

        .macro return value1=%rax, value2=%rdx
        mov_reg \value1, %rax
        mov_reg \value2, %rdx
        .if callee_saved_size > 0
        mov     local_offset+POINTER_SIZE(%rsp), %r12
        mov     local_offset(%rsp), %rbx
        .endif
        add     $stack_frame_size, %rsp
        ret
        .endm

        .macro unbox_pointer_internal ptr, to=%rax
        mov_reg \ptr, \to
        shl     $PAYLOAD_SHIFT, \to
        shr     $PAYLOAD_SHIFT, \to
        .endm

        .macro car from, to=%rax
        unbox_pointer_internal \from \to
        mov     pair_car(\to), \to
        .endm

        .macro cdr from, to=%rax
        unbox_pointer_internal \from \to
        mov     pair_cdr(\to), \to
        .endm

        .macro eq_internal x, y, store=true
        cmp     \x, \y
        .ifc \store, true
        sete    %al
        and     $C_TRUE, %eax
        .endif
        .endm

        .macro box_int_internal value=%eax, tmp=%r11
        mov     \value, %eax
        tag     TAG_INT, %rax, %rax, \tmp
        .endm

        .macro box_boolean_internal value=%rax
        tag     TAG_BOOLEAN, \value
        .endm

        .macro tag tag value=%rax, target=%rax, tmp=%r11
        mov_reg \value, \target
        mov     $(NAN_MASK | \tag << TAG_SHIFT), \tmp
        or      \tmp, \target
        .endm

        .macro has_tag tag, value=%rax, store=true
        mov_reg \value, %rax
        shr     $TAG_SHIFT, %rax
        .if (\tag >= TAG_SYMBOL)
        and     $POINTER_TAG_MASK, %al
        .endif
        eq_internal $(\tag | NAN_MASK >> TAG_SHIFT), %eax, \store
        .endm

        .macro is_double_internal value, tmp=%r11, tmp2=%rax, store=true
        mov     \value, \tmp
        btr     $SIGN_BIT, \tmp
        mov     $NAN_MASK, \tmp2
        cmp     \tmp2, \tmp
        .ifc \store, true
        setle   %al
        and     $C_TRUE, %eax
        .endif
        .endm

        .macro assert_tag tag, value, error
        has_tag \tag, \value, store=false
        je      .L_\@_1
        mov     $error, %r11
        call_fn *%r11, \error, \value
.L_\@_1:
        .endm

        .macro assert_pair value, error=not_a_pair_string
        is_nil_internal \value
        je      .L_\@_1
        has_tag TAG_PAIR, \value, store=false
        je      .L_\@_2
.L_\@_1:
        mov     $error, %r11
        call_fn *%r11, \error, \value
.L_\@_2:
        .endm

        .macro assert_object value, class, error
        has_tag TAG_OBJECT, \value, store=false
        jne     .L_\@_1
        unbox_pointer_internal \value, %r11
        test    %r11, %r11
        jz      .L_\@_1
        mov     header_object_type(%r11), %ax
        cmp     $\class, %ax
        je      .L_\@_2
.L_\@_1:
        mov     $error, %r11
        call_fn *%r11, \error, \value
.L_\@_2:
        .endm

        .macro is_nil_internal value, tmp=%r11, store=false
        mov     $NIL, \tmp
        eq_internal \value, \tmp, store=\store
        .endm

        .macro is_void_internal value, tmp=%r11, store=false
        mov     $VOID, \tmp
        eq_internal \value, \tmp, store=\store
        .endm

        .macro is_eof_object_internal value, tmp=%r11, store=false
        mov     $EOF_OBJECT, \tmp
        eq_internal \value, \tmp, store=\store
        .endm

        .macro store_pointer idx, ptr=%rax, at=%rbx
        mov     \idx, %ecx
        mov     \ptr, %rax
        movq    %rax, (\at,%rcx,POINTER_SIZE)
        .endm

        .macro extract_tag from=%rdi
        is_double_internal \from, store=false
        mov     $TAG_DOUBLE, %eax
        cmovg   \from, %rax
        shr     $TAG_SHIFT, %rax
        and     $TAG_MASK, %eax
        mov     $POINTER_TAG_MASK, %r11b
        mov     $TAG_MASK, %r9b
        test    %r11b, %al
        cmovnz  %r11w, %r9w
        and     %r9b, %al
        .endm

        .macro tagged_jump table receiver=%rdi
        extract_tag \receiver
        call    *\table(,%rax,POINTER_SIZE)
        .endm

        .macro extract_binary_op
        has_tag TAG_INT, %rdi, store=false
        sete    %r11b
        has_tag TAG_INT, %rsi, store=false
        sete    %al
        shl     %al
        or      %r11b, %al
        and     $BINARY_OP_MASK, %eax
        .endm

        .macro binary_op_jump name
        extract_binary_op
        shl     $BINARY_OP_SHIFT, %al
        lea     \name\()_double_double(%eax), %rax
        jmp     *%rax
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

        .macro binary_op name, double_op, integer_op
        binary_op_jump \name
\name\()_int_int:
        mov     %edi, %eax
        \integer_op %esi, %eax
        box_int_internal
        jmp     \name\()_return
\name\()_op:
        \double_op %xmm1, %xmm0
        movq    %xmm0, %rax
\name\()_return:
        ret
        .endm

        .macro binary_comparsion name, double_setter, integer_setter
        binary_op_jump \name
\name\()_int_int:
        xor     %eax, %eax
        cmp     %esi, %edi
        \integer_setter %al
        jmp     \name\()_return
\name\()_op:
        xor     %eax, %eax
        comisd  %xmm1, %xmm0
        \double_setter %al
\name\()_return:
        box_boolean_internal
        ret
        .endm

        .macro integer_division
        has_tag TAG_INT, %rdi, store=false
        je      .L_\@_1
        movd    %rdi, %xmm0
        cvtsd2si %xmm0, %rdi
.L_\@_1:
        has_tag TAG_INT, %rsi, store=false
        je      .L_\@_2
        movq    %rsi, %xmm0
        cvtsd2si %xmm0, %rsi
.L_\@_2:
        mov     %edi, %eax
        cdq
        idiv    %esi
        .endm

        .macro maybe_round_to_int from=%xmm0, tmp=%xmm1
        roundsd $ROUNDING_MODE_TRUNCATE, \from, \tmp
        ucomisd \from, \tmp
        je      .L_\@_1
        movq    \from, %rax
        jmp     .L_\@_2
.L_\@_1:
        cvtsd2si \tmp, %rax
        box_int_internal
.L_\@_2:
        .endm

        .macro math_library_unary_call name, round=false, return_int=false
        minimal_prologue
        movq    %rdi, %xmm0
        has_tag TAG_INT, %rdi, store=false
        jne     \name\()_double
\name\()_int:
        .ifc \return_int, true
        mov     %rdi, %rax
        jmp     \name\()_return
        .else
        cvtsi2sd %edi, %xmm0
        .endif
\name\()_double:
        call_fn \name
        movq    %xmm0, %rax
        .ifc \round, true
        maybe_round_to_int
        .endif
\name\()_return:
        return
        .endm

        .macro math_library_binary_call name, round=false
        minimal_prologue
        binary_op_jump \name
\name\()_int_int:
        cvtsi2sd %edi, %xmm0
        cvtsi2sd %esi, %xmm1
\name\()_op:
        call_fn \name
        .ifc \round, true
        maybe_round_to_int
        .else
        movq    %xmm0, %rax
        .endif
        return
        .endm

        .macro with_file_io_template name, stream
        prologue previous_port
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_tag TAG_PROCEDURE, %rsi, not_a_procedure_string
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

        .macro open_input_buffer_template size_adjust, tag, error
        prologue empty_stream, empty_stream_size
        assert_tag \tag, %rdi, \error
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %esi
        add     \size_adjust, %esi
        test    %esi, %esi
        jz      .L_\@_1
        add     $header_size, %rax
        call_fn fmemopen, %rax, %rsi, $read_mode
        perror
        tag     TAG_PORT, %rax
        return

.L_\@_1:
        lea     empty_stream(%rsp), %rdi
        lea     empty_stream_size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        tag     TAG_PORT, %rax
        return
        .endm

        .macro patch_jump stream, target, origin, offset
        call_fn ftell, \stream
        mov     %rax, \target
        sub     \origin, %rax
        mov     %eax, \offset
        mov     \origin, %rax
        sub     $INT_SIZE, %rax
        call_fn fseek, \stream, %rax, $SEEK_SET
        lea     \offset, %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, \stream
        call_fn fseek, \stream, \target, $SEEK_SET
        .endm

        .macro default_arg tag, default, value, tmp=%r11
        mov     \default, \tmp
        has_tag \tag, \value, store=false
        cmovne  \tmp, \value
        .endm

        .macro lookup_global_symbol_internal symbol_id
        mov     symbol_table_values(,\symbol_id,POINTER_SIZE), %rax
        .endm

        .macro register_for_gc ptr=%rax
        call_fn push_pointer_on_stack, $object_space, \ptr
        .endm

        .macro perror success=jg
        cmp    $NULL, %rax
        \success .L_\@_1
        call_fn perror, $NULL
        call_fn exit, $1
.L_\@_1:
        .endm

        .macro read_number_template radix unget=false
        prologue
        mov     %rdi, %rbx
        mov     %rsi, %rdi
        .ifc \unget, true
        call_fn ungetc, %rdi, %rbx
        .endif
        call_fn read_token, %rbx
        register_for_gc
        call_fn string_to_number, %rax, \radix
        return
        .endm

        .macro open_string_buffer str, size, stream
        lea     \str, %rdi
        lea     \size, %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, \stream
        call_fn fseek, \stream, $header_size, $SEEK_SET
        .endm

        .macro string_buffer_to_string str, size, stream
        call_fn fclose, \stream
        perror  je
        mov     \str, %rax
        movw    $TAG_STRING, header_object_type(%rax)
        mov     \size, %r11d
        sub     $(header_size - 1), %r11d
        mov     %r11d, header_object_size(%rax)
        tag     TAG_STRING, %rax
        .endm

        .macro read_byte_jump table, stream=%rbx byte=%rax, tmp=%r11
        mov     \table(,\byte,POINTER_SIZE), \tmp
        test    \tmp, \tmp
        jnz     .L_\@_1
        mov_reg  \byte, %rax
        tag     TAG_CHAR, %rax
        call_fn error, read_error_string, %rax
        jmp     .L_\@_2
.L_\@_1:
        call_fn *\tmp, \stream, \byte
.L_\@_2:
        .endm

        .macro intern_string var, name
        .section .rodata
        .align  16
\var\()_c:
        .string "\name"
        .align  16
        .data
\var:
        .quad   0
        .text
        call_fn box_string, $\var\()_c
        mov     %rax, \var
        .endm

        .macro intern_symbol var, name, id=
        intern_string \var, "\name"
        .ifnb \id
        mov     $\id, %r11
        mov     %rax, symbol_table_names(,%r11,POINTER_SIZE)
        .endif
        call_fn string_to_symbol, %rax
        mov     %rax, \var
        .endm

        .macro define name, value, tag=TAG_PROCEDURE
        .section .rodata
        .align  16
tmp_string_\@:
        .string "\name"
        .text
        call_fn box_string, $tmp_string_\@
        call_fn string_to_symbol, %rax
        .ifnb \tag
        tag    \tag, \value, target=%rcx
        .endif
        mov     %rcx, symbol_table_values(,%eax,POINTER_SIZE)
        .endm

        .macro update_max_locals max_locals, value=%rax, tmp=%r11
        mov     \max_locals, \tmp
        cmp     \value, \tmp
        cmovl   \value, \tmp
        mov     \tmp, \max_locals
        .endm

        .macro string_comparator comparator, setter, string1=%rdi, string2=%rsi
        prologue
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_tag TAG_STRING, %rsi, not_a_string_string
        unbox_pointer_internal \string1
        add     $header_size, %rax
        mov     %rax, %rdi
        unbox_pointer_internal \string2, %rsi
        add     $header_size, %rsi

        xor     %ebx, %ebx
        call_fn \comparator, %rdi, %rsi
        \setter %bl
        box_boolean_internal %rbx
        return
        .endm
