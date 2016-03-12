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

        .macro unbox_pointer_internal ptr, to=%rax, tmp=%r11
        .ifc \ptr, \to
        mov     $PAYLOAD_MASK, \tmp
        and     \ptr, \tmp
        mov     \tmp, \to
        .else
        mov     $PAYLOAD_MASK, \to
        and     \ptr, \to
        .endif
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

        .macro is_double_internal value, tmp=%r11, store=true
        mov     \value, \tmp
        btr     $SIGN_BIT, \tmp
        mov     $NAN_MASK, %rax
        cmp     %rax, \tmp
        .ifc \store, true
        setle   %al
        and     $C_TRUE, %eax
        .endif
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
        mov     $TAG_MASK, %cl
        test    %r11b, %al
        cmovnz  %r11w, %cx
        and     %cl, %al
        .endm

        .macro tagged_jump table
        extract_tag
        call    *\table(,%rax,POINTER_SIZE)
        .endm

        .macro binary_op_jump name
        has_tag TAG_INT, %rdi, store=false
        sete    %r11b
        has_tag TAG_INT, %rsi, store=false
        sete    %al
        shl     %al
        or      %r11b, %al
        and     $BINARY_OP_MASK, %eax
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

        .macro binary_op name, double_op, integer_op
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

        .macro binary_comparsion name, double_setter, integer_setter
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
        movq    %rdi, %xmm0
        has_tag TAG_INT, %rdi, store=false
        jne     \name\()_double
\name\()_int:
        .ifc \return_int, true
        mov     %rdi, %rax
        ret
        .else
        cvtsi2sd %edi, %xmm0
        .endif
\name\()_double:
        minimal_prologue
        call_fn \name
        movq    %xmm0, %rax
        .ifc \round, true
        maybe_round_to_int
        .endif
        return
        .endm

        .macro math_library_binary_call name, round=false
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

        .macro with_file_io_template name, stream
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
        call_fn error, read_error_string
        jmp     .L_\@_2
.L_\@_1:
        call_fn *\tmp, \stream, \byte
.L_\@_2:
        .endm

        .macro intern_string var, name
        .section .rodata
\var\()_c:
        .string "\name"
        .data
\var:
        .quad   0
        .text
        call_fn box_string, $\var\()_c
        mov     %rax, \var
        .endm

        .macro string_literal str
        .section .rodata
tmp_string_\@_c:
        .string "\str"
        .text
        call_fn box_string, $tmp_string_\@_c
        .endm

        .macro intern_double var, value
        .section .rodata
\var:
        .double   \value
        .text
        mov     \var, %rax
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
tmp_string_\@:
        .string "\name"
        .text
        call_fn box_string, $tmp_string_\@
        call_fn string_to_symbol, %rax
        .ifnb \tag
        tag    \tag, \value, target=%rcx
        .endif
        unbox_pointer_internal %rax
        mov     %rcx, symbol_table_values(,%rax,POINTER_SIZE)
        .endm

        .macro macroexpand expander, debug=false
        prologue form
        mov     %rsi, %r12
        mov     %rdx, %rbx

        call_fn cdr, %rdi
        call_fn \expander, %rax
        mov     %rax, form(%rsp)

        .ifc \debug, true
        call_fn display, %rax
        call_fn newline
        .endif

        call_fn jit_datum, form(%rsp), %r12, %rbx

        return
        .endm
