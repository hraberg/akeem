        .include "macros.s"

        .text

        ## R5RS

        ## 6. Standard procedures
        ## 6.1. Equivalence predicates

is_eq:                          # obj1, obj2
is_eqv:                         # obj1, obj2
        eq_internal %rdi, %rsi
        box_boolean_internal
        ret

        ## 6.2. Numbers
        ## 6.2.5. Numerical operations

is_number:                      # obj
is_complex:                     # obj
is_real:                        # obj
is_rational:                    # obj
        is_double_internal %rdi
        jnz     1f
        has_tag TAG_INT, %rdi
1:      box_boolean_internal
        ret

is_integer:                     # obj
        movq    %rdi, %xmm0
        has_tag TAG_INT, %rdi, store=false
        jne     1f
        mov     $TRUE, %rax
        ret
1:      maybe_round_to_int
        has_tag TAG_INT, %rax
        box_boolean_internal
        ret

is_exact:                       # z
        has_tag TAG_INT, %rdi
        box_boolean_internal
        ret

is_inexact:                     # z
        is_double_internal %rdi
        box_boolean_internal
        ret

equal:                          # z1, z2
        binary_comparsion equals, sete, sete

less_than:                      # z1, z2
        binary_comparsion less_than, setb, setl

greater_than:                   # z1, z2
        binary_comparsion greater_than, seta, setg

less_than_or_equal:             # z1, z2
        binary_comparsion less_than_or_equals, setbe, setle

greater_than_or_equal:          # z1, z2
        binary_comparsion greater_than_or_equals, setae, setge

plus:                           # z1, z2
        binary_op plus, addsd, add

minus:                          # z1, z2
        binary_op minus, subsd, sub

multiply:                       # z1, z2
        binary_op multiply, mulsd, imul

divide:                         # z1, z2
        binary_op divide, divsd
divide_int_int:
        cvtsi2sd %edi, %xmm0
        cvtsi2sd %esi, %xmm1
        divsd   %xmm1, %xmm0
        maybe_round_to_int
        ret

quotient:                       # n1, n2
        integer_division
        box_int_internal
        ret

remainder:                      # n1, n2
        prologue
        extract_binary_op
        mov     %rax, %rbx
        integer_division
        cmp     $BINARY_OP_INT_INT, %rbx
        jne     1f
        box_int_internal %edx
        return
1:      cvtsi2sd %edx, %xmm0
        return  %xmm0

modulo:                         # n1, n2
        prologue
        extract_binary_op
        mov     %rax, %rbx
        integer_division
        test    %edx, %edx
        jz      1f
        xor     %esi, %edi
        jns     1f
        add     %esi, %edx
1:      cmp     $BINARY_OP_INT_INT, %rbx
        jne     2f
        box_int_internal %edx
        return
2:      cvtsi2sd %edx, %xmm0
        return  %xmm0

floor_:                         # z
        math_library_unary_call floor, return_int=true

ceiling_:                       # z
        math_library_unary_call ceil, return_int=true

truncate_:                      # z
        math_library_unary_call trunc, return_int=true

round_:                         # z
        math_library_unary_call round, return_int=true

        .irp name, exp, log, sin, cos, tan, asin, acos, atan
\name\()_:                      # z
        math_library_unary_call \name
        .endr

sqrt_:                          # z
        math_library_unary_call sqrt, round=true

expt_:                          # z1, z2
        math_library_binary_call pow, round=true

exact_to_inexact:               # z
        has_tag TAG_INT, %rdi, store=false
        jne     1f
        cvtsi2sd %edi, %xmm0
        movq    %xmm0, %rax
        ret
1:      mov     %rdi, %rax
        ret

inexact_to_exact:               # z
        has_tag TAG_INT, %rdi, store=false
        je      1f
        movq    %rdi, %xmm0
        cvtsd2si %xmm0, %rax
        box_int_internal
        ret
1:      mov     %rdi, %rax
        ret

        ## 6.2.6. Numerical input and output

number_to_string:               # z, radix
        minimal_prologue
        tagged_jump to_string_jump_table
        return

string_to_number:               # string, radix
        prologue tail
        default_arg TAG_INT, $10, %rsi
        mov     %esi, %esi

        unbox_pointer_internal %rdi, %rbx
        add     $header_size, %rbx

        lea     tail(%rsp), %r11
        call_fn strtol, %rbx, %r11, %rsi
        mov     tail(%rsp), %r11
        cmpb    $NULL, (%r11)
        jne     1f
        box_int_internal
        return

1:      lea     tail(%rsp), %r11
        call_fn strtod, %rbx, %r11
        mov     tail(%rsp), %r11
        cmpb    $NULL, (%r11)
        jne     2f
        movq    %xmm0, %rax
        return

2:      return  $FALSE

        ## 6.3. Other data types

        ## 6.3.1. Booleans

not:                            # obj
        mov     $FALSE, %rax
        eq_internal %rax, %rdi
        box_boolean_internal
        ret

        ## 6.3.2. Pairs and lists

is_pair:                        # obj
        is_nil_internal %rdi
        jne     1f
        mov     $FALSE, %rax
        ret
1:      has_tag TAG_PAIR, %rdi
        box_boolean_internal
        ret

cons:                           # obj1, obj2
        prologue
        mov     %rdi, %rbx
        mov     %rsi, %r12
        call_fn gc_allocate_memory, $pair_size
        movw    $TAG_PAIR, header_object_type(%rax)
        movl    $pair_size, header_object_size(%rax)
        mov     %rbx, pair_car(%rax)
        mov     %r12, pair_cdr(%rax)
        tag     TAG_PAIR, %rax
        register_for_gc
        return

car:                            # pair
        unbox_pointer_internal %rdi
        mov     pair_car(%rax), %rax
        ret

cdr:                            # pair
        unbox_pointer_internal %rdi
        mov     pair_cdr(%rax), %rax
        ret

set_car:                        # pair, obj
        unbox_pointer_internal %rdi
        mov     %rsi, pair_car(%rax)
        mov     $VOID, %rax
        ret

set_cdr:                        # pair, obj
        unbox_pointer_internal %rdi
        mov     %rsi, pair_cdr(%rax)
        mov     $VOID, %rax
        ret

is_null:                        # obj
        is_nil_internal %rdi, store=true
        box_boolean_internal
        ret

length:                         # list
        prologue
        mov     %rdi, %rax
        xor     %ebx, %ebx

1:      is_nil_internal %rax
        je      2f

        call_fn cdr, %rax
        inc     %rbx
        jmp     1b

2:      box_int_internal %ebx
        return

append:                        # list1, list2
        prologue
        mov     %rsi, %r12
        call_fn reverse, %rdi
        mov     %rax, %rbx
1:      is_nil_internal %rbx
        je      2f

        calL_fn car, %rbx
        call_fn cons, %rax, %r12
        mov     %rax, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return  %r12

reverse:                        # list
        prologue
        mov     %rdi, %rbx
        mov     $NIL, %r12
1:      is_nil_internal %rbx
        je      2f

        calL_fn car, %rbx
        call_fn cons, %rax, %r12
        mov     %rax, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return  %r12

        ## 6.3.3. Symbols

is_symbol:                      # obj
        has_tag TAG_SYMBOL, %rdi
        box_boolean_internal
        ret

symbol_to_string:               # symbol
        mov     symbol_table_names(,%edi,POINTER_SIZE), %rax
        ret

string_to_symbol:               # string
        prologue
        unbox_pointer_internal %rdi
        mov     %rax, %r12
        mov     symbol_next_id, %rbx

1:      test    %rbx, %rbx
        jz      2f

        dec     %rbx
        mov     symbol_table_names(,%rbx,POINTER_SIZE), %rax
        unbox_pointer_internal %rax

        test    %eax, %eax
        jz      1b
        add     $header_size, %rax
        lea     header_size(%r12), %r11
        call_fn strcmp, %r11, %rax
        jnz     1b
        jmp     3f

2:      mov     symbol_next_id, %rbx
        incq    symbol_next_id

        lea     header_size(%r12), %r11
        call_fn box_string, %r11
        register_for_gc
        mov     %rax, symbol_table_names(,%rbx,POINTER_SIZE)
        call_fn jit_maybe_add_to_constant_pool, %rax

3:      tag     TAG_SYMBOL, %rbx
        return

        ## 6.3.4. Characters

is_char:                        # obj
        has_tag TAG_CHAR, %rdi
        box_boolean_internal
        ret

char_to_integer:                # char
        mov    %edi, %eax
        box_int_internal
        ret

integer_to_char:                # n
        xor     %eax, %eax
        mov     %di, %ax
        tag     TAG_CHAR, %rax
        ret

is_char_alphabetic:             # char
        minimal_prologue
        mov    %edi, %edi
        call_fn isalpha, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_numeric:                # char
        minimal_prologue
        mov    %edi, %edi
        call_fn isdigit, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_whitespace:             # char
        minimal_prologue
        mov    %edi, %edi
        call_fn isspace, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_upper_case:             # char
        minimal_prologue
        mov    %edi, %edi
        call_fn isupper, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_lower_case:             # char
        minimal_prologue
        mov    %edi, %edi
        call_fn islower, %rdi
        setnz   %al
        box_boolean_internal
        return

char_upcase:                    # char
        minimal_prologue
        mov    %edi, %edi
        call_fn toupper, %rdi
        tag     TAG_CHAR, %rax
        return

char_downcase:                  # char
        minimal_prologue
        mov    %edi, %edi
        call_fn tolower, %rdi
        tag     TAG_CHAR, %rax
        return

        ## 6.3.5. Strings

is_string:                      # obj
        has_tag TAG_STRING, %rdi
        box_boolean_internal
        ret

make_string:                    # k, fill
        prologue
        mov     %edi, %edi
        mov     %edi, %ebx
        add     $header_size, %edi
        inc     %edi
        mov     %rsi, %r12
        call_fn gc_allocate_memory, %rdi
        movw    $TAG_STRING, header_object_type(%rax)
        mov     %ebx, header_object_size(%rax)
        incl    header_object_size(%rax)

        movb    $NULL, header_size(%rax,%rbx)

1:      test    %ebx, %ebx
        jz      2f
        dec     %ebx
        mov     %r12b, header_size(%rax,%rbx,1)
        jmp     1b

2:      tag     TAG_STRING, %rax
        register_for_gc
        return

string_length:                  # string
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        dec     %eax
        box_int_internal %eax
        ret

string_ref:                     # string, k
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        movzxb  header_size(%rax,%rsi), %eax
        tag     TAG_CHAR, %rax
        ret

string_set:                     # string, k, char
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        mov     %dl, header_size(%rax,%rsi)
        mov     $VOID, %rax
        ret

is_string_equal:                # string1, string2
        string_comparator strcmp, setz

is_string_ci_equal:             # string1, string2
        string_comparator strcasecmp, setz

is_string_less_than:            # string1, string2
        string_comparator strcmp, setl

is_string_greater_than:         # string1, string2
        string_comparator strcmp, setg

is_string_less_than_or_equal:  # string1, string2
        string_comparator strcmp, setle

is_string_greater_than_or_equal: # string1, string2
        string_comparator strcmp, setge

is_string_ci_less_than:         # string1, string2
        string_comparator strcasecmp, setl

is_string_ci_greater_than:      # string1, string2
        string_comparator strcasecmp, setg

is_string_ci_less_than_or_equal: # string1, string2
        string_comparator strcasecmp, setle

is_string_ci_greater_than_or_equal: # string1, string2
        string_comparator strcasecmp, setge

        ## 6.3.6. Vectors

is_vector:                      # obj
        has_tag TAG_VECTOR, %rdi
        box_boolean_internal %rax
        ret

make_vector:                    # k, fill
        prologue
        mov     %rsi, %r12
        shl     $POINTER_SIZE_SHIFT, %edi
        mov     %edi, %ebx
        add     $header_size, %edi
        call_fn gc_allocate_memory, %rdi
        movw    $TAG_VECTOR, header_object_type(%rax)
        mov     %ebx, header_object_size(%rax)

1:      test    %ebx, %ebx
        jz      2f
        sub     $POINTER_SIZE, %ebx
        mov     %r12, header_size(%rax,%rbx)
        jmp     1b

2:      tag     TAG_VECTOR, %rax
        register_for_gc
        return

vector_length:                  # vector
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        shr     $POINTER_SIZE_SHIFT, %eax
        box_int_internal
        ret

vector_ref:                     # vector, k
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     header_size(%rax,%rsi,POINTER_SIZE), %rax
        ret

vector_set:                     # vector, k, obj
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     %rdx, header_size(%rax,%rsi,POINTER_SIZE)
        mov     $VOID, %rax
        ret

list_to_vector:                 # list
        prologue vec
        mov     %rdi, %r12
        call_fn length, %r12
        call_fn make_vector, %rax
        mov     %rax, vec(%rsp)

        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      2f

        call_fn car, %r12
        call_fn vector_set, vec(%rsp), %rbx, %rax
        call_fn cdr, %r12
        mov     %rax, %r12

        inc     %rbx
        jmp     1b

2:      return  vec(%rsp)

        ## 6.4. Control features

is_procedure:                   # obj
        has_tag TAG_PROCEDURE, %rdi
        box_boolean_internal
        ret

apply:                          # proc, args
        prologue
        unbox_pointer_internal %rdi
        push    %rax

        mov     %rsi, %r12
        call_fn length, %r12
        mov     %eax, %ebx

1:      is_nil_internal %r12
        je      2f
        call_fn car, %r12
        push %rax
        call_fn cdr, %r12
        mov     %rax, %r12
        jmp     1b

2:      mov     $MAX_REGISTER_ARGS, %eax
        sub     %ebx, %eax
        js      apply_6
        lea     apply_6(,%eax,APPLY_JUMP_ALIGNMENT), %rax
        jmp     *%rax

        .align  16
apply_6:
        pop      %r9
        .align  APPLY_JUMP_ALIGNMENT
apply_5:
        pop      %r8
        .align  APPLY_JUMP_ALIGNMENT
apply_4:
        pop      %rcx
        .align  APPLY_JUMP_ALIGNMENT
apply_3:
        pop      %rdx
        .align  APPLY_JUMP_ALIGNMENT
apply_2:
        pop      %rsi
        .align  APPLY_JUMP_ALIGNMENT
apply_1:
        pop      %rdi
        .align  APPLY_JUMP_ALIGNMENT
apply_0:
        pop     %rax
        call    *%rax
        return

call_with_current_continuation: # proc
        prologue jmp_buffer
        unbox_pointer_internal %rdi, %rbx
        call_fn malloc, $JMP_BUF_SIZE
        perror
        mov     %rax, jmp_buffer(%rsp)

        call_fn setjmp, jmp_buffer(%rsp) # https://www.gnu.org/software/libc/manual/html_mono/libc.html#System-V-contexts
        jnz 1f
        call_fn jit_call_with_current_continuation_escape_factory, jmp_buffer(%rsp)
        tag     TAG_PROCEDURE, %rax
        call_fn *%rbx, %rax
        return
1:      call_fn free, jmp_buffer(%rsp)
        return  %xmm0

        ## 6.5. Eval

eval:                           # expression, environment-specifier
        prologue max_global_symbol
        default_arg TAG_INT, $-1, %rsi

        mov     %esi, max_global_symbol(%rsp)

        call_fn jit_code, %rdi, $NIL, $NIL
        call    *%rax
        return

scheme_report_environment:      # version
        box_int_internal max_scheme_report_environment_symbol
        ret

null_environment:               # version
        box_int_internal max_null_environment_symbol
        ret

interaction_environment:
        box_int_internal $-1
        ret

        ## 6.6. Input and output
        ## 6.6.1. Ports

open_input_file:                # filename
        minimal_prologue
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $read_mode
        perror
        tag     TAG_PORT, %rax
        return

open_output_file:               # filename
        minimal_prologue
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $write_mode
        perror
        tag     TAG_PORT, %rax
        return

close_input_port:               # port
close_output_port:              # port
        minimal_prologue
        call_fn close_port, %rdi
        return

with_input_from_file:           # filename, thunk
        with_file_io_template input, stdin

with_output_to_file:            # filename, thunk
        with_file_io_template output, stdout

current_output_port:
        tag     TAG_PORT, stdout
        ret

current_input_port:
        tag     TAG_PORT, stdin
        ret

is_input_port:                  # obj
        minimal_prologue
        has_tag TAG_PORT, %rdi, store=false
        jne     1f
        unbox_pointer_internal %rdi
        call_fn __freadable, %rax
        cmp     $NULL, %rax
        setg    %al
        box_boolean_internal
        return
1:      return $FALSE

is_output_port:                 # obj
        minimal_prologue
        has_tag TAG_PORT, %rdi, store=false
        jne     1f
        unbox_pointer_internal %rdi
        call_fn __fwritable, %rax
        cmp     $NULL, %rax
        setg    %al
        box_boolean_internal
        return
1:      return $FALSE

        ## 6.6.2. Input

read:                           # port
        minimal_prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi
        call_fn read_datum, %rax
        return

read_char:                      # port
        minimal_prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi
        call_fn fgetc, %rax
        cmp     $EOF, %al
        je      1f
        tag     TAG_CHAR, %rax
        return
1:      return  $EOF_OBJECT

peek_char:                      # port
        prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi, %rbx
        call_fn fgetc, %rbx
        call_fn ungetc, %rax, %rbx
        cmp     $EOF, %al
        je      1f
        tag     TAG_CHAR, %rax
        return
1:      return  $EOF_OBJECT

is_eof_object:                  # obj
        is_eof_object_internal %rdi, store=true
        box_boolean_internal
        ret

        ## 6.6.3. Output

write:                          # obj, port
        prologue

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_CHAR, $char_to_machine_readable_string
        store_pointer $TAG_STRING, $string_to_machine_readable_string

        call_fn display, %rdi, %rsi

        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_STRING, $string_to_string

        return  $VOID

display:                        # obj, port
        prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi, %rbx
        call_fn to_string, %rdi
        unbox_pointer_internal %rax, %rdi
        xor     %al, %al
        add     $header_size, %rdi
        call_fn fprintf, %rbx, %rdi
        call_fn fflush, %rbx
        return  $VOID

write_char:                     # char, port
        minimal_prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi, %rax
        mov     %edi, %edi
        call_fn fputc, %rdi, %rax
        return  $VOID

        ## 6.6.4. System interface

load:                           # filename
        prologue
        call_fn open_input_file, %rdi
        mov     %rax, %rbx
        call_fn read_all, %rax
        call_fn close_input_port, %rbx
        return  $VOID

        ## R7RS

        ## 6. Standard procedures
        ## 6.2. Numbers
        ## 6.2.6. Numerical operations

is_infinite:                    # z
        minimal_prologue
        is_double_internal %rdi
        jz      1f
        movq    %rdi, %xmm0
        call    isinf
        setnz   %al
        box_boolean_internal
        return
1:      return  $FALSE

is_nan:                         # z
        minimal_prologue
        is_double_internal %rdi
        jz      1f
        movq    %rdi, %xmm0
        call    isnan
        setnz   %al
        box_boolean_internal
        return
1:      return  $FALSE

        ## 6.9. Bytevectors

is_bytevector:                  # obj
        minimal_prologue
        call_fn class_of, %rdi
        eq_internal $TAG_BYTEVECTOR, %eax
        box_boolean_internal %rax
        return

make_bytevector:                # k, byte
        prologue
        mov     %rsi, %r12
        mov     %edi, %ebx
        add     $header_size, %edi
        call_fn gc_allocate_memory, %rdi
        movw    $TAG_BYTEVECTOR, header_object_type(%rax)
        mov     %ebx, header_object_size(%rax)

1:      test    %ebx, %ebx
        jz      2f
        dec     %ebx
        mov     %r12b, header_size(%rax,%rbx)
        jmp     1b

2:      tag     TAG_OBJECT, %rax
        register_for_gc
        return

bytevector_length:              # bytevector
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        box_int_internal
        ret

bytevector_u8_ref:              # bytevector, k
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        xor     %r11d, %r11d
        mov     header_size(%rax,%rsi), %r11b
        tag     TAG_INT, %r11
        ret

bytevector_u8_set:              # bytevector, k, byte
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     %dl, header_size(%rax,%rsi)
        mov     $VOID, %rax
        ret

list_to_bytevector:             # list
        prologue vec
        mov     %rdi, %r12
        call_fn length, %r12
        call_fn make_bytevector, %rax
        mov     %rax, vec(%rsp)

        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      2f

        call_fn car, %r12
        call_fn bytevector_u8_set, vec(%rsp), %rbx, %rax
        call_fn cdr, %r12
        mov     %rax, %r12

        inc     %rbx
        jmp     1b

2:      return  vec(%rsp)

        ## 6.11. Exceptions

error:                          # message
        prologue
        mov     %rdi, %rbx
        call_fn current_error_port
        mov     %rax, %r12
        call_fn display, %rbx, %r12
        call_fn display, $NEWLINE_CHAR, %r12
        call_fn exit, $1
        return

        ## 6.13. Input and output
        ## 6.13.1. Ports

call_with_port:                 # port, proc
        prologue
        unbox_pointer_internal %rsi, %rbx
        mov     %rdi, %r12
        call_fn *%rbx, %rax
        mov     %rax, %rbx
        call_fn close_port, %r12
        return  %rbx

current_error_port:
        tag     TAG_PORT, stderr
        ret

close_port:                     # port
        minimal_prologue
        unbox_pointer_internal %rdi
        call_fn fclose, %rax
        test    %eax, %eax
        setz    %al
        box_boolean_internal
        return

open_input_string:              # string
        open_input_buffer_template $-1

open_input_bytevector:          # bytevector
        open_input_buffer_template $0

        ## 6.13.2. Input

eof_object:
        mov     $EOF_OBJECT, %rax
        ret

read_u8:                        # port
        minimal_prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi
        call_fn fgetc, %rax
        cmp     $EOF, %al
        je      1f
        tag     TAG_INT, %rax
        return
1:      return  $EOF_OBJECT

peek_u8:                        # port
        prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi, %rbx
        call_fn fgetc, %rbx
        call_fn ungetc, %rax, %rbx
        cmp     $EOF, %al
        je      1f
        tag     TAG_INT, %rax
        return
1:      return  $EOF_OBJECT

        ## 6.13.3. Output

write_u8:                       # byte, port
        minimal_prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi, %rax
        mov     %edi, %edi
        call_fn fputc, %rdi, %rax
        return  $VOID

flush_output_port:              # port
        minimal_prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi
        call_fn fflush, %rax
        return  $VOID

        ## 6.14. System interface

delete_file:                    # filename
        minimal_prologue
        call_fn unbox_string, %rdi
        call_fn unlink, %rax
        perror  jge
        return  $VOID

is_file_exists:                 # filename
        minimal_prologue
        call_fn unbox_string, %rdi
        call_fn access, %rax, $F_OK
        cmp     $-1, %rax
        setne   %al
        and     $C_TRUE, %rax
        box_boolean_internal
        return

command_line:
        mov     command_line_arguments, %rax
        ret

exit_:                          # obj
        minimal_prologue

        mov     $TRUE, %r11
        cmp     %r11, %rdi
        jne     1f
        mov     $0, %rdi
        jmp     3f

1:      mov     $FALSE, %r11
        cmp     %r11, %rdi
        jne     2f
        mov     $1, %rdi
        jmp     3f

2:      default_arg TAG_INT, $0, %rdi
        mov     %edi, %edi

3:      call_fn exit, %rdi
        return

emergency_exit:                 # obj
        minimal_prologue
        default_arg TAG_INT, $1, %rdi
        mov     %edi, %edi
        call_fn exit, %rdi
        return

get_environment_variables:
        mov     environment_alist, %rax
        ret

current_second:
        prologue tv_sec, tv_usec
        lea     tv_sec(%rsp), %rax
        call_fn gettimeofday, %rax, $NULL
        cvtsi2sd tv_sec(%rsp), %xmm0
        cvtsi2sd tv_usec(%rsp), %xmm1
        mov     $MICROSECONDS_PER_SEC, %rax
        cvtsi2sd %rax, %xmm2
        divsd   %xmm2, %xmm1
        addsd   %xmm1, %xmm0
        return  %xmm0

current_jiffy:
        minimal_prologue
        call_fn clock
        box_int_internal
        return

jiffies_per_second:
        mov     $CLOCKS_PER_SEC, %rax
        box_int_internal
        ret

        ## Runtime

        .globl main

main:                # argc, argv
        prologue argc, argv
        mov     %rdi, argc(%rsp)
        mov     %rsi, argv(%rsp)
        mov     %rsp, execution_stack_top
        movq    $LOG_JIT, jit_code_debug

        call_fn init_pointer_stack, $object_space, $POINTER_STACK_INITIAL_SIZE
        call_fn init_pointer_stack, $gc_mark_stack, $POINTER_STACK_INITIAL_SIZE
        call_fn init_pointer_stack, $constant_pool, $POINTER_STACK_INITIAL_SIZE

        movq    $CODE_SPACE_SIZE, jit_code_space_size
        call_fn mmap, $NULL, jit_code_space_size, $(PROT_READ | PROT_WRITE | PROT_EXEC), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
        perror
        mov     %rax, jit_code_space
        mov     %rax, jit_code_space_next_address

        lea     jit_constant_pool_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $jit_add_to_constant_pool_nop
        store_pointer $TAG_BOOLEAN, $jit_add_to_constant_pool_nop
        store_pointer $TAG_CHAR, $jit_add_to_constant_pool_nop
        store_pointer $TAG_INT, $jit_add_to_constant_pool_nop
        store_pointer $TAG_SYMBOL, $jit_add_to_constant_pool_nop
        store_pointer $TAG_PROCEDURE, $jit_add_to_constant_pool_nop
        store_pointer $TAG_PORT, $jit_add_to_constant_pool_nop
        store_pointer $TAG_STRING, $jit_add_to_constant_pool
        store_pointer $TAG_PAIR, $jit_add_to_constant_pool
        store_pointer $TAG_VECTOR, $jit_add_to_constant_pool
        store_pointer $TAG_OBJECT, $jit_add_to_constant_pool

        lea     gc_mark_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $gc_mark_nop
        store_pointer $TAG_BOOLEAN, $gc_mark_nop
        store_pointer $TAG_CHAR, $gc_mark_nop
        store_pointer $TAG_INT, $gc_mark_nop
        store_pointer $TAG_SYMBOL, $gc_mark_nop
        store_pointer $TAG_PROCEDURE, $gc_mark_nop
        store_pointer $TAG_PORT, $gc_mark_nop
        store_pointer $TAG_STRING, $gc_mark_string
        store_pointer $TAG_PAIR, $gc_mark_object
        store_pointer $TAG_VECTOR, $gc_mark_object
        store_pointer $TAG_OBJECT, $gc_mark_object

        lea     gc_mark_queue_jump_table, %rbx
        store_pointer $TAG_PAIR, $gc_mark_queue_pair
        store_pointer $TAG_VECTOR, $gc_mark_queue_vector

        intern_symbol double_symbol, "double", id=TAG_DOUBLE
        intern_symbol boolean_symbol, "boolean", id=TAG_BOOLEAN
        intern_symbol byte_symbol, "byte", id=TAG_BYTE
        intern_symbol char_symbol, "char", id=TAG_CHAR
        intern_symbol int_symbol, "int", id=TAG_INT
        intern_symbol symbol_symbol, "symbol", id=TAG_SYMBOL
        intern_symbol procedure_symbol, "procedure", id=TAG_PROCEDURE
        intern_symbol port_symbol, "port", id=TAG_PORT
        intern_symbol string_symbol, "string", id=TAG_STRING
        intern_symbol pair_symbol, "pair", id=TAG_PAIR
        intern_symbol vector_symbol, "vector", id=TAG_VECTOR
        intern_symbol object_symbol, "object", id=TAG_OBJECT
        intern_symbol bytevector_symbol, "bytevector", id=TAG_BYTEVECTOR

        intern_symbol quote_symbol, "quote"
        intern_symbol lambda_symbol, "lambda"
        intern_symbol if_symbol, "if"
        intern_symbol set_symbol, "set!"
        intern_symbol let_symbol, "let"
        intern_symbol letrec_symbol, "letrec"
        intern_symbol begin_symbol, "begin"
        intern_symbol quasiquote_symbol, "quasiquote"
        intern_symbol unquote_symbol, "unquote"
        intern_symbol unquote_splicing_symbol, "unquote-splicing"
        intern_symbol define_syntax_symbol, "define-syntax"

        intern_symbol r5rs_let_symbol, "r5rs-let"

        intern_symbol dot_symbol, "."
        intern_symbol void_symbol, "void"
        intern_symbol eof_symbol, "eof"

        mov     symbol_next_id, %rax
        mov     %rax, max_null_environment_symbol

        intern_string empty_string, ""

        intern_string read_error_string, "Unexpected input"
        intern_string code_space_error_string, "Code space exceeded"
        intern_string false_string, "#f"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_FALSE
        intern_string true_string, "#t"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_TRUE

        intern_string alarm_char, "#\\alarm"
        intern_string backspace_char, "#\\backspace"
        intern_string delete_char, "#\\delete"
        intern_string escape_char, "#\\escape"
        intern_string newline_char, "#\\newline"
        intern_string null_char, "#\\null"
        intern_string return_char, "#\\return"
        intern_string space_char, "#\\space"
        intern_string tab_char, "#\\tab"

        lea     char_to_string_table, %rbx
        store_pointer $7, alarm_char
        store_pointer $'\b, backspace_char
        store_pointer $127, delete_char
        store_pointer $27, escape_char
        store_pointer $'\n, newline_char
        store_pointer $0, null_char
        store_pointer $'\r, return_char
        store_pointer $'\ , space_char
        store_pointer $'\t, tab_char

        lea     escape_char_table, %rbx
        movb    $'a, 7(%rbx)
        movb    $'b, 8(%rbx)
        movb    $'t, 9(%rbx)
        movb    $'n, 10(%rbx)
        movb    $'r, 13(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'\\, 92(%rbx)

        lea     unescape_char_table, %rbx
        movb    $'\a, 97(%rbx)
        movb    $'\b, 98(%rbx)
        movb    $'\t, 116(%rbx)
        movb    $'\n, 110(%rbx)
        movb    $'\r, 114(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'\\, 92(%rbx)

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $double_to_string
        store_pointer $TAG_BOOLEAN, $boolean_to_string
        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_INT, $integer_to_string
        store_pointer $TAG_SYMBOL, $symbol_to_string
        store_pointer $TAG_PROCEDURE, $object_to_string
        store_pointer $TAG_PORT, $object_to_string
        store_pointer $TAG_STRING, $string_to_string
        store_pointer $TAG_PAIR, $pair_to_string
        store_pointer $TAG_VECTOR, $vector_to_string
        store_pointer $TAG_OBJECT, $object_to_string

        store_pointer bytevector_symbol, $bytevector_to_string

        lea     unbox_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $unbox_double
        store_pointer $TAG_BOOLEAN, $unbox_boolean
        store_pointer $TAG_CHAR, $unbox_char
        store_pointer $TAG_INT, $unbox_integer
        store_pointer $TAG_SYMBOL, $unbox_symbol
        store_pointer $TAG_PROCEDURE, $unbox_port
        store_pointer $TAG_PORT, $unbox_port
        store_pointer $TAG_STRING, $unbox_string
        store_pointer $TAG_PAIR, $unbox_pair
        store_pointer $TAG_VECTOR, $unbox_vector
        store_pointer $TAG_OBJECT, $unbox_object

        lea     integer_to_string_format_table, %rbx
        store_pointer $8, $oct_format
        store_pointer $10, $int_format
        store_pointer $16, $hex_format

        lea     read_datum_jump_table, %rbx
        store_pointer $'\#, $read_hash
        store_pointer $'(, $read_list
        store_pointer $'\", $read_string
        .irp digit, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
        store_pointer $(\digit + '0), $read_number
        .endr
        store_pointer $'+, $read_number_or_symbol
        store_pointer $'-, $read_number_or_symbol
        store_pointer $'', $read_quote
        store_pointer $'`, $read_quasiquote
        store_pointer $',, $read_unquote

        lea     read_hash_jump_table, %rbx
        store_pointer $'t, $read_true
        store_pointer $'f, $read_false
        store_pointer $'\\, $read_character
        store_pointer $'(, $read_vector
        store_pointer $'b, $read_binary_number
        store_pointer $'o, $read_octal_number
        store_pointer $'d, $read_decimal_number
        store_pointer $'x, $read_hexadecimal_number
        store_pointer $'u, $read_bytevector
        store_pointer $'!, $read_hashbang

        lea     jit_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $jit_literal
        store_pointer $TAG_BOOLEAN, $jit_literal
        store_pointer $TAG_CHAR, $jit_literal
        store_pointer $TAG_INT, $jit_literal
        store_pointer $TAG_SYMBOL, $jit_symbol
        store_pointer $TAG_PROCEDURE, $jit_literal
        store_pointer $TAG_PORT, $jit_literal
        store_pointer $TAG_STRING, $jit_literal
        store_pointer $TAG_PAIR, $jit_pair
        store_pointer $TAG_VECTOR, $jit_literal
        store_pointer $TAG_OBJECT, $jit_literal

        lea     jit_argument_to_register_id_table, %rbx
        movb    $RDI, 0(%rbx)
        movb    $RSI, 1(%rbx)
        movb    $RDX, 2(%rbx)
        movb    $RCX, 3(%rbx)
        movb    $R8, 4(%rbx)
        movb    $R9, 5(%rbx)

        lea     jit_pop_register_table, %rbx
        store_pointer $RAX, $jit_pop_rax
        store_pointer $RCX, $jit_pop_rcx
        store_pointer $RDX, $jit_pop_rdx
        store_pointer $RSI, $jit_pop_rsi
        store_pointer $RDI, $jit_pop_rdi
        store_pointer $R8, $jit_pop_r8
        store_pointer $R9, $jit_pop_r9

        lea     jit_pop_register_size_table, %rbx
        store_pointer $RAX, $jit_pop_rax_size
        store_pointer $RCX, jit_pop_rcx_size
        store_pointer $RDX, jit_pop_rdx_size
        store_pointer $RSI, jit_pop_rsi_size
        store_pointer $RDI, jit_pop_rdi_size
        store_pointer $R8, jit_pop_r8_size
        store_pointer $R9, jit_pop_r9_size

        lea     jit_literal_to_register_table, %rbx
        store_pointer $RAX, $jit_literal_to_rax
        store_pointer $RCX, $jit_literal_to_rcx
        store_pointer $RDX, $jit_literal_to_rdx
        store_pointer $RSI, $jit_literal_to_rsi
        store_pointer $RDI, $jit_literal_to_rdi
        store_pointer $R8, $jit_literal_to_r8
        store_pointer $R9, $jit_literal_to_r9

        lea     jit_literal_to_register_size_table, %rbx
        store_pointer $RAX, jit_literal_to_rax_size
        store_pointer $RCX, jit_literal_to_rcx_size
        store_pointer $RDX, jit_literal_to_rdx_size
        store_pointer $RSI, jit_literal_to_rsi_size
        store_pointer $RDI, jit_literal_to_rdi_size
        store_pointer $R8, jit_literal_to_r8_size
        store_pointer $R9, jit_literal_to_r9_size

        lea     jit_rax_to_register_table, %rbx
        store_pointer $RAX, $jit_rax_to_rax
        store_pointer $RCX, $jit_rax_to_rcx
        store_pointer $RDX, $jit_rax_to_rdx
        store_pointer $RSI, $jit_rax_to_rsi
        store_pointer $RDI, $jit_rax_to_rdi
        store_pointer $R8, $jit_rax_to_r8
        store_pointer $R9, $jit_rax_to_r9

        lea     jit_rax_to_register_size_table, %rbx
        store_pointer $RAX, jit_rax_to_rax_size
        store_pointer $RCX, jit_rax_to_rcx_size
        store_pointer $RDX, jit_rax_to_rdx_size
        store_pointer $RSI, jit_rax_to_rsi_size
        store_pointer $RDI, jit_rax_to_rdi_size
        store_pointer $R8, jit_rax_to_r8_size
        store_pointer $R9, jit_rax_to_r9_size

        lea     jit_local_to_register_table, %rbx
        store_pointer $RAX, $jit_local_to_rax
        store_pointer $RCX, $jit_local_to_rcx
        store_pointer $RDX, $jit_local_to_rdx
        store_pointer $RSI, $jit_local_to_rsi
        store_pointer $RDI, $jit_local_to_rdi
        store_pointer $R8, $jit_local_to_r8
        store_pointer $R9, $jit_local_to_r9

        lea     jit_local_to_register_size_table, %rbx
        store_pointer $RAX, jit_local_to_rax_size
        store_pointer $RCX, jit_local_to_rcx_size
        store_pointer $RDX, jit_local_to_rdx_size
        store_pointer $RSI, jit_local_to_rsi_size
        store_pointer $RDI, jit_local_to_rdi_size
        store_pointer $R8, jit_local_to_r8_size
        store_pointer $R9, jit_local_to_r9_size

        lea     jit_register_to_local_table, %rbx
        store_pointer $RAX, $jit_rax_to_local
        store_pointer $RCX, $jit_rcx_to_local
        store_pointer $RDX, $jit_rdx_to_local
        store_pointer $RSI, $jit_rsi_to_local
        store_pointer $RDI, $jit_rdi_to_local
        store_pointer $R8, $jit_r8_to_local
        store_pointer $R9, $jit_r9_to_local

        lea     jit_register_to_local_size_table, %rbx
        store_pointer $RAX, jit_rax_to_local_size
        store_pointer $RCX, jit_rcx_to_local_size
        store_pointer $RDX, jit_rdx_to_local_size
        store_pointer $RSI, jit_rsi_to_local_size
        store_pointer $RDI, jit_rdi_to_local_size
        store_pointer $R8, jit_r8_to_local_size
        store_pointer $R9, jit_r9_to_local_size

        lea     jit_syntax_jump_table, %rbx
        .irp symbol, quote, if, set, lambda, begin, let, letrec, define_syntax
        unbox_pointer_internal \symbol\()_symbol
        store_pointer %eax, $jit_\symbol
        .endr

        .irp name, eq, eqv, number, complex, real, rational, integer, exact, inexact
        define "\name?", $is_\name
        .endr

        define "=", $equal
        define "<", $less_than
        define "<=", $less_than_or_equal
        define ">", $greater_than
        define ">=", $greater_than_or_equal

        define "+", $plus
        define "-", $minus
        define "*", $multiply
        define "/", $divide

        define "quotient", $quotient
        define "remainder", $remainder
        define "modulo", $modulo

        .irp name, ceiling, truncate, round, floor, exp, log, sin, cos, tan, asin, acos, atan, sqrt, expt
        define "\name", $\name\()_
        .endr

        define "exact->inexact", $exact_to_inexact
        define "inexact->exact", $inexact_to_exact

        define "number->string", $number_to_string
        define "string->number", $string_to_number

        define "not", $not

        define "pair?", $is_pair
        define "cons", $cons
        define "car", $car
        define "cdr", $cdr
        define "set-car!", $set_car
        define "set-cdr!", $set_cdr

        define "null?", $is_null
        define "length", $length
        define "append", $append
        define "reverse", $reverse

        define "symbol?", $is_symbol
        define "symbol->string", $symbol_to_string
        define "string->symbol", $string_to_symbol

        define "char?", $is_char
        define "char-alphabetic?", $is_char_alphabetic
        define "char-numeric?", $is_char_numeric
        define "char-whitespace?", $is_char_whitespace
        define "char-upper-case?", $is_char_upper_case
        define "char-lower-case?", $is_char_lower_case
        define "char->integer", $char_to_integer
        define "integer->char", $integer_to_char
        define "char-upcase", $char_upcase
        define "char-downcase", $char_downcase

        define "string?", $is_string
        define "make-string", $make_string
        define "string-length", $string_length
        define "string-ref", $string_ref
        define "string-set!", $string_set

        define "string=?", $is_string_equal
        define "string-ci=?", $is_string_ci_equal

        define "string<?", $is_string_less_than
        define "string>?", $is_string_greater_than
        define "string<=?", $is_string_less_than_or_equal
        define "string>=?", $is_string_greater_than_or_equal

        define "string-ci<?", $is_string_ci_less_than
        define "string-ci>?", $is_string_ci_greater_than
        define "string-ci<=?", $is_string_ci_less_than_or_equal
        define "string-ci>=?", $is_string_ci_greater_than_or_equal

        define "vector?", $is_vector
        define "make-vector", $make_vector
        define "vector-length", $vector_length
        define "vector-ref", $vector_ref
        define "vector-set!", $vector_set
        define "list->vector", $list_to_vector

        define "procedure?", $is_procedure
        define "apply", $apply
        define "call-with-current-continuation", $call_with_current_continuation

        define "eval", $eval
        define "scheme-report-environment", $scheme_report_environment
        define "null-environment", $null_environment
        define "interaction-environment", $interaction_environment

        define "input-port?", $is_input_port
        define "output-port?", $is_output_port

        define "current-input-port", $current_input_port
        define "current-output-port", $current_output_port

        define "with-input-from-file", $with_input_from_file
        define "with-output-to-file", $with_output_to_file

        define "open-input-file", $open_input_file
        define "open-output-file", $open_output_file

        define "close-input-port", $close_input_port
        define "close-output-port", $close_output_port

        define "read", $read
        define "read-char", $read_char
        define "peek-char", $peek_char
        define "eof-object?", $is_eof_object

        define "write", $write
        define "display", $display
        define "write-char", $write_char

        define "load", $load

        call_fn box_string, $boot_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax

        call_fn box_string, $r5rs_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax

        mov     symbol_next_id, %rax
        mov     %rax, max_scheme_report_environment_symbol

        define "infinite?", $is_infinite
        define "nan?", $is_nan

        define "bytevector?", $is_bytevector
        define "make-bytevector", $make_bytevector
        define "bytevector-length", $bytevector_length
        define "bytevector-u8-ref", $bytevector_u8_ref
        define "bytevector-u8-set!", $bytevector_u8_set
        define "list->bytevector", $list_to_bytevector

        define "error", $error

        define "call-with-port", $call_with_port
        define "current-error-port", $current_error_port
        define "close-port", $close_port
        define "open-input-string", $open_input_string
        define "open-input-bytevector", $open_input_bytevector

        define "eof-object", $eof_object
        define "read-u8", $read_u8
        define "peek-u8", $peek_u8

        define "write-u8", $write_u8
        define "flush-output-port", $flush_output_port

        define "delete-file", $delete_file
        define "file-exists?", $is_file_exists
        define "command-line", $command_line
        define "exit", $exit_
        define "emergency-exit", $emergency_exit
        define "get-environment-variables", $get_environment_variables
        define "current-second", $current_second
        define "current-jiffy", $current_jiffy
        define "jiffies-per-second", $jiffies_per_second

        call_fn box_string, $r7rs_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax

        define "read-all", $read_all
        define "gc", $gc
        define "object-space-size", $object_space_size
        define "class-of", $class_of
        define "void", $void

        call_fn box_string_array_as_list, argv(%rsp)
        mov     %rax, command_line_arguments

        call_fn box_string_array_as_list, environ
        call_fn build_environment_alist, %rax
        mov     %rax, environment_alist

        call_fn gc

        call_fn box_string, $init_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax

        return  $0

box_string_array_as_list:       # c-string-array
        prologue strings
        mov     %rdi, %r12
        mov     $NIL, %rax
        mov     %rax, strings(%rsp)

        xor     %ebx, %ebx

1:      mov     (%r12,%rbx,POINTER_SIZE), %rax
        test    %eax, %eax
        jz      2f

        call_fn box_string, %rax
        call_fn cons, %rax, strings(%rsp)
        mov     %rax, strings(%rsp)

        inc     %ebx
        jmp     1b

2:      call_fn reverse, strings(%rsp)
        call_fn jit_maybe_add_to_constant_pool, %rax
        return  %rax

build_environment_alist:        # list
        prologue env_var, env_var_name, save_ptr
        mov     %rdi, %rbx
        mov     %rdi, %r12
1:      is_nil_internal %r12
        je      3f

        call_fn car, %r12
        unbox_pointer_internal %rax
        add     $header_size, %rax
        call_fn strdup, %rax
        mov     %rax, env_var(%rsp)

        lea     save_ptr(%rsp), %rax
        call_fn strtok_r, env_var(%rsp), $equals_sign, %rax
        call_fn box_string, %rax
        mov     %rax, env_var_name(%rsp)

        lea     save_ptr(%rsp), %rax
        call_fn strtok_r, $NULL, $empty_string_c, %rax
        test    %rax, %rax
        jnz     2f
        mov     $empty_string_c, %rax

2:      call_fn box_string, %rax
        call_fn cons, env_var_name(%rsp), %rax
        call_fn set_car, %r12, %rax

        call_fn free, env_var(%rsp)

        call_fn cdr, %r12
        mov     %rax, %r12
        jmp     1b

3:      return  %rbx

        ## Public API

void:
        mov     $VOID, %rax
        ret


read_all:                       # port
        prologue
        mov     %rdi, %rbx
1:      call_fn read, %rbx
        is_eof_object_internal %rax
        je      2f
        call_fn eval, %rax
        jmp     1b
2:      return  $TRUE

class_of:                       # obj
        prologue
        mov     %rdi, %rbx
        extract_tag
        cmp     $TAG_OBJECT, %rax
        je      1f
        tag     TAG_SYMBOL, %rax
        return
1:      is_eof_object_internal %rbx
        jne     2f
        return  eof_symbol
2:      is_void_internal  %rbx
        jne     3f
        return  void_symbol
3:      xor     %eax, %eax
        unbox_pointer_internal %rbx, %r11
        mov     header_object_type(%r11), %ax
        tag     TAG_SYMBOL, %rax
        return

object_space_size:
        mov     stack_top_offset + object_space, %rax
        shr     $POINTER_SIZE_SHIFT, %rax
        box_int_internal
        ret

        ## Boxing from C

box_boolean:                    # c-boolean
        and     $C_TRUE, %edi
        box_boolean_internal %rdi
        ret

box_int:                        # c-int
        box_int_internal %edi
        ret

box_string:                     # c-string
        prologue str, size
        mov     %rdi, %rbx
        cmp     $NULL, %rbx
        je      1f
        cmpb    $0, (%rbx)
        jz      2f

1:      open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $string_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        return

2:      return empty_string

        ## Unboxing to C

unbox_double:                   # double
        mov     %rdi, %rax
        ret

unbox_boolean:                  # boolean
unbox_char:                     # char
unbox_symbol:                   # symbol
unbox_integer:                  # int
        movsx   %edi, %rax
        ret

unbox_pair:                     # pair
        is_nil_internal %rdi
        je      1f
        unbox_pointer_internal %rdi
        add     $header_size, %rax
1:      mov     $NULL, %rax
        ret

unbox_object:                   # object
        is_void_internal %rdi
        jz      1f
        unbox_pointer_internal %rdi
        add     $header_size, %rax
1:      ret

unbox_procedure:                # procedure
unbox_port:                     # port
        unbox_pointer_internal %rdi
        ret

unbox_string:                   # string
unbox_vector:                   # vector
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        ret

unbox:                          # value
        minimal_prologue
        tagged_jump unbox_jump_table
        return

        ## Runtime Internals

        ## Stack ADT
init_pointer_stack:             # a-stack, c-size
        prologue
        mov     %rdi, %rbx
        movq    %rsi, stack_max_size(%rbx)
        call_fn malloc, %rsi
        perror
        mov     %rax, stack_bottom(%rbx)
        movq    $0, stack_top_offset(%rbx)
        return  %rbx

resize_pointer_stack:           # a-stack
        prologue
        mov     %rdi, %rbx
        shlq    stack_max_size(%rbx)
        call_fn realloc, stack_bottom(%rdi), stack_max_size(%rbx)
        perror
        mov     %rax, stack_bottom(%rbx)
        return  %rbx

push_pointer_on_stack:          # a-stack, pointer
        prologue
        mov     stack_top_offset(%rdi), %rcx
        cmp     stack_max_size(%rdi), %rcx
        jl      1f
        mov     %rsi, %rbx
        call_fn resize_pointer_stack, %rdi
        mov     %rax, %rdi
        mov     %rbx, %rsi
1:      mov     stack_bottom(%rdi), %r11
        mov     stack_top_offset(%rdi), %rcx
        mov     %rsi, (%r11,%rcx)
        add     $POINTER_SIZE, stack_top_offset(%rdi)
        return  %rsi

pop_pointer_from_stack:         # a-stack
        sub     $POINTER_SIZE, stack_top_offset(%rdi)
        mov     stack_top_offset(%rdi), %rcx
        mov     stack_bottom(%rdi), %r11
        mov     (%r11,%rcx), %rax
        ret

        ## Garbage Collection

gc_allocate_memory:             # c-size
        prologue
        mov     %rdi, %rbx
        call_fn malloc, %rbx
        cmp    $NULL, %rax
        jg     1f
        call_fn gc
        call_fn malloc, %rbx
        perror
1:      return

gc_mark_nop:                    # obj
        ret

gc_mark_string:                 # string
        unbox_pointer_internal %rdi
        btsw    $GC_MARK_BIT, header_object_mark(%rax)
        ret

gc_is_markable_object:          # pointer
        minimal_prologue
        is_nil_internal %rdi
        je      1f
        is_void_internal %rdi
        je      1f
        is_eof_object_internal %rdi
        je      1f
        test    %rdi, %rdi
        js      1f
        return  $C_TRUE
1:      return  $C_FALSE

gc_mark_object:                 # pointer
        prologue
        mov     %rdi, %rbx

        call_fn gc_is_markable_object, %rdi
        cmp     $C_TRUE, %rax
        jne     1f

        unbox_pointer_internal %rbx
        btsw    $GC_MARK_BIT, header_object_mark(%rax)
        jc      1f
        call_fn push_pointer_on_stack, $gc_mark_stack, %rbx
1:      return

gc_maybe_mark:                  # obj
        minimal_prologue
        tagged_jump gc_mark_jump_table
        return

gc_mark_queue_pair:             # pair
        prologue
        is_nil_internal %rdi
        je      1f
        unbox_pointer_internal %rdi, %rbx
        call_fn gc_maybe_mark, pair_car(%rbx)
        call_fn gc_maybe_mark, pair_cdr(%rbx)
1:      return

gc_mark_queue_vector:           # vector
        prologue
        unbox_pointer_internal %rdi
        mov     %rax, %r12
        mov     header_object_size(%rax), %ebx
1:      test    %ebx, %ebx
        jz      2f

        sub     $POINTER_SIZE, %ebx
        mov     header_size(%r12,%rbx), %rdi
        call_fn gc_maybe_mark, %rdi
        jmp     1b
2:      return

gc_mark_queue_registers:
        minimal_prologue
        .irp callee_saved, %rbx, %rbp, %r12, %r13, %r14, %r15
        call_fn gc_maybe_mark, \callee_saved
        .endr
        return

gc_mark_queue_stack:
        prologue
        mov     %rsp, %rbx
1:      cmp     execution_stack_top, %rbx
        je      2f

        call_fn gc_maybe_mark, (%rbx)
        add     $POINTER_SIZE, %rbx
        jmp     1b
2:      return


gc_mark_queue_global_variables:
        prologue
        mov     symbol_next_id, %rbx
1:      test    %rbx, %rbx
        jz      2f

        dec     %rbx
        cmpq    $NULL, symbol_table_names(,%rbx,POINTER_SIZE)
        je      1b

        mov     symbol_table_values(,%rbx,POINTER_SIZE), %rdi
        call_fn gc_maybe_mark, %rdi
        jmp     1b
2:      return

gc_mark_queue_constant_pool:
        prologue

        xor     %rbx, %rbx
1:      cmp     %rbx, stack_top_offset + constant_pool
        je      2f

        mov     stack_bottom + constant_pool, %rax
        mov     (%rax,%rbx), %rdi
        call_fn gc_maybe_mark, %rdi

        add     $POINTER_SIZE, %rbx
        jmp     1b

2:      return

gc_mark:
        minimal_prologue
        call_fn gc_mark_queue_registers
        call_fn gc_mark_queue_stack
        call_fn gc_mark_queue_global_variables
        call_fn gc_mark_queue_constant_pool

1:      cmpq    $0, stack_top_offset + gc_mark_stack
        je      2f
        call_fn pop_pointer_from_stack, $gc_mark_stack
        mov    %rax, %rdi
        tagged_jump gc_mark_queue_jump_table
        jmp     1b
2:      return

gc_sweep:
        prologue
        xor     %rbx, %rbx
1:      cmp     %rbx, stack_top_offset + object_space
        je      3f

        mov     stack_bottom + object_space, %rax
        mov     (%rax,%rbx), %r11
        unbox_pointer_internal %r11

        btrw    $GC_MARK_BIT, header_object_mark(%rax)
        jc      2f

        call_fn free, %rax
        call_fn pop_pointer_from_stack, $object_space
        mov     stack_bottom + object_space, %r11
        mov     %rax, (%r11,%rbx)
        jmp     1b

2:      add     $POINTER_SIZE, %rbx
        jmp     1b
3:      return

gc:
        prologue
        call_fn gc_mark
        call_fn gc_sweep

        call_fn object_space_size
        return

        ## Printer
vector_to_string:               # vector
        prologue str, stream, size
        unbox_pointer_internal %rdi, %rbx

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'\#, stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

        xor     %r12d, %r12d
1:      cmp     header_object_size(%rbx), %r12d
        je      3f
        test    %r12d, %r12d
        jz      2f

        call_fn fputc, $' , stream(%rsp)

2:      mov     header_size(%rbx,%r12), %rax
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        add     $POINTER_SIZE, %r12
        jmp     1b

3:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), size(%rsp), stream(%rsp)
        register_for_gc
        return

bytevector_to_string:           # bytevector
        prologue str, stream, size
        unbox_pointer_internal %rdi, %rbx

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'\#, stream(%rsp)
        call_fn fputc, $'u, stream(%rsp)
        call_fn fputc, $'8, stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

        xor     %r12d, %r12d
1:      cmp     header_object_size(%rbx), %r12d
        je      3f
        test    %r12d, %r12d
        jz      2f

        call_fn fputc, $' , stream(%rsp)

2:      mov     header_size(%rbx,%r12), %al
        call_fn fprintf, stream(%rsp), $int_format, %rax

        inc     %r12
        jmp     1b

3:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), size(%rsp), stream(%rsp)
        register_for_gc
        return

pair_to_string:                 # pair
        prologue str, size, stream
        mov     %rdi, %r12

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

1:      is_nil_internal %r12
        je      2f

        call_fn car, %r12
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        call_fn cdr, %r12
        mov     %rax, %r12
        is_nil_internal %r12
        je      2f

        call_fn fputc, $' , stream(%rsp)

        has_tag TAG_PAIR, %r12, store=false
        je      1b

        call_fn fputc, $'., stream(%rsp)
        call_fn fputc, $' , stream(%rsp)
        call_fn to_string, %r12
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

2:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), size(%rsp), stream(%rsp)
        register_for_gc
        return

char_to_string:                 # char
        prologue str, size
        mov     %edi, %ebx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $char_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

char_to_machine_readable_string: # char
        prologue str, size
        mov     %edi, %ebx
        test    %ebx, %ebx
        jnz     1f
        return  null_char

1:      mov     char_to_string_table(,%ebx,POINTER_SIZE), %rax
        test    %rax, %rax
        jz      2f
        return
2:      open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $machine_readable_char_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

integer_to_string:              # int, radix
        prologue str, size, format
        default_arg TAG_INT, $10, %rsi
        mov     %esi, %esi

        mov     integer_to_string_format_table(,%rsi,8), %rax
        mov     %rax, format(%rsp)

        movsx   %edi, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, format(%rsp), %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

double_to_string:               # double
        prologue str, size
        movq   %rdi, %xmm0
        open_string_buffer str(%rsp), size(%rsp), %r12
        mov    %r12, %rdi
        mov    $double_format, %rsi
        mov    $1, %al         # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        call   fprintf
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

boolean_to_string:              # boolean
        and    $C_TRUE, %edi
        mov    boolean_string_table(,%edi,POINTER_SIZE), %rax
        ret

string_to_string:               # string
        mov     %rdi, %rax
        ret

string_to_machine_readable_string: # string
        prologue str, size, stream

        unbox_pointer_internal %rdi, %rbx
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)

        call_fn fputc, $'\", stream(%rsp)
        test    %ebx, %ebx
        jz      4f

        mov     $header_size, %r12
1:      xor     %eax, %eax
        mov     (%rbx,%r12), %al
        test    %al, %al
        jz      4f

        xor     %r11d, %r11d
        mov     escape_char_table(%eax), %r11b
        test    %r11b, %r11b
        jz      2f

        xor     %al, %al
        call_fn fprintf, stream(%rsp), $machine_readable_escape_code_format, %r11
        jmp     3f

2:      call_fn fputc, %rax, stream(%rsp)

3:      incq    %r12
        jmp     1b

4:      call_fn fputc, $'\", stream(%rsp)

        string_buffer_to_string str(%rsp), size(%rsp), stream(%rsp)
        register_for_gc
        return

object_to_string:               # obj
        prologue obj, str, size
        mov     %rdi, obj(%rsp)
        call_fn class_of, %rdi
        mov     %rax, %rbx
        unbox_pointer_internal %rax, %rcx
        mov     to_string_jump_table(,%rcx,POINTER_SIZE), %r11
        cmp     $object_to_string, %r11
        je      1f
        test    %r11, %r11
        jnz     2f
1:      call_fn to_string, %rbx
        call_fn unbox_string, %rax
        mov     %rax, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12
        call_fn fprintf, %r12, $object_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return
2:      mov     obj(%rsp), %rdi
        call    *%r11
        return

to_string:                      # value
        minimal_prologue
        tagged_jump to_string_jump_table
        return

        ## Reader

read_whitespace:                # c-stream
        prologue
        mov     %rdi, %rbx

1:      call_fn fgetc, %rbx
        mov     %rax, %r12
        cmp     $EOF, %al
        je      2f

        call_fn isspace, %rax
        jnz     1b

        call_fn ungetc, %r12, %rbx
        return

2:      tag     TAG_CHAR, %rax
        return

read_comment:                   # c-stream
        prologue
        mov     %rdi, %rbx

        call_fn fgetc, %rbx
        cmp     $';, %al
        jne     2f

1:      call_fn fgetc, %rbx
        cmp     $'\n, %al
        je      3f
        jmp     1b

2:      call_fn ungetc, %rax, %rbx
3:      return

read_hashbang:                  # c-stream
        prologue
        mov     %rdi, %rbx

1:      call_fn fgetc, %rbx
        cmp     $'\n, %al
        je      3f
        jmp     1b

3:      return

read_datum:                     # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn fgetc, %rbx
        cmp     $EOF, %eax
        je      1f
        call_fn ungetc, %rax, %rbx

        call_fn read_comment, %rbx
        call_fn read_whitespace, %rbx
        cmp     $EOF, %eax
        je      1f
        call_fn read_comment, %rbx
        call_fn fgetc, %rbx
        cmp     $EOF, %eax
        je      1f
        read_byte_jump read_datum_jump_table, %rbx, %rax
        return

1:      return  $EOF_OBJECT

read_hash:                      # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn fgetc, %rbx
        read_byte_jump read_hash_jump_table, %rbx, %rax
        return

read_token:                     # c-stream
        prologue str
        mov     %rdi, %rbx
        lea     str(%rsp), %rdx
        xor     %al, %al
        call_fn fscanf, %rbx, $token_format, %rdx
        perror  jge
        call_fn box_string, str(%rsp)
        mov     %rax, %rbx
        call_fn free, str(%rsp)
        return  %rbx

read_symbol:                    # c-stream, c-char
        prologue str
        mov     %rdi, %rbx
        mov     %rsi, %rdi
        call_fn ungetc, %rdi, %rbx
        call_fn read_token, %rbx
        mov     %rax, str(%rsp)
        call_fn unbox, %rax
        mov     %rax, %rbx

        xor     %r12d, %r12d
        xor     %eax, %eax
1:      mov     (%rbx,%r12), %al
        test    %al, %al
        jz      2f

        call_fn tolower, %rax
        movb    %al, (%rbx,%r12)
        inc     %r12d
        jmp     1b

2:      call_fn string_to_symbol, str(%rsp)
        mov     %rax, %rbx
        register_for_gc str(%rsp)
        return  %rbx

read_number:                    # c-stream, c-char
        read_number_template $DECIMAL_RADIX_INT, unget=true

read_binary_number:             # c-stream, c-char
        read_number_template $BINARY_RADIX_INT

read_octal_number:              # c-stream, c-char
        read_number_template $OCTAL_RADIX_INT

read_decimal_number:            # c-stream, c-char
        read_number_template $DECIMAL_RADIX_INT

read_hexadecimal_number:        # c-stream, c-char
        read_number_template $HEX_RADIX_INT

read_number_or_symbol:          # c-stream, c-char
        prologue sign
        mov     %rdi, %rbx
        mov     %rsi, sign(%rsp)
        call_fn fgetc, %rbx
        mov     %rax, %r12
        call_fn ungetc, %r12, %rbx
        call_fn isdigit, %r12
        jz      2f
        call_fn read_number %rbx, sign(%rsp)
        return
2:      call_fn read_symbol, %rbx, sign(%rsp)
        return

read_string:                    # c-stream, c-char
        prologue str, size, hex, idx
        mov     %rdi, %rbx
        movq    $-1, idx(%rsp)
        open_string_buffer str(%rsp), size(%rsp), %r12

1:      incq    idx(%rsp)
        call_fn fgetc, %rbx
        cmp     $'\", %rax
        je      5f

        cmp     $EOF, %al
        je      7f

        cmp     $'\\, %rax
        jne     3f
        call_fn fgetc, %rbx

        cmp     $'\n, %al
        je      1b

        cmp     $'x, %al
        je      2f

        mov     unescape_char_table(%eax), %al
        test    %al, %al
        jz      7f
        jmp     3f

2:      lea     hex(%rsp), %rdx
        call_fn fscanf, %rbx, $hex_format, %rdx
        perror  jge

        call_fn fputc, hex(%rsp), %r12
        call_fn fgetc, %rbx
        cmp     $';, %al
        jne     7f

        jmp     1b

3:      call_fn fputc, %rax, %r12
        jmp     1b

5:      cmpq    $0, idx(%rsp)
        je      6f

        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

6:      return  empty_string

7:      call_fn error, read_error_string
        return

read_true:                      # c-stream
        mov     $TRUE, %rax
        ret

read_false:                     # c-stream
        mov     $FALSE, %rax
        ret

read_character:                 # c-stream, c-char
        prologue
        mov     %rdi, %rbx
        call_fn read_token, %rbx
        register_for_gc
        mov     %rax, %r12
        call_fn string_length, %r12
        cmp     $1, %eax
        je      4f

        mov     $CHAR_TABLE_SIZE, %rbx
        unbox_pointer_internal %r12, %r12
1:      test    %ebx, %ebx
        jz      2f
        dec     %ebx
        mov     char_to_string_table(,%rbx,POINTER_SIZE), %rax
        unbox_pointer_internal %rax

        test    %rax, %rax
        jz      1b

        add     $header_size + CHAR_PREFIX_LENGTH, %rax
        lea     header_size(%r12), %r11
        call_fn strcmp, %r11, %rax
        jnz     1b

        tag     TAG_CHAR, %rbx
        return

        ## TODO: figure out why 0 (null) offset doesn't work here.
2:      mov     null_char, %rax
        unbox_pointer_internal %rax
        add     $header_size + CHAR_PREFIX_LENGTH, %rax
        lea     header_size(%r12), %r11
        call_fn strcmp, %r11, %rax
        jnz     3b

        tag     TAG_CHAR, $0
        return

3:      call_fn error, read_error_string
        return

4:      call_fn string_ref, %r12, $ZERO_INT
        return

read_quote:                     # c-stream
        minimal_prologue
        call_fn read_datum, %rdi
        call_fn cons, %rax, $NIL
        call_fn cons, quote_symbol, %rax
        return

read_quasiquote:                # c-stream
        minimal_prologue
        call_fn read_datum, %rdi
        call_fn cons, %rax, $NIL
        call_fn cons, quasiquote_symbol, %rax
        return

read_unquote:                   # c-stream
        prologue
        mov     %rdi, %rbx
        mov     unquote_symbol, %r12
        call_fn fgetc, %rbx
        cmp     $'@, %al
        je      1f
        call_fn ungetc, %rax, %rbx
        jmp     2f
1:      mov     unquote_splicing_symbol, %r12
2:      call_fn read_datum, %rbx
        call_fn cons, %rax, $NIL
        call_fn cons, %r12, %rax
        return

read_vector:                    # c-stream
        prologue
        call_fn read_list, %rdi
        call_fn list_to_vector, %rax
        return

read_bytevector:                # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn fgetc, %rbx
        cmp     $'8, %al
        jne     1f
        call_fn fgetc, %rbx
        cmp     $'(, %al
        jne     1f

        call_fn read_list, %rbx
        call_fn list_to_bytevector, %rax
        return

1:      call_fn error, read_error_string
        return

read_list:                      # c-stream
        prologue head
        mov     %rdi, %rbx

        mov     $NIL, %r12
        mov     %r12, head(%rsp)
1:      call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     $'), %al
        je      4f

        call_fn ungetc, %rax, %rbx

        call_fn read_datum, %rbx
        cmp     dot_symbol, %rax
        je      3f

        call_fn cons, %rax, $NIL
        is_nil_internal %r12
        jne     2f
        mov     %rax, %r12
        mov     %r12, head(%rsp)
        jmp     1b

2:      mov     %r12, %r11
        mov     %rax, %r12
        call_fn set_cdr, %r11, %r12
        jmp     1b

3:      call_fn read_datum, %rbx
        call_fn set_cdr, %r12, %rax

        call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     $'), %al
        je      4f
        call_fn error, read_error_string

4:      return  head(%rsp)

        ## JIT

jit_write_code_to_disk:         # c-code, c-size
        prologue filename, file
        testq   $C_TRUE, jit_code_debug
        jz      1f
        mov     %rdi, %rbx
        mov     %rsi, %r12

        call_fn mkdir, $jit_code_directory, $0777

        lea     filename(%rsp), %r11
        incq    jit_code_file_counter
        xor     %al, %al
        call_fn asprintf, %r11, $jit_code_file_format, jit_code_file_counter
        perror
        call_fn fopen, filename(%rsp), $write_mode
        perror
        mov     %rax, file(%rsp)
        call_fn fwrite, %rbx, $1, %r12, file(%rsp)
        call_fn fclose, file(%rsp)
        perror  jz
        call_fn free, filename(%rsp)
1:      return

jit_allocate_code:              # c-code, c-size
        prologue code, size
        mov     %rdi, code(%rsp)
        mov     %rsi, size(%rsp)
        call_fn jit_write_code_to_disk, %rdi, %rsi

        mov     jit_code_space_next_address, %r12
        add     size(%rsp), %r12
        add     $(2 * POINTER_SIZE), %r12
        and     $-(2 * POINTER_SIZE), %r12

        mov     jit_code_space, %r11
        add     jit_code_space_size, %r11
        cmp     %r11, %r12
        jge     1f

        mov     jit_code_space_next_address, %rbx
        call_fn memcpy, %rbx, code(%rsp), size(%rsp)
        perror

        mov     %r12, jit_code_space_next_address

        return  %rbx
1:      call_fn error, code_space_error_string
        return

jit_code:                       # form, environment, arguments
        prologue env, args, code, size
        mov     %rdi, %r12
        mov     %rsi, env(%rsp)
        mov     %rdx, args(%rsp)
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %rbx

        call_fn jit_procedure, %r12, %rbx, env(%rsp), args(%rsp)
        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        return

jit_datum:                      # form, c-stream, environment, register, tail
        minimal_prologue
        tagged_jump jit_jump_table
        return

        ## 4.1.1. Variable references

jit_index_of_local:             # environment, symbol
        prologue symbol
        mov     %rdi, %r12
        mov     %rsi, symbol(%rsp)
        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      2f

        call_fn car, %r12
        cmp     symbol(%rsp), %rax
        je      3f

        call_fn cdr, %r12
        mov     %rax, %r12
        inc     %rbx
        jmp     1b

2:      mov     $-1, %rbx
3:      return  %rbx

jit_symbol:                     # symbol, c-stream, environment, register, tail
        prologue env, env_size, symbol_address, symbol, local, register
        mov     %rdi, symbol(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        call_fn length, %rdx
        mov     %rax, env_size(%rsp)

        call_fn jit_index_of_local, env(%rsp), symbol(%rsp)
        cmp     $0, %rax
        jge     3f

2:      mov     symbol(%rsp), %rdi
        lea     symbol_table_values(,%edi,POINTER_SIZE), %rax
        mov     %rax, symbol_address(%rsp)

        call_fn fwrite, $jit_global_to_rax, $1, jit_global_to_rax_size, %r12
        lea     symbol_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        jmp     4f

3:      mov     %rax, %rbx
        sub     env_size(%rsp), %ebx
        neg     %ebx
        shl     $POINTER_SIZE_SHIFT, %ebx
        neg     %ebx
        mov     %ebx, local(%rsp)
        mov     register(%rsp), %rbx
        mov     jit_local_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_local_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12

4:      call_fn length, env(%rsp)
        return

        ## 4.1.2. Literal expressions

jit_add_to_constant_pool_nop:   # obj
        ret

jit_add_to_constant_pool:       # obj
        prologue
        mov     %rdi, %rbx

        call_fn gc_is_markable_object, %rdi
        cmp     $C_TRUE, %rax
        jne     1f

1:      call_fn push_pointer_on_stack, $constant_pool, %rbx
        return

jit_maybe_add_to_constant_pool: # obj
        minimal_prologue
        tagged_jump jit_constant_pool_jump_table
        return

jit_quote:                      # form, c-stream, environment, register, tail
        prologue env, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        call_fn cdr, %rbx
        call_fn car, %rax
        call_fn jit_literal, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

jit_literal:                    # literal, c-stream, environment, register, tail
        prologue env, literal, register, tail
        mov     %rdi, literal(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)

        call_fn jit_maybe_add_to_constant_pool, %rdi

        mov     register(%rsp), %rbx
        mov     jit_literal_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_literal_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        lea     literal(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12, tail(%rsp)

        call_fn length, env(%rsp)
        return

jit_pair:                       # form, c-stream, environment, register, tail
        prologue env, symbol, syntax, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)

        call_fn car, %rbx
        mov     %rax, symbol(%rsp)
        has_tag TAG_SYMBOL, symbol(%rsp), store=false
        jne     3f

        call_fn jit_index_of_local, env(%rsp), symbol(%rsp)
        cmp     $0, %rax
        jge     3f

2:      unbox_pointer_internal symbol(%rsp)
        mov     jit_syntax_jump_table(,%rax,8), %rax
        test    %eax, %eax
        jnz     4f

3:      call_fn jit_procedure_call, %rbx, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

4:      mov     %rax, syntax(%rsp)
        has_tag TAG_PROCEDURE, %rax, store=false
        je      5f
        unbox_pointer_internal syntax(%rsp)
        call_fn *%rax, %rbx, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

5:      unbox_pointer_internal syntax(%rsp), %rax
        call_fn *%rax, %rbx, env(%rsp)
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

        ## 4.1.3. Procedure calls

jit_procedure_call:             # form, c-stream, environment, register, tail
        prologue form, len, operand, literal, env, max_locals, register, tail
        mov     %rdi, %rbx
        mov     %rbx, form(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn length, %rbx
        mov     %rax, len(%rsp)

        call_fn car, %rbx
        mov     %rax, operand(%rsp)
        has_tag TAG_PAIR, %rax, store=false
        jne     1f
        call_fn jit_datum, operand(%rsp), %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx

1:      is_nil_internal %rbx
        je      4f

        call_fn car, %rbx
        has_tag TAG_PAIR, %rax, store=false
        je      2f

        jmp     3f

2:      call_fn car, %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

3:      call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

4:      mov     len(%rsp), %rbx
        dec     %ebx
        call_fn cdr, form(%rsp)
        call_fn reverse, %rax
        mov     %rax, form(%rsp)

5:      test    %ebx, %ebx
        jz      8f
        dec     %ebx

        call_fn car, form(%rsp)
        has_tag TAG_PAIR, %rax, store=false
        je      6f

        call_fn car, form(%rsp)
        xor     %r11d, %r11d
        mov     jit_argument_to_register_id_table(%rbx), %r11b
        call_fn jit_datum, %rax, %r12, env(%rsp), %r11, $C_FALSE
        update_max_locals max_locals(%rsp)

        jmp     7f

6:      xor     %r11d, %r11d
        mov     jit_argument_to_register_id_table(%rbx), %r11b
        mov     jit_pop_register_table(,%r11,POINTER_SIZE), %rax
        mov     jit_pop_register_size_table(,%r11,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

7:      call_fn cdr, form(%rsp)
        mov     %rax, form(%rsp)

        jmp     5b

8:      has_tag TAG_PAIR, operand(%rsp), store=false
        jne     9f

        call_fn fwrite, $jit_pop_rax, $1, jit_pop_rax_size, %r12
        jmp     10f

9:      call_fn jit_datum, operand(%rsp), %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)

10:     call_fn fwrite, $jit_unbox_rax, $1, jit_unbox_rax_size, %r12
        cmp     $C_TRUE, tail(%rsp)
        je      12f

11:     call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12
        jmp     13f

12:     call_fn fwrite, $jit_tco_adjust_stack, $1, jit_tco_adjust_stack_size, %r12
        call_fn fwrite, $jit_jump_rax, $1, jit_jump_rax_size, %r12

        return  max_locals(%rsp)

13:     mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

        return  max_locals(%rsp)

        ## 4.1.4. Procedures

jit_procedure:                  # form, c-stream, environment, arguments
        prologue env, args, env_size, frame_size, local_idx, local, prologue_offset, end_offset
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, args(%rsp)

        call_fn fwrite, $jit_prologue, $1, jit_prologue_size, %r12
        call_fn ftell, %r12
        mov     %rax, prologue_offset(%rsp)

        call_fn length, env(%rsp)
        mov     %eax, env_size(%rsp)

        call_fn length, args(%rsp)
        mov     %eax, local_idx(%rsp)

1:      mov     local_idx(%rsp), %ecx
        test    %ecx, %ecx
        jz      2f
        dec     %ecx
        mov     %ecx, local_idx(%rsp)

        xor     %r11d, %r11d
        mov     jit_argument_to_register_id_table(%rcx), %r11b
        mov     jit_register_to_local_table(,%r11,POINTER_SIZE), %rax
        mov     jit_register_to_local_size_table(,%r11,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        mov     local_idx(%rsp), %eax
        add     env_size(%rsp), %eax
        shl     $POINTER_SIZE_SHIFT, %eax
        add     $POINTER_SIZE, %eax
        neg     %eax
        mov     %eax, local(%rsp)
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12
        jmp     1b

2:      call_fn reverse, args(%rsp)
        call_fn append, %rax, env(%rsp)
        call_fn jit_datum, %rbx, %r12, %rax, $RAX, $C_TRUE

        shl     $POINTER_SIZE_SHIFT, %eax
        add     $POINTER_SIZE, %eax
        and     $-(2 * POINTER_SIZE), %eax
        mov     %eax, frame_size(%rsp)

        call_fn ftell, %r12
        mov     %rax, end_offset(%rsp)
        mov     prologue_offset(%rsp), %rax
        sub     $INT_SIZE, %rax
        call_fn fseek, %r12, %rax, $SEEK_SET

        lea     frame_size(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12
        call_fn fseek, %r12, end_offset(%rsp), $SEEK_SET

        call_fn fwrite, $jit_epilogue, $1, jit_epilogue_size, %r12
        return

jit_lambda_factory_code:        # lambda, c-stream, env-size
        prologue  rbp, local_idx, local, env_size
        unbox_pointer_internal %rdi, %r12
        mov     %rsi, %rbx
        mov     %rbp, rbp(%rsp)
        movq    $0, local_idx(%rsp)
        mov     %edx, env_size(%rsp)

        call_fn jit_literal, %r12, %rbx, $NIL, $RAX, $C_FALSE

        sub     $POINTER_SIZE, rbp(%rsp)
1:      cmp     $0, env_size(%rsp)
        je      2f
        mov     rbp(%rsp), %rax
        call_fn jit_maybe_add_to_constant_pool, (%rax)
        mov     rbp(%rsp), %rax
        call_fn jit_literal, (%rax), %rbx, $NIL, $RAX, $C_FALSE
        sub     $POINTER_SIZE, rbp(%rsp)

        decl    env_size(%rsp)
        incl    local_idx(%rsp)
        mov     local_idx(%rsp), %ecx
        shl     $POINTER_SIZE_SHIFT, %rcx
        add     $POINTER_SIZE, %rcx
        neg     %ecx
        mov     %ecx, local(%rsp)
        call_fn fwrite, $jit_rax_to_closure, $1, jit_rax_to_closure_size, %rbx
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %rbx

        jmp     1b

2:      call_fn jit_literal, %r12, %rbx, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_jump_rax, $1, jit_jump_rax_size, %rbx
        return

jit_lambda_factory:             # lambda, env-size
        prologue code, size, env_size
        mov     %rdi, %r12
        mov     %esi, env_size(%rsp)
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %rbx

        call_fn jit_lambda_factory_code, %r12, %rbx, env_size(%rsp)
        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        tag     TAG_PROCEDURE, %rax
        return

jit_lambda_patch_factory:       # lambda-factory, env-size
        prologue code, env_size, lambda
        unbox_pointer_internal %rdi, %r12
        mov     %edx, env_size(%rsp)

        call_fn fmemopen, %r12, $PAGE_SIZE, $read_mode
        perror
        mov     %rax, %rbx

        call_fn fseek, %rbx, jit_literal_to_rax_size
        lea     lambda(%rsp), %rax
        call_fn fread, %rax, $1, $POINTER_SIZE, %rbx
        call_fn fclose, %rbx
        perror  je

        call_fn fmemopen, %r12, $PAGE_SIZE, $write_mode
        perror
        mov     %rax, %rbx

        call_fn jit_lambda_factory_code, lambda(%rsp), %rbx, env_size(%rsp)
        call_fn fclose, %rbx
        perror  je

        tag     TAG_PROCEDURE, %r12
        return

jit_lambda:                     # form, c-stream, environment, register, tail
        prologue env, args, form, lambda, register
        mov     %rdi, %rbx
        mov     %rdi, form(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rbx
        mov     %rax, args(%rsp)

        call_fn cdr, %rbx
        call_fn cons, begin_symbol, %rax
        mov     %rax, %rbx

        call_fn jit_code, %rbx, env(%rsp), args(%rsp)
        mov     %rax, lambda(%rsp)
        call_fn length, env(%rsp)
        test    %eax, %eax
        jz      1f

        call_fn jit_literal, lambda(%rsp), %r12, $NIL, $RDI, $C_FALSE

        call_fn length, env(%rsp)
        call_fn jit_literal, %rax, %r12, $NIL, $RSI, $C_FALSE

        mov     $jit_lambda_factory, %rax
        call_fn jit_literal, %rax, %r12, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12
        mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

        call_fn length, env(%rsp)
        return

1:      tag     TAG_PROCEDURE, lambda(%rsp)
        call_fn jit_literal, %rax, %r12, $NIL, register(%rsp), $C_FALSE

        call_fn length, env(%rsp)
        return

        ## 4.1.5. Conditionals

jit_if:                         # form, c-stream, environment, register, tail
        prologue if_offset, else_offset, end_offset, jump_offset, env, max_locals, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_conditional_rax_is_false_jump, $1, jit_conditional_rax_is_false_jump_size, %r12

        call_fn ftell, %r12
        mov     %rax, if_offset(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_unconditional_jump, $1, jit_unconditional_jump_size, %r12

        patch_jump %r12, else_offset(%rsp), if_offset(%rsp), jump_offset(%rsp)

        call_fn cdr, %rbx
        is_nil_internal %rax
        jne     1f
        mov     $VOID, %rax
        jmp     2f
1:      call_fn car, %rax
2:      call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        patch_jump %r12, end_offset(%rsp), else_offset(%rsp), jump_offset(%rsp)

        return  max_locals(%rsp)

        ## 4.1.6. Assignments

jit_set_with_rax_as_value:      # symbol, c-stream, environment
        prologue env, env_size, symbol, symbol_address, local
        mov     %rdi, symbol(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        call_fn length, %rdx
        mov     %rax, env_size(%rsp)

        call_fn jit_index_of_local, env(%rsp), symbol(%rsp)
        cmp     $0, %rax
        jge     3f

2:      mov     symbol(%rsp), %rax
        lea     symbol_table_values(,%eax,POINTER_SIZE), %rax
        mov     %rax, symbol_address(%rsp)

        call_fn fwrite, $jit_rax_to_global, $1, jit_rax_to_global_size, %r12
        lea     symbol_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        call_fn fwrite, $jit_void_to_rax, $1, jit_void_to_rax_size, %r12
        return

3:      mov     %rax, %rbx
        sub     env_size(%rsp), %ebx
        neg     %ebx
        shl     $POINTER_SIZE_SHIFT, %rbx
        neg     %ebx
        mov     %ebx, local(%rsp)
        call_fn fwrite, $jit_rax_to_local, $1, jit_rax_to_local_size, %r12
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12

        call_fn fwrite, $jit_void_to_rax, $1, jit_void_to_rax_size, %r12
        return

jit_set:                        # form, c-stream, environment, register, tail
        prologue env, max_locals, symbol
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rbx
        mov     %rax, symbol(%rsp)

        call_fn cdr, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        mov     %rax, max_locals(%rsp)

        call_fn jit_set_with_rax_as_value, symbol(%rsp), %r12, env(%rsp)
        return  max_locals(%rsp)

        ## 4.2.2. Binding constructs

jit_let_collect_bindings:       # bindings
        prologue
        mov     %rdi, %rbx
        mov     $NIL, %r12

1:      is_nil_internal %rbx
        je      2f
        call_fn car, %rbx
        call_fn car, %rax
        call_fn cons, %rax, %r12
        mov     %rax, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return  %r12

jit_let_bindings:               # bindings, c-stream, environment, bindings-environment
        prologue env, bindings_env, variable_init, init, max_locals
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, bindings_env(%rsp)
        movq    $0, max_locals(%rsp)

1:      is_nil_internal %rbx
        je      2f
        call_fn car, %rbx
        mov     %rax, variable_init(%rsp)

        call_fn cdr, variable_init(%rsp)
        call_fn car, %rax
        mov     %rax, init(%rsp)

        call_fn jit_datum, init(%rsp), %r12, bindings_env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)

        call_fn car, variable_init(%rsp)
        call_fn cons, %rax, env(%rsp)
        mov     %rax, env(%rsp)
        call_fn car, variable_init(%rsp)
        call_fn jit_set_with_rax_as_value, %rax, %r12, env(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return max_locals(%rsp)

jit_let:                        # form, c-stream, environment, register, tail
        prologue form, env, variable_init, max_locals, register, tail
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        call_fn cdr, %rdi
        mov     %rax, form(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn car, form(%rsp)
        has_tag TAG_SYMBOL, %rax, store=false
        jne     1f

        call_fn cons, r5rs_let_symbol, form(%rsp)
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        return

1:      call_fn car, form(%rsp)
        call_fn jit_let_bindings %rax, %r12, env(%rsp), env(%rsp)
        update_max_locals max_locals(%rsp)

        call_fn car, form(%rsp)
        call_fn jit_let_collect_bindings, %rax
        call_fn append, %rax, env(%rsp)
        mov    %rax, env(%rsp)

        call_fn cdr, form(%rsp)
        call_fn cons, begin_symbol, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        return  max_locals(%rsp)

jit_is_lambda:                  # form
        prologue
        mov     %rdi, %rbx
        is_nil_internal %rbx
        je      1f
        has_tag TAG_PAIR, %rbx, store=false
        jne     1f
        call_fn car, %rbx
        eq_internal lambda_symbol, %rax
        return
1:      return  $C_FALSE

jit_letrec:                     # form, c-stream, environment, register, tail
        prologue form, env, full_env, variable_init, local, max_locals, register, tail
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        call_fn cdr, %rdi
        mov     %rax, form(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn car, form(%rsp)
        call_fn jit_let_collect_bindings, %rax
        call_fn append, %rax, env(%rsp)
        mov    %rax, full_env(%rsp)

        call_fn car, form(%rsp)
        call_fn jit_let_bindings %rax, %r12, env(%rsp), full_env(%rsp)
        update_max_locals max_locals(%rsp)

        call_fn car, form(%rsp)
        mov     %rax, %rbx

1:      is_nil_internal %rbx
        je      3f

        call_fn car, %rbx
        call_fn cdr, %rax
        call_fn car, %rax

        call_fn jit_is_lambda, %rax
        cmp     $C_TRUE, %rax
        jne     2f

        call_fn car, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, full_env(%rsp), $RDI, $C_FALSE

        call_fn length, full_env(%rsp)
        call_fn jit_literal, %rax, %r12, $NIL, $RDX, $C_FALSE

        mov     $jit_lambda_patch_factory, %rax
        call_fn jit_literal, %rax, %r12, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12

2:      call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

3:      call_fn cdr, form(%rsp)
        call_fn cons, begin_symbol, %rax
        call_fn jit_datum, %rax, %r12, full_env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        return  max_locals(%rsp)

        ## 4.2.3. Sequencing

jit_begin:                     # form, c-stream, environment, register, tail
        prologue env, form, max_locals, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx

        is_nil_internal %rbx
        jne     2f

        call_fn cons, $VOID, $NIL
        mov     %rax, %rbx

2:      is_nil_internal %rbx
        je      5f

        call_fn cdr, %rbx
        is_nil_internal %rax
        je      3f

        call_fn car, %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), $C_FALSE
        jmp     4f

3:      call_fn car, %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)

4:      update_max_locals max_locals(%rsp)
        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     2b

5:      return  max_locals(%rsp)

        ## 5.3. Syntax definitions

jit_define_syntax:              # form, c-stream, environment, register, tail
        prologue env, max_locals, syntax_address
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        movq    $0, max_locals(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx

        call_fn cdr, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)

        call_fn car, %rbx
        lea     jit_syntax_jump_table(,%eax,POINTER_SIZE), %rax
        mov     %rax, syntax_address(%rsp)

        call_fn fwrite, $jit_rax_to_syntax, $1, jit_rax_to_syntax_size, %r12
        lea     syntax_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        call_fn fwrite, $jit_void_to_rax, $1, jit_void_to_rax_size, %r12
        return  max_locals(%rsp)

        ## 6.4. Control features

jit_call_with_current_continuation_escape: # return, jmp-buffer
        minimal_prologue
        movq    %rdi, %xmm0
        mov     %rsi, %rdi
        call_fn longjmp, %rdi, $C_TRUE
        return

jit_call_with_current_continuation_escape_factory: # jmp-buffer
        prologue  code, size
        mov     %rdi, %r12
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %rbx

        call_fn jit_literal, %r12, %rbx, $NIL, $RSI, $C_FALSE

        call_fn jit_literal, $jit_call_with_current_continuation_escape, %rbx, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_jump_rax, $1, jit_jump_rax_size, %rbx

        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        return

        .data
        .align  16
integer_to_string_format_table:
        .zero   16 * POINTER_SIZE

        .align  16
char_to_string_table:
        .zero   CHAR_TABLE_SIZE * POINTER_SIZE

        .align  16
escape_char_table:
        .zero   CHAR_TABLE_SIZE
unescape_char_table:
        .zero   CHAR_TABLE_SIZE

        .align  16
boolean_string_table:
        .zero   2 * POINTER_SIZE

        .align  16
to_string_jump_table:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

        .align  16
unbox_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
gc_mark_queue_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
gc_mark_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
jit_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
jit_constant_pool_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
jit_argument_to_register_id_table:
        .zero   MAX_REGISTER_ARGS

        .align  16
jit_pop_register_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_pop_register_size_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_literal_to_register_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_literal_to_register_size_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_local_to_register_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_local_to_register_size_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_rax_to_register_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_rax_to_register_size_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_register_to_local_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_register_to_local_size_table:
        .zero   NUMBER_OF_REGISTERS * POINTER_SIZE

        .align  16
jit_syntax_jump_table:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

        .align  16
symbol_table_values:
        .rept   MAX_NUMBER_OF_SYMBOLS
        .quad   VOID
        .endr

        .align  16
symbol_table_names:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

        .align  16
symbol_next_id:
        .quad   TAG_MASK + 1
execution_stack_top:
        .quad   0

        .align  16
read_datum_jump_table:
        .rept   CHAR_TABLE_SIZE
        .quad   read_symbol
        .endr
read_hash_jump_table:
        .zero   CHAR_TABLE_SIZE * POINTER_SIZE

        .align  16
object_space:
        .zero   stack_size
gc_mark_stack:
        .zero   stack_size
constant_pool:
        .zero   stack_size

        .align  16
jit_code_file_counter:
        .quad   0
jit_code_debug:
        .quad   0

        .align  16
jit_code_space:
        .quad   0
jit_code_space_next_address:
        .quad   0
jit_code_space_size:
        .quad   0

        .align  16
command_line_arguments:
        .quad   0
        .align  16
environment_alist:
        .quad   0

        .align  16
max_scheme_report_environment_symbol:
        .quad   0
max_null_environment_symbol:
        .quad   0

        .section .rodata
string_format:
        .string "%s"
token_format:
        .string "%a[^ \f\n\r\t\v()\";]"
char_format:
        .string "%c"
machine_readable_char_format:
        .string "#\\%c"
machine_readable_escape_code_format:
        .string "\\%c"
oct_format:
        .string "%o"
int_format:
        .string "%d"
hex_format:
        .string "%x"
double_format:
        .string "%f"
object_format:
        .string "#<%s>"
equals_sign:
        .string "="
read_mode:
        .string "r"
write_mode:
        .string "w"
jit_code_directory:
        .string "jit_code"
jit_code_file_format:
        .string "jit_code/jit_code_%06d.bin"

        .align  16
jit_prologue:
        push    %rbp
        mov     %rsp, %rbp
        sub     $0x11223344, %rsp
jit_prologue_size:
        .quad   (. - jit_prologue)

        .align  16
jit_epilogue:
        mov     %rbp, %rsp
        pop     %rbp
        ret
jit_epilogue_size:
        .quad   . - jit_epilogue

        .align  16
jit_tco_adjust_stack:
        mov     %rbp, %rsp
        pop     %rbp
jit_tco_adjust_stack_size:
        .quad   . - jit_tco_adjust_stack

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_literal_to_\reg\():
        mov     $0x1122334455667788, %\reg
jit_literal_to_\reg\()_size:
        .quad   (. - jit_literal_to_\reg - POINTER_SIZE)
        .endr

        .align  16
jit_conditional_rax_is_false_jump:
        mov     $FALSE, %r11
        cmp     %rax, %r11
        je      0
jit_conditional_rax_is_false_jump_size:
        .quad   (. - jit_conditional_rax_is_false_jump)

        .align  16
jit_unconditional_jump:
        jmp     0
jit_unconditional_jump_size:
        .quad   (. - jit_unconditional_jump)

        .align  16
jit_unconditional_known_jump:
        jmp     0
jit_unconditional_known_jump_size:
        .quad   (. - jit_unconditional_known_jump) - INT_SIZE

        .align  16
jit_unbox_rax:
        mov     $PAYLOAD_MASK, %r11
        and     %r11, %rax
jit_unbox_rax_size:
        .quad   . - jit_unbox_rax

        .align  16
jit_call_rax:
        call    *%rax
jit_call_rax_size:
        .quad   . - jit_call_rax

        .align  16
jit_jump_rax:
        jmp     *%rax
jit_jump_rax_size:
        .quad   (. - jit_jump_rax)

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_pop_\reg\():
        pop    %\reg
jit_pop_\reg\()_size:
        .quad   . - jit_pop_\reg
        .endr

        .align  16
jit_rax_to_rax:
jit_rax_to_rax_size:
        .quad   . - jit_rax_to_rax
        .irp reg, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_rax_to_\reg\():
        mov     %rax, %\reg
jit_rax_to_\reg\()_size:
        .quad   . - jit_rax_to_\reg
        .endr

        .align  16
jit_push_rax:
        push    %rax
jit_push_rax_size:
        .quad   . - jit_push_rax

        .align  16
jit_void_to_rax:
        mov     $VOID, %rax
jit_void_to_rax_size:
        .quad   . - jit_void_to_rax

        .align  16
jit_global_to_rax:
        mov     0x1122334455667788, %rax
jit_global_to_rax_size:
        .quad   (. - jit_global_to_rax) - POINTER_SIZE

        .align  16
jit_rax_to_global:
        mov     %rax, 0x1122334455667788
jit_rax_to_global_size:
        .quad   (. - jit_rax_to_global) - POINTER_SIZE

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_local_to_\reg\():
        mov     -0x11223344(%rbp), %\reg
jit_local_to_\reg\()_size:
        .quad   (. - jit_local_to_\reg) - INT_SIZE
        .endr

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_\reg\()_to_local:
        mov     %\reg, -0x11223344(%rbp)
jit_\reg\()_to_local_size:
        .quad   (. - jit_\reg\()_to_local) - INT_SIZE
        .endr

        .align  16
jit_rax_to_syntax:
        mov     %rax, 0x1122334455667788
jit_rax_to_syntax_size:
        .quad   (. - jit_rax_to_syntax) - POINTER_SIZE

        .align  16
jit_rax_to_closure:
        mov     %rax, -0x11223344(%rsp)
jit_rax_to_closure_size:
        .quad   (. - jit_rax_to_closure) - INT_SIZE

        .irp file, boot r5rs, r7rs, init
        .align  16
\file\()_scm:
        .incbin "\file\().scm"
        .byte   0
        .endr
