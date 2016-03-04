        .include "macros.s"

        .text

        ## R5RS

        ## 6. Standard procedures
        ## 6.1. Equivalence predicates
        .globl is_eq, is_eq_v

is_eq:                          # obj1, obj2
is_eqv:                         # obj1, obj2
        eq_internal %rdi, %rsi
        box_boolean_internal
        ret

        ## 6.2. Numbers
        ## 6.2.5. Numerical operations
        .globl is_number, is_integer, is_exact, is_inexact
        .globl equal, less_than, greater_than, less_than_or_equal, greater_than_or_equal
        .globl plus, minus, multiply, divide
        .globl quotient, remainder, modulo
        .globl floor_, ceiling_, truncate_, round_, exp_, log_, sin_, cos_, tan_, asin_, acos_, atan_, sqrt_, expt_
        .globl exact_to_inexact, inexact_to_exact

is_number:                      # obj
        is_double_internal %rdi
        jnz     1f
        has_tag TAG_INT, %rdi
1:      box_boolean_internal
        ret

is_integer:                     # obj
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
        integer_division
        box_int_internal %edx
        ret

modulo:                         # n1, n2
        integer_division
        test    %edx, %edx
        jz      1f
        xor     %esi, %edi
        jns     1f
        add     %esi, %edx
1:      box_int_internal %edx
        ret

floor_:                         # z
        math_library_unary_call floor

ceiling_:                       # z
        math_library_unary_call ceil

truncate_:                      # z
        math_library_unary_call trunc

round_:                         # z
        math_library_unary_call round

        .irp name, exp, log, sin, cos, tan, asin, acos, atan
\name\()_:                      # z
        math_library_unary_call \name
        .endr

sqrt_:                          # z
        math_library_unary_call sqrt, round=true

expt_:                          # z1, z2
        math_library_binary_call pow, round=true

exact_to_inexact:               # z
        cvtsi2sd %edi, %xmm0
        movq    %xmm0, %rax
        ret

inexact_to_exact:               # z
        movq     %rdi, %xmm0
        cvtsd2si %xmm0, %rax
        box_int_internal
        ret

        ## 6.2.6. Numerical input and output
        .globl number_to_string, string_to_number

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
        .globl not, is_boolean

not:                            # obj
        mov     $FALSE, %rax
        eq_internal %rdi, %rax
        box_boolean_internal
        ret

is_boolean:                     # obj
        has_tag TAG_BOOLEAN, %rdi
        box_boolean_internal
        ret

        ## 6.3.2. Pairs and lists
        .globl is_pair, cons, car, cdr, set_car, set_cdr, is_null, length, reverse

is_pair:                        # obj
        mov     $NIL, %rax
        cmp     %rax, %rdi
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
        mov     %rsi, %rax
        ret

set_cdr:                        # pair, obj
        unbox_pointer_internal %rdi
        mov     %rsi, pair_cdr(%rax)
        mov     %rsi, %rax
        ret

is_null:                        # obj
        mov     $NIL, %rax
        eq_internal %rax, %rdi
        box_boolean_internal
        ret

length:                         # list
        prologue
        mov     %rdi, %rax
        xor     %ebx, %ebx

        mov     $NIL, %r12
1:      cmp     %r12, %rax
        je      2f

        call_fn cdr, %rax
        inc     %rbx
        jmp     1b

2:      box_int_internal %ebx
        return

reverse:                        # list
        prologue
        mov     $NIL, %r12
        mov     %rdi, %rbx
1:      mov     $NIL, %r11
        cmp     %r11, %rbx
        je      2f

        calL_fn car, %rbx
        call_fn cons, %rax, %r12
        mov     %rax, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return  %r12

        ## 6.3.3. Symbols
        .globl is_symbol, symbol_to_string, string_to_symbol

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

2:      movq    symbol_next_id, %rbx
        incq    symbol_next_id

        tag     TAG_STRING, %r12
        mov     %rax, symbol_table_names(,%rbx,POINTER_SIZE)

3:      tag     TAG_SYMBOL, %rbx
        return

        ## 6.3.4. Characters
        .globl is_char, char_to_integer, integer_to_char

is_char:                        # obj
        has_tag TAG_CHAR, %rdi
        box_boolean_internal
        ret

char_to_integer:
        movsx   %di, %eax
        box_int_internal
        ret

integer_to_char:
        xor     %eax, %eax
        mov     %di, %ax
        tag     TAG_CHAR, %rax
        ret

        ## 6.3.5. Strings
        .globl is_string, make_string, string_length, string_ref, string_set

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
        box_int_internal %edx
        ret

        ## 6.3.6. Vectors
        .globl is_vector, make_vector, vector_length, vector_ref, vector_set, list_to_vector

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
        mov     %rdx, %rax
        ret

list_to_vector:                 # list
        prologue vec
        mov     %rdi, %r12
        call_fn length, %r12
        call_fn make_vector, %rax
        mov     %rax, vec(%rsp)

        xor     %ebx, %ebx
1:      mov     $NIL, %r11
        cmp     %r11, %r12
        je      2f

        call_fn car, %r12
        call_fn vector_set, vec(%rsp), %rbx, %rax
        call_fn cdr, %r12
        mov     %rax, %r12

        inc     %rbx
        jmp     1b

2:      return  vec(%rsp)

        ## 6.4. Control features
        .globl is_procedure, call_with_current_continuation

is_procedure:                   # obj
        has_tag TAG_PROCEDURE, %rdi
        box_boolean_internal
        ret

call_with_current_continuation: # proc
        prologue
        mov     %rdi, %rbx
        call_fn setjmp, $jump_buffer # https://www.gnu.org/software/libc/manual/html_mono/libc.html#System-V-contexts
        jnz 1f
        call_fn *%rbx, $call_with_current_continuation_escape
        return
1:      return  %xmm0

        ## 6.5. Eval
        .globl eval, scheme_report_environment, null_environment, interaction_environment

eval:                           # expression environment-specifier
        prologue max_global_symbol
        default_arg TAG_INT, $-1, %rsi

        mov     %esi, max_global_symbol(%rsp)

        call_fn jit_code, %rdi, $NIL
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
        .globl call_with_input_file, call_with_output_file
        .globl is_input_port, is_output_port, current_input_port, current_output_port
        .globl with_input_from_file, with_output_to_file
        .globl open_input_file, open_output_file, close_input_port, close_output_port

open_input_file:                # filename
        minimal_prologue
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $read_mode
        tag     TAG_PORT, %rax
        return

open_output_file:               # filename
        minimal_prologue
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $write_mode
        tag     TAG_PORT, %rax
        return

close_input_port:               # port
close_output_port:              # port
        minimal_prologue
        unbox_pointer_internal %rdi
        call_fn fclose, %rax
        tag     TAG_INT, %rax
        return

call_with_input_file:          # filename, proc
        call_with_file_template input

call_with_output_file:          # filename, proc
        call_with_file_template output

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
        unbox_pointer_internal %rdi
        call_fn __freadable, %rax
        cmp     $NULL, %rax
        setg    %al
        box_boolean_internal
        ret

is_output_port:                 # obj
        unbox_pointer_internal %rdi
        call_fn __fwritable, %rax
        cmp     $NULL, %rax
        setg    %al
        box_boolean_internal
        ret

        ## 6.6.2. Input
        .globl read, read_char, peek_char, is_eof_object

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
        tag     TAG_CHAR, %rax
        return

peek_char:                      # port
        prologue
        default_arg TAG_PORT, stdin, %rdi

        unbox_pointer_internal %rdi, %rbx
        call_fn fgetc, %rbx
        call_fn ungetc, %rax, %rbx
        tag     TAG_CHAR, %rax
        return

is_eof_object:                  # obj
        eq_internal $EOF, %edi
        box_boolean_internal
        ret

        ## 6.6.3. Output
        .globl write, display, newline, write_char
write:                          # obj, port
        prologue

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_CHAR, $char_to_machine_readable_string
        store_pointer $TAG_STRING, $string_to_machine_readable_string

        call_fn display, %rdi, %rsi

        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_STRING, $string_to_string

        return

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
        return  $NIL

newline:                        # port
        minimal_prologue
        call_fn write_char, $NEWLINE_CHAR, %rdi
        return

write_char:                     # char, port
        minimal_prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi, %rax
        mov     %edi, %edi
        call_fn fputc, %rdi, %rax
        tag     TAG_CHAR, %rax
        return

        ## 6.6.4. System interface
        .globl load

load:                           # filename
        prologue
        call_fn open_input_file, %rdi
        mov     %rax, %rbx
        call_fn read_all, %rax
        mov     %rax, %r12
        call_fn close_input_port, %rbx
        return  $TRUE

        ## Scheme Requests for Implementation

        ## SRFI 6: Basic String Ports
        .globl open_input_string

open_input_string:              # string
        minimal_prologue
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %esi
        dec     %esi
        add     $header_size, %rax
        call_fn fmemopen, %rax, %rsi, $read_mode
        tag     TAG_PORT, %rax
        return

        ## SRFI 23: Error reporting mechanism
        .globl error

error:                          # reason
        minimal_prologue
        tag     TAG_PORT, stderr
        call_fn display, %rdi, %rax
        call_fn exit, $1
        return

        ## Runtime

        .globl init_runtime

init_runtime:                   # execution_stack_top, jit_code_debug
        prologue
        mov     %rdi, execution_stack_top
        mov     %rsi, jit_code_debug

        call_fn init_pointer_stack, $object_space, $OBJECT_SPACE_INITIAL_SIZE
        call_fn init_pointer_stack, $gc_mark_stack, $OBJECT_SPACE_INITIAL_SIZE
        call_fn init_pointer_stack, $constant_pool, $OBJECT_SPACE_INITIAL_SIZE

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

        intern_symbol quote_symbol, "quote"
        intern_symbol quasiquote_symbol, "quasiquote"
        intern_symbol unquote_symbol, "unquote"
        intern_symbol unquote_splicing_symbol, "unquote-splicing"

        intern_symbol lambda_symbol, "lambda"
        intern_symbol if_symbol, "if"
        intern_symbol let_symbol, "let"
        intern_symbol define_symbol, "define"
        intern_symbol begin_symbol, "begin"
        intern_symbol set_symbol, "set!"

        mov     symbol_next_id, %rax
        mov     %rax, max_null_environment_symbol

        intern_string read_error_string, "unexpected input\n"
        intern_string false_string, "#f"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_FALSE
        intern_string true_string, "#t"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_TRUE

        intern_string backspace_char, "#\\backspace"
        intern_string tab_char, "#\\tab"
        intern_string newline_char, "#\\newline"
        intern_string return_char, "#\\return"
        intern_string space_char, "#\\space"

        lea     char_table, %rbx
        store_pointer $'\b, backspace_char
        store_pointer $'\t, tab_char
        store_pointer $'\n, newline_char
        store_pointer $'\r, return_char
        store_pointer $'\ , space_char

        lea     escape_char_table, %rbx
        movb    $'b, 8(%rbx)
        movb    $'t, 9(%rbx)
        movb    $'n, 10(%rbx)
        movb    $'r, 13(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'', 39(%rbx)
        movb    $'\\, 92(%rbx)

        lea     unescape_char_table, %rbx
        movb    $'\b, 98(%rbx)
        movb    $'\t, 116(%rbx)
        movb    $'\n, 110(%rbx)
        movb    $'\r, 114(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'', 39(%rbx)
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

        lea     gc_mark_queue_jump_table, %rbx
        store_pointer $TAG_PAIR, $gc_mark_queue_pair
        store_pointer $TAG_VECTOR, $gc_mark_queue_vector

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

        lea     jit_pop_argument_table, %rbx
        store_pointer $0, $jit_pop_rax
        store_pointer $1, $jit_pop_rdi
        store_pointer $2, $jit_pop_rsi
        store_pointer $3, $jit_pop_rdx
        store_pointer $4, $jit_pop_rcx
        store_pointer $5, $jit_pop_r8
        store_pointer $6, $jit_pop_r9

        lea     jit_pop_argument_size_table, %rbx
        store_pointer $0, jit_pop_rax_size
        store_pointer $1, jit_pop_rdi_size
        store_pointer $2, jit_pop_rsi_size
        store_pointer $3, jit_pop_rdx_size
        store_pointer $4, jit_pop_rcx_size
        store_pointer $5, jit_pop_r8_size
        store_pointer $6, jit_pop_r9_size

        lea     jit_parameter_to_rax_table, %rbx
        store_pointer $0, $jit_rdi_to_rax
        store_pointer $1, $jit_rsi_to_rax
        store_pointer $2, $jit_rdx_to_rax
        store_pointer $3, $jit_rcx_to_rax
        store_pointer $4, $jit_r8_to_rax
        store_pointer $5, $jit_r9_to_rax

        lea     jit_parameter_to_rax_size_table, %rbx
        store_pointer $0, jit_rdi_to_rax_size
        store_pointer $1, jit_rsi_to_rax_size
        store_pointer $2, jit_rdx_to_rax_size
        store_pointer $3, jit_rcx_to_rax_size
        store_pointer $4, jit_r8_to_rax_size
        store_pointer $5, jit_r9_to_rax_size

        lea     jit_syntax_jump_table, %rbx
        unbox_pointer_internal quote_symbol
        store_pointer %eax, $jit_quote
        unbox_pointer_internal if_symbol
        store_pointer %eax, $jit_if
        unbox_pointer_internal set_symbol
        store_pointer %eax, $jit_set
        unbox_pointer_internal define_symbol
        store_pointer %eax, $jit_define
        unbox_pointer_internal lambda_symbol
        store_pointer %eax, $jit_lambda
        unbox_pointer_internal begin_symbol
        store_pointer %eax, $jit_begin

        .irp name, eq, eqv, number, integer, exact, inexact
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
        define "boolean?", $is_boolean

        define "pair?", $is_pair
        define "cons", $cons
        define "car", $car
        define "cdr", $cdr
        define "set-car!", $set_car
        define "set-cdr!", $set_cdr

        define "null?", $is_null
        define "length", $length
        define "reverse", $reverse

        define "symbol?", $is_symbol
        define "symbol->string", $symbol_to_string
        define "string->symbol", $string_to_symbol

        define "char?", $is_char
        define "char->integer", $char_to_integer
        define "integer->char", $integer_to_char

        define "string?", $is_string
        define "make-string", $make_string
        define "string-length", $string_length
        define "string-ref", $string_ref
        define "string-set!", $string_set

        define "vector?", $is_vector
        define "make-vector", $make_vector
        define "vector-length", $vector_length
        define "vector-ref", $vector_ref
        define "vector-set!", $vector_set
        define "list->vector", $list_to_vector

        define "procedure?", $is_procedure
        define "call-with-current-continuation", $call_with_current_continuation

        define "eval", $eval
        define "scheme-report-environment", $scheme_report_environment
        define "null-environment", $null_environment
        define "interaction-environment", $interaction_environment

        define "call-with-input-file", $call_with_input_file
        define "call-with-output-file", $call_with_output_file

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
        define "newline", $newline
        define "write-char", $write_char

        define "load", $load

        call_fn box_string, $r5rs_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax

        mov     symbol_next_id, %rax
        mov     %rax, max_scheme_report_environment_symbol

        define "open-input-string", $open_input_string
        define "read-all", $read_all
        define "error", $error
        define "gc", $gc
        define "object-space-count", $object_space_size
        define "class-of", $class_of
        define "lookup-global-symbol", $lookup_global_symbol

        call_fn box_string, $core_scm
        call_fn open_input_string, %rax
        call_fn read_all, %rax
        return

        ## Public API
        .globl set, lookup_global_symbol, object_space_size, class_of
        .globl box_boolean, box_int, box_string, unbox

set:                            # variable, expression
        unbox_pointer_internal %rdi
        mov     %rsi, symbol_table_values(,%rax,POINTER_SIZE)
        mov     %rdi, %rax
        ret

read_all:                       # port
        prologue
        mov     %rdi, %rbx
1:      call_fn read, %rbx
        cmp     $EOF, %eax
        je      2f
        call_fn eval, %rax
        jmp     1b
        return  $TRUE

class_of:                       # obj
        extract_tag
        tag     TAG_SYMBOL, %rax
        ret

lookup_global_symbol:           # symbol
        lookup_global_symbol_internal %edi
        ret

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
        mov     %edi, %ebx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $string_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        return

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
        mov     $NIL, %rax
        cmp     %rax, %rdi
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

        ## Call/CC Helper
call_with_current_continuation_escape: # return
        minimal_prologue
        movq    %rdi, %xmm0
        call_fn longjmp, $jump_buffer, $C_TRUE
        return

        ## Stack ADT
init_pointer_stack:             # stack, size
        prologue
        mov     %rdi, %rbx
        movq    %rsi, stack_max_size(%rbx)
        call_fn malloc, %rsi
        perror
        mov     %rax, stack_bottom(%rbx)
        movq    $0, stack_top_offset(%rbx)
        return  %rbx

resize_pointer_stack:           # stack
        prologue
        mov     %rdi, %rbx
        shlq    stack_max_size(%rbx)
        call_fn realloc, stack_bottom(%rdi), stack_max_size(%rbx)
        perror
        mov     %rax, stack_bottom(%rbx)
        return  %rbx

push_pointer_on_stack:          # stack, ptr
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

pop_pointer_from_stack:         # stack
        sub     $POINTER_SIZE, stack_top_offset(%rdi)
        mov     stack_top_offset(%rdi), %rcx
        mov     stack_bottom(%rdi), %r11
        mov     (%r11,%rcx), %rax
        ret

        ## Garbage Collection
        .globl gc, gc_mark, gc_sweep, gc_has_mark

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

gc_has_mark:                    # pointer
        unbox_pointer_internal %rdi
        btw     $GC_MARK_BIT, header_object_mark(%rax)
        setc    %al
        and     $C_TRUE, %eax
        box_boolean_internal
        ret

gc_mark_nop:                    # obj
        ret

gc_mark_string:                 # string
        unbox_pointer_internal %rdi
        btsw    $GC_MARK_BIT, header_object_mark(%rax)
        ret

gc_mark_object:                 # pointer
        minimal_prologue
        mov     $NIL, %rax
        cmp     %rax, %rdi
        je      1f
        unbox_pointer_internal %rdi
        btsw    $GC_MARK_BIT, header_object_mark(%rax)
        jc      1f
        call_fn push_pointer_on_stack, $gc_mark_stack, %rdi
1:      return

gc_maybe_mark:                  # obj
        minimal_prologue
        tagged_jump gc_mark_jump_table
        return

gc_mark_queue_pair:             # pair
        prologue
        unbox_pointer_internal %rdi, %rbx
        call_fn gc_maybe_mark, pair_car(%rbx)
        call_fn gc_maybe_mark, pair_cdr(%rbx)
        return

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
1:      test    %ebx, %ebx
        jz      2f

        dec     %ebx
        cmpq    $NULL, symbol_table_names(,%ebx,POINTER_SIZE)
        je      1b

        mov     symbol_table_values(,%ebx,POINTER_SIZE), %rdi
        call_fn gc_maybe_mark, %rdi
        jmp     1b
2:      return

gc_mark_queue_constant_pool:
        prologue
        xor     %ebx, %ebx
1:      cmp     %rbx, stack_top_offset + constant_pool
        je      2f

        mov     stack_bottom + object_space, %rax
        mov     (%rax,%rbx), %r11
        call_fn gc_maybe_mark, %rdi

        add     $POINTER_SIZE, %ebx
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
        xor     %ebx, %ebx
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

2:      add     $POINTER_SIZE, %ebx
        jmp     1b
3:      return

gc:
        minimal_prologue
        call_fn gc_mark
        call_fn gc_sweep
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

pair_to_string:                 # pair
        prologue str, size, stream
        mov     %rdi, %r12

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)
        mov     $NIL, %rbx
1:      cmp     %rbx, %r12
        je      2f

        call_fn car, %r12
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        call_fn cdr, %r12
        mov     %rax, %r12
        cmp     %rbx, %r12
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
        cmp     $(SPACE_CHAR & INT_MASK), %bx
        jg      1f
        mov     char_table(,%ebx,POINTER_SIZE), %rax
        test    %eax, %eax
        jz      1f
        return
1:      open_string_buffer str(%rsp), size(%rsp), %r12
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
        prologue str, size
        call_fn class_of, %rdi
        call_fn to_string, %rax
        call_fn unbox_string, %rax
        mov     %rax, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12
        call_fn fprintf, %r12, $object_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
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
3:      return  $FALSE

read_datum:                     # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn read_comment, %rbx
        call_fn read_whitespace, %rbx
        cmp     $EOF, %eax
        je      1f
        call_fn read_comment, %rbx
        call_fn fgetc, %rbx
        read_byte_jump read_datum_jump_table, %rbx, %rax
1:      return

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
        prologue
        mov     %rdi, %rbx
        mov     %rsi, %rdi
        call_fn ungetc, %rdi, %rbx
        call_fn read_token, %rbx
        call_fn string_to_symbol, %rax
        return

read_number:                    # c-stream, c-char
        prologue
        mov     %rdi, %rbx
        mov     %rsi, %rdi
        call_fn ungetc, %rdi, %rbx
        call_fn read_token, %rbx
        call_fn string_to_number, %rax
        return

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
        prologue str, size
        mov     %rdi, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12

1:      call_fn fgetc, %rbx
        cmp     $'\", %rax
        je      3f

        cmp     $'\\, %rax
        jne     2f
        call_fn fgetc, %rbx
        mov     unescape_char_table(%eax), %al

2:      call_fn fputc, %rax, %r12
        jmp     1b

3:      string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
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
        mov     %rax, %r12
        call_fn string_length, %r12
        cmp     $1, %eax
        je      3f

        mov     $CHAR_TABLE_SIZE, %rbx
        unbox_pointer_internal %r12, %r12
1:      test    %ebx, %ebx
        jz      2f
        dec     %ebx
        mov     char_table(,%rbx,POINTER_SIZE), %rax
        unbox_pointer_internal %rax

        test    %eax, %eax
        jz      1b

        add     $header_size + CHAR_PREFIX_LENGTH, %rax
        lea     header_size(%r12), %r11
        call_fn strcmp, %r11, %rax
        jnz     1b

        tag     TAG_CHAR, %rbx
        return

2:      call_fn error, read_error_string
        return

3:      call_fn string_ref, %r12, $ZERO_INT
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

read_list:                      # c-stream
        prologue head
        mov     %rdi, %rbx

        mov     $NIL, %r12
        mov     %r12, head(%rsp)
1:      call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     $'), %al
        je      4f

        cmp     $'., %rax
        je      3f

        call_fn ungetc, %rax, %rbx

        call_fn read_datum, %rbx
        call_fn cons, %rax, $NIL
        mov     $NIL, %r11
        cmp     %r11, %r12
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
        call_fn mmap, $NULL, $PAGE_SIZE, $(PROT_READ | PROT_WRITE), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
        perror
	mov     %rax, %rbx
        call_fn memcpy, %rbx, code(%rsp), size(%rsp)
        perror
        call_fn mprotect, %rbx, $PAGE_SIZE, $(PROT_READ | PROT_EXEC)
        perror  je
        return  %rbx

jit_code:                       # form, environment
        prologue env, code, size
        mov     %rdi, %r12
        mov     %rsi, env(%rsp)
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %rbx

        call_fn jit_function, %r12, %rbx, env(%rsp)
        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        return

jit_function:                   # form, c-stream, environment
        prologue env, frame_size, local_idx, local
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn fwrite, $jit_prologue, $1, jit_prologue_size, %r12

        call_fn length, env(%rsp)
        mov     %eax, local_idx(%rsp)

        shl     $POINTER_SIZE_SHIFT, %eax
        add     $POINTER_SIZE, %eax
        and     $-(POINTER_SIZE * 2), %eax
        mov     %eax, frame_size(%rsp)
        lea     frame_size(%rsp), %rax
        call_fn fwrite, %rax $1, $INT_SIZE, %r12

1:      mov     local_idx(%rsp), %ecx
        test    %ecx, %ecx
        jz      2f
        dec     %ecx
        mov     %ecx, local_idx(%rsp)

        mov     jit_parameter_to_rax_table(,%rcx,POINTER_SIZE), %rax
        mov     jit_parameter_to_rax_size_table(,%rcx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

        mov     local_idx(%rsp), %ecx
        inc     %ecx
        shl     $POINTER_SIZE_SHIFT, %ecx
        neg     %ecx
        mov     %ecx, local(%rsp)
        call_fn fwrite, $jit_rax_to_local, $1, jit_rax_to_local_size, %r12
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12

        jmp     1b

2:      call_fn jit_datum, %rbx, %r12, env(%rsp)

        call_fn fwrite, $jit_epilogue, $1, jit_epilogue_size, %r12
        return

jit_datum:                      # form, c-stream, environment
        minimal_prologue
        tagged_jump jit_jump_table
        return

jit_procedure_call:             # form, c-stream, environment
        prologue len, env
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn length, %rbx
        mov     %rax, len(%rsp)

1:      mov     $NIL, %r11
        cmp     %r11, %rbx
        je      2f

        call_fn car, %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      mov     len(%rsp), %rbx
3:      test    %ebx, %ebx
        jz      4f
        dec     %ebx

        mov     jit_pop_argument_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_pop_argument_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        jmp     3b

4:      call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12
        return

jit_if:                         # form, c-stream, environment
        prologue if_offset, else_offset, end_offset, jump_offset, env
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp)
        call_fn fwrite, $jit_conditional_rax_is_false_jump, $1, jit_conditional_rax_is_false_jump_size, %r12

        call_fn ftell, %r12
        mov     %rax, if_offset(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp)
        call_fn fwrite, $jit_unconditional_jump, $1, jit_unconditional_jump_size, %r12

        patch_jump %r12, else_offset(%rsp), if_offset(%rsp), jump_offset(%rsp)

        call_fn cdr, %rbx
        mov     $NIL, %r11
        cmp     %rax, %r11
        jne     1f
        mov     $FALSE, %rax
        jmp     2f
1:      call_fn car, %rax
2:      call_fn jit_datum, %rax, %r12, env(%rsp)

        patch_jump %r12, end_offset(%rsp), else_offset(%rsp), jump_offset(%rsp)

        return

jit_begin:                     # form, c-stream, environment
        prologue env, form
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx

1:      mov     $NIL, %r11
        cmp     %r11, %rbx
        je      2f

        call_fn car, %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp)
        call_fn cdr, %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return

jit_lambda:                     # form, c-stream, environment
        prologue env, args
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn car, %rbx
        mov     %rax, env(%rsp)
        call_fn cdr, %rbx
        call_fn cons, begin_symbol, %rax
        call_fn jit_code, %rax, env(%rsp)

        tag     TAG_PROCEDURE, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp)

        return

jit_define:                     # form, c-stream, environment
        prologue env, form, variable_and_formals, variable, formals
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn cdr, %rbx
        mov     %rax, %rbx
        call_fn cdr, %rbx
        mov     %rax, form(%rsp)
        call_fn car, %rbx
        mov     %rax, variable_and_formals(%rsp)

        call_fn car, variable_and_formals(%rsp)
        mov     %rax, variable(%rsp)
        call_fn cdr, variable_and_formals(%rsp)
        mov     %rax, formals(%rsp)

        call_fn cons, formals(%rsp), form(%rsp)
        call_fn cons, lambda_symbol, %rax
        call_fn cons, %rax, $NIL

        call_fn cons, variable(%rsp), %rax
        call_fn cons, set_symbol, %rax

        call_fn jit_datum, %rax, %r12, env(%rsp)
        return

jit_set:                        # form, c-stream, environment
        prologue symbol_address, env
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        call_fn cdr, %rbx
        mov     %rax, %rbx

        call_fn car, %rbx
        lea     symbol_table_values(,%eax,POINTER_SIZE), %rax
        mov     %rax, symbol_address(%rsp)

        call_fn cdr, %rbx
        call_fn car, %rax
        call_fn jit_datum, %rax, %r12, env(%rsp)

        call_fn fwrite, $jit_rax_to_global, $1, jit_rax_to_global_size, %r12
        lea     symbol_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        return

jit_quote:                      # form, c-stream, environment
        prologue env
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        call_fn cdr, %rbx
        call_fn car, %rax
        call_fn jit_literal, %rax, %r12, env(%rsp)
        return

jit_pair:                       # form, c-stream, environment
        prologue env
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        call_fn car, %rbx
        mov     %rax, %r11
        has_tag TAG_SYMBOL, %r11, store=false
        jne     1f
        unbox_pointer_internal %r11
        mov     jit_syntax_jump_table(,%rax,8), %rax
        test    %eax, %eax
        jnz     2f

1:      call_fn jit_procedure_call, %rbx, %r12, env(%rsp)
        return

2:      call_fn *%rax, %rbx, %r12
        return

jit_symbol:                    # symbol, c-stream, environment
        prologue env, symbol_address, symbol, local
        mov     %rdi, symbol(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)

        xor     %ebx, %ebx
        mov     $NIL, %r11
1:      cmp     %r11, env(%rsp)
        je      2f

        call_fn car, env(%rsp)
        cmp     symbol(%rsp), %rax
        je      3f

        call_fn cdr, env(%rsp)
        mov     %rax, env(%rsp)
        inc     %rbx
        jmp     1b

2:      mov     symbol(%rsp), %rdi
        lea     symbol_table_values(,%edi,POINTER_SIZE), %rax
        mov     %rax, symbol_address(%rsp)

        call_fn fwrite, $jit_global_to_rax, $1, jit_global_to_rax_size, %r12
        lea     symbol_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        return

3:      inc     %ebx
        shl     $POINTER_SIZE_SHIFT, %ebx
        neg     %ebx
        mov     %ebx, local(%rsp)
        call_fn fwrite, $jit_local_to_rax, $1, jit_local_to_rax_size, %r12
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12
        return

jit_literal:                    # literal, c-stream, environment
        prologue literal
        mov     %rdi, literal(%rsp)
        mov     %rsi, %r12
        call_fn jit_maybe_add_to_constant_pool, %rdi
        call_fn fwrite, $jit_literal_to_rax, $1, jit_literal_to_rax_size, %r12
        lea     literal(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        return

jit_add_to_constant_pool_nop:   # obj
        ret

jit_add_to_constant_pool:       # obj
        minimal_prologue
        call_fn push_pointer_on_stack, $constant_pool, %rdi
        return

jit_maybe_add_to_constant_pool: # obj
        minimal_prologue
        tagged_jump jit_constant_pool_jump_table
        return

        .data
        .align  16
integer_to_string_format_table:
        .zero   16 * POINTER_SIZE

        .align  16
char_table:
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
        .zero   TAG_MASK * POINTER_SIZE

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
jit_syntax_jump_table:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

        .align  16
jit_pop_argument_table:
        .zero   16 * POINTER_SIZE

        .align  16
jit_pop_argument_size_table:
        .zero   16 * POINTER_SIZE

        .align  16
jit_parameter_to_rax_table:
        .zero   16 * POINTER_SIZE

        .align  16
jit_parameter_to_rax_size_table:
        .zero   16 * POINTER_SIZE

        .align  16
symbol_table_values:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

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
jump_buffer:
        .zero   JMP_BUF_SIZE

        .align  16
jit_code_file_counter:
        .quad   0
jit_code_debug:
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
read_mode:
        .string "r"
write_mode:
        .string "w"
jit_code_file_format:
        .string "jit_code_%04d.bin"

        ## register numbers:
        ## rax = r0, rcx = r1, rdx = r2, rbx = r3,
        ## rsp = r4, rbp = r5, rsi = r6, rdi = r7

        .align  16
jit_prologue:
        push    %rbp
        mov     %rsp, %rbp
        sub     $0x11223344, %rsp
jit_prologue_size:
        .quad   (. - jit_prologue) - INT_SIZE

        .align  16
jit_epilogue:
        mov     %rbp, %rsp
        pop     %rbp
        ret
jit_epilogue_size:
        .quad   . - jit_epilogue

        .align  16
jit_literal_to_rax:
        mov     $0x1122334455667788, %rax
jit_literal_to_rax_size:
        .quad   (. - jit_literal_to_rax) - POINTER_SIZE

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
jit_call_rax:
        mov     $PAYLOAD_MASK, %r11
        and     %r11, %rax
        call    *%rax
jit_call_rax_size:
        .quad   . - jit_call_rax

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_pop_\reg\():
        pop    %\reg
jit_pop_\reg\()_size:
        .quad   . - jit_pop_\reg\()
        .endr

        .align  16
        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_push_\reg\():
        push    %\reg
jit_push_\reg\()_size:
        .quad   . - jit_push_\reg
        .endr

        .align  16
        .irp reg, rdi, rsi, rdx, rcx, r8, r9
        .align  16
jit_\reg\()_to_rax:
        mov    %\reg, %rax
jit_\reg\()_to_rax_size:
        .quad   . - jit_\reg\()_to_rax
        .endr

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

        .align  16
jit_local_to_rax:
        mov     -0x11223344(%rbp), %rax
jit_local_to_rax_size:
        .quad   (. - jit_local_to_rax) - INT_SIZE

        .align  16
jit_rax_to_local:
        mov     %rax, -0x11223344(%rbp)
jit_rax_to_local_size:
        .quad   (. - jit_rax_to_local) - INT_SIZE

        .align  16
r5rs_scm:
        .incbin "r5rs.scm"
        .byte   0

core_scm:
        .incbin "core.scm"
        .byte   0
