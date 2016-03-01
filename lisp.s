        .include "macros.s"

        .text
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
        .globl neg, plus, minus, multiply, divide
        .globl quotient, remainder, modulo
        .globl floor_, ceiling, truncate, round_, exp_, log_, sin_, cos_, tan_, asin_, acos_, atan_, sqrt_, expt
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

neg:                            # z1
        has_tag TAG_INT, %rdi, store=false
        je      neg_int
neg_double:
        mov     %rdi, %rax
        btc     $SIGN_BIT, %rax
        ret
neg_int:
        neg     %edi
        box_int_internal %edi
        ret

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

ceiling:                        # z
        math_library_unary_call ceil

truncate:                       # z
        math_library_unary_call trunc

round_:                         # z
        math_library_unary_call round

        .irp name, exp, log, sin, cos, tan, asin, acos, atan
\name\()_:                      # z
        math_library_unary_call \name
        .endr

sqrt_:                          # z
        math_library_unary_call sqrt, round=true

expt:                           # z1, z2
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
        cmovnz  %esi, %esi

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
        .globl is_boolean, not

is_boolean:                     # obj
        has_tag TAG_BOOLEAN, %rdi
        box_boolean_internal
        ret

not:                            # obj
        mov     $FALSE, %rax
        eq_internal %rdi, %rax
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
        call_fn allocate_memory, $pair_size
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

        test    %eax, %eax
        jz      1b
        add     $header_size, %rax
        mov     %r12, %r11
        add     $header_size, %r11
        call_fn strcmp, %r11, %rax
        jnz     1b
        jmp     3f

2:      movq    symbol_next_id, %rbx
        incq    symbol_next_id

        mov     %r12, symbol_table_names(,%rbx,POINTER_SIZE)

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
        call_fn allocate_memory, %rdi
        movw    $TAG_STRING, header_object_type(%rax)
        movl    %ebx, header_object_size(%rax)
        incl    header_object_size(%rax)

        movb    $NULL, header_size(%rax,%rbx)

1:      test    %ebx, %ebx
        jz      2f
        dec     %ebx
        movb    %r12b, header_size(%rax,%rbx,1)
        jmp     1b

2:      tag     TAG_STRING, %rax
        register_for_gc
        return

string_length:                  # string
        unbox_pointer_internal %rdi
        movl    header_object_size(%rax), %eax
        dec     %eax
        box_int_internal %eax
        ret

string_ref:                     # string, k
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        movsxb  header_size(%rax,%rsi), %eax
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
        mov     %edi, size(%rsp)
        mov     %edi, %ebx
        add     $header_size, %edi
        call_fn allocate_memory, %rdi
        movw    $TAG_VECTOR, header_object_type(%rax)
        movl    %ebx, header_object_size(%rax)

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
        prologue
        default_arg TAG_PORT, stdout, %rsi

        unbox_pointer_internal %rsi, %rbx
        mov     %edi, %edi
        call_fn fputc, %rdi, %rbx
        tag     TAG_CHAR, %rax
        return

        ## SRFI 6: Basic String Ports
        .globl open_input_string

open_input_string:              # string
        prologue
        unbox_pointer_internal %rdi
        movl    header_object_size(%rax), %esi
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

        .globl init_runtime, allocate_code, set, lookup_global_symbol, gc, gc_mark, gc_sweep, gc_has_mark, object_space_size
        .globl int_format, double_format, read_mode, box_int, box_string, unbox, to_string, true_c_string, false_c_string

init_runtime:                   # execution_stack_top
        prologue
        mov     %rdi, execution_stack_top

        call_fn init_pointer_stack, $object_space, $OBJECT_SPACE_INITIAL_SIZE
        call_fn init_pointer_stack, $gc_mark_stack, $OBJECT_SPACE_INITIAL_SIZE

        call_fn box_string, $procedure_c_string
        mov     %rax, procedure_string
        call_fn box_string, $port_c_string
        mov     %rax, port_string
        call_fn box_string, $true_c_string
        mov     %rax, true_string
        call_fn box_string, $false_c_string
        mov     %rax, false_string
        call_fn box_string, $read_error_c_string
        mov     %rax, read_error_string

        lea     char_table, %rbx
        call_fn box_string, $backspace_char
        store_pointer $'\b
        call_fn box_string, $tab_char
        store_pointer $'\t
        call_fn box_string, $newline_char
        store_pointer $'\n
        call_fn box_string, $return_char
        store_pointer $'\r
        call_fn box_string, $space_char
        store_pointer $'\ ,

        lea     escape_char_table, %rbx
        movb    $'b, 8(%rbx)
        movb    $'t, 9(%rbx)
        movb    $'n, 10(%rbx)
        movb    $'r, 13(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'', 39(%rbx)
        movb    $'\\, 92(%rbx)

        lea     unescape_char_table, %rbx
        movb    $8, 98(%rbx)
        movb    $9, 116(%rbx)
        movb    $10, 110(%rbx)
        movb    $13, 114(%rbx)
        movb    $'\", 34(%rbx)
        movb    $'', 39(%rbx)
        movb    $'\\, 92(%rbx)

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $double_to_string
        store_pointer $TAG_BOOLEAN, $boolean_to_string
        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_INT, $integer_to_string
        store_pointer $TAG_SYMBOL, $symbol_to_string
        store_pointer $TAG_PROCEDURE, $procedure_to_string
        store_pointer $TAG_PORT, $port_to_string
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

        lea     read_hash_jump_table, %rbx
        store_pointer $'t, $read_true
        store_pointer $'f, $read_false
        store_pointer $'\\, $read_character
        store_pointer $'(, $read_vector

        return

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

allocate_memory:                # size
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
        mov     header_object_mark(%rax), %ax
        eq_internal $C_TRUE, %ax
        box_boolean_internal
        ret

gc_mark_nop:                    # obj
        ret

gc_mark_string:                 # string
        unbox_pointer_internal %rdi
        movw    $C_TRUE, header_object_mark(%rax)
        ret

gc_mark_object:                 # pointer
        minimal_prologue
        mov     $NIL, %rax
        cmp     %rax, %rdi
        je      1f
        unbox_pointer_internal %rdi
        cmpw    $C_TRUE, header_object_mark(%rax)
        je      1f
        movw    $C_TRUE, header_object_mark(%rax)
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

gc_mark:
        minimal_prologue
        call_fn gc_mark_queue_registers
        call_fn gc_mark_queue_stack
        call_fn gc_mark_queue_global_variables

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

        cmpw    $C_TRUE, header_object_mark(%rax)
        je      2f

        call_fn free, %rax
        call_fn pop_pointer_from_stack, $object_space
        mov     stack_bottom + object_space, %r11
        mov     %rax, (%r11,%rbx)
        jmp     1b

2:      movw    $C_FALSE, header_object_mark(%rax)
        add     $POINTER_SIZE, %ebx
        jmp     1b
3:      return

gc:
        minimal_prologue
        call_fn gc_mark
        call_fn gc_sweep
        return

object_space_size:
        mov     stack_top_offset + object_space, %rax
        shr     $POINTER_SIZE_SHIFT, %rax
        box_int_internal
        ret

vector_to_string:               # vector
        prologue str, stream, size
        unbox_pointer_internal %rdi, %rbx

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'\#, stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

        xor     %r12d, %r12d
1:      test    %r12d, %r12d
        jz      2f
        cmp     header_object_size(%rbx), %r12d
        je      3f

        call_fn fputc, $' , stream(%rsp)

2:      mov     header_size(%rbx,%r12), %rax
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        add     $POINTER_SIZE, %r12
        jmp     1b

3:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), stream(%rsp)
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

        string_buffer_to_string str(%rsp), stream(%rsp)
        register_for_gc
        return

char_to_string:                 # char
        prologue str, size
        mov     %edi, %ebx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $char_format, %rbx
        string_buffer_to_string str(%rsp), %r12
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
        tag     TAG_STRING, %rax
        return
1:      open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $machine_readable_char_format, %rbx
        string_buffer_to_string str(%rsp), %r12
        register_for_gc
        return

integer_to_string:              # int, radix
        prologue str, size, format
        default_arg TAG_INT, $10, %rsi
        cmovnz  %esi, %esi

        mov     integer_to_string_format_table(,%rsi,8), %rax
        mov     %rax, format(%rsp)

        movsx   %edi, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, format(%rsp), %rbx
        string_buffer_to_string str(%rsp), %r12

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
        string_buffer_to_string str(%rsp), %r12
        register_for_gc
        return

boolean_to_string:              # boolean
        mov     true_string, %rax
        mov     false_string, %r11
        test    $C_TRUE, %rdi
        cmovz   %r11, %rax
        tag     TAG_STRING, %rax
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

        movq    $header_size, %r12
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

        string_buffer_to_string str(%rsp), stream(%rsp)
        register_for_gc
        return

port_to_string:                 # port
        tag     TAG_STRING, port_string
        ret

procedure_to_string:            # procedure
        tag     TAG_STRING, procedure_string
        ret

to_string:                      # value
        minimal_prologue
        tagged_jump to_string_jump_table
        return

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

box_boolean:                    # c-boolean
        and     $C_TRUE, %rdi
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
        string_buffer_to_string str(%rsp), %r12
        return

read_whitespace:                # c-stream
        prologue
        mov     %rdi, %rbx

1:      call_fn fgetc, %rbx
        mov     %rax, %r12
        call_fn isspace, %rax
        jnz     1b

        call_fn ungetc, %r12, %rbx
        return

read_datum:                     # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        read_byte_jump read_datum_jump_table, %rbx, %rax
        return

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
        perror
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

3:      string_buffer_to_string str(%rsp), %r12
        register_for_gc
        return

read_true:                      # c-stream
        mov     $TRUE, %rax
        ret

read_false:                     # c-stream
        mov     $FALSE, %rax
        ret

read_character:                 # c-stream
        prologue
        mov     %rdi, %rbx
        call_fn fgetc, %rbx
        tag     TAG_CHAR, %rax
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
        cmp     $'), %rax
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

2:      mov     %r12, %r11
        mov     %rax, %r12
        call_fn set_cdr, %r11, %r12
        jmp     1b

3:      call_fn read_datum, %rbx
        call_fn set_cdr, %r12, %rax

        call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     $'), %rax
        je      4f
        call_fn error, read_error_string

4:      return  head(%rsp)

call_with_current_continuation_escape: # return
        minimal_prologue
        movq    %rdi, %xmm0
        call_fn longjmp, $jump_buffer, $C_TRUE
        return

set:                            # variable, expression
        unbox_pointer_internal %rdi
        mov     %rsi, symbol_table_values(,%rax,POINTER_SIZE)
        mov     %rdi, %rax
        ret

lookup_global_symbol:           # symbol
        lookup_global_symbol_internal %edi
        ret

allocate_code:                  # code, size
        prologue code, size
        mov     %rdi, code(%rsp)
        mov     %rsi, size(%rsp)
        call_fn mmap, $NULL, $PAGE_SIZE, $(PROT_READ | PROT_WRITE), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
        perror
	mov     %rax, %rbx
        call_fn memcpy, %rbx, code(%rsp), size(%rsp)
        perror
        call_fn mprotect, %rbx, $PAGE_SIZE, $(PROT_READ | PROT_EXEC)
        perror  je
        return  %rbx

        .data
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
read_format:
        .string "%as"

read_error_c_string:
        .string "unexpected input\n"
read_error_string:
        .quad   0

read_mode:
        .string "r"
write_mode:
        .string "w"

backspace_char:
        .string "#\\backspace"
tab_char:
        .string "#\\tab"
newline_char:
        .string "#\\newline"
return_char:
        .string "#\\return"
space_char:
        .string "#\\space"

port_c_string:
        .string "#<port>"
port_string:
        .quad   0
procedure_c_string:
        .string "#<procedure>"
procedure_string:
        .quad   0
false_c_string:
        .string "#f"
false_string:
        .quad   0
true_c_string:
        .string "#t"
true_string:
        .quad   0

        .align  16
integer_to_string_format_table:
        .zero   16 * POINTER_SIZE

        .align  16
char_table:
        .zero   ((SPACE_CHAR & INT_MASK) + 1) * POINTER_SIZE

        .align  16
escape_char_table:
        .zero   CHAR_TABLE_SIZE
unescape_char_table:
        .zero   CHAR_TABLE_SIZE

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

        .align  16
jump_buffer:
        .zero   JMP_BUF_SIZE
