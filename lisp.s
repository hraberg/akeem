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
        mov     %rax, %r11
        has_tag TAG_INT, %rdi
        or      %r11, %rax
        box_boolean_internal
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
        unbox_pointer_internal %rdi, %rbx
        add     $header_size, %rbx

        mov     $10, %r11
        has_tag TAG_INT, %rsi
        cmovz   %r11, %rsi
        cmovnz  %esi, %esi

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
        ret

        ## 6.3.2. Pairs and lists
        .globl is_pair, cons, car, cdr, set_car, set_cdr, is_null, length

is_pair:                        # obj
        has_tag TAG_PAIR, %rdi
        mov     %rax, %r11
        mov     $NIL, %rax
        eq_internal %rax, %rdi
        btc     $0, %rax
        and     %r11, %rax
        box_boolean_internal
        ret

cons:                           # obj1, obj2
        prologue obj1, obj2
        mov     %rdi, obj1(%rsp)
        mov     %rsi, obj2(%rsp)
        call_fn aligned_alloc, $POINTER_SIZE, $pair_size
        perror
        movw    $TAG_PAIR, header_object_type(%rax)
        movl    $pair_size, header_object_size(%rax)
        mov     obj1(%rsp), %rdi
        mov     %rdi, pair_car(%rax)
        mov     obj2(%rsp), %rsi
        mov     %rsi, pair_cdr(%rax)
        tag     TAG_PAIR, %rax
        call_fn register_for_gc, %rax
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
        xor     %ecx, %ecx
        mov     $NIL, %rbx
1:      cmp     %rbx, %rax
        je      2f

        call_fn cdr, %rax
        inc     %rcx
        jmp     1b

2:      box_int_internal %ecx
        return

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
        prologue string
        unbox_pointer_internal %rdi
        mov     %rax, string(%rsp)
        mov     symbol_next_id, %rbx

1:      test    %rbx, %rbx
        jnz     2f

        dec     %rbx
        mov     symbol_table_names(,%rbx,POINTER_SIZE), %rax

        test    %eax, %eax
        jz      1b
        add     $header_size, %rax
        mov     string(%rsp), %r11
        add     $header_size, %r11
        call_fn strcmp, %r11, %rax
        jnz     1b
        jmp     3f

2:      movq    symbol_next_id, %rbx
        incq    symbol_next_id

        mov     string(%rsp), %rax
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
        prologue fill
        mov     %edi, %edi
        mov     %edi, %ebx
        add     $header_size, %edi
        inc     %edi
        mov     %rsi, fill(%rsp)
        call_fn malloc, %rdi
        perror
        movw    $TAG_STRING, header_object_type(%rax)
        movl    %ebx, header_object_size(%rax)
        incl    header_object_size(%rax)

        movb    $NULL, header_size(%rax,%rbx)
        mov     fill(%rsp), %ecx

1:      dec     %ebx
        movb    %cl, header_size(%rax,%rbx,1)
        test    %ebx, %ebx
        jnz     1b

        tag     TAG_STRING, %rax
        call_fn register_for_gc, %rax
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
        .globl is_vector, make_vector, vector_length, vector_ref, vector_set

is_vector:                      # obj
        has_tag TAG_VECTOR, %rdi
        box_boolean_internal %rax
        ret

make_vector:                    # k, fill
        prologue fill
        mov     %rsi, fill(%rsp)
        shl     $POINTER_SIZE_SHIFT, %edi
        mov     %edi, size(%rsp)
        mov     %edi, %ebx
        add     $header_size, %edi
        call_fn aligned_alloc, $POINTER_SIZE, %rdi
        perror
        movw    $TAG_VECTOR, header_object_type(%rax)
        movl    %ebx, header_object_size(%rax)

        mov     fill(%rsp), %rsi
1:      test    %ebx, %ebx
        jz      2f
        sub     $POINTER_SIZE, %ebx
        mov     %rsi, header_size(%rax,%rbx)
        jmp     1b

2:      tag     TAG_VECTOR, %rax
        call_fn register_for_gc, %rax
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

vector_to_string:                 # vector
        prologue idx, str, size, stream
        unbox_pointer_internal %rdi, %rbx

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'\#, stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

        movq    $0, idx(%rsp)
1:      mov     idx(%rsp), %rcx
        test    %ecx, %ecx
        jz      2f
        cmp     header_object_size(%rbx), %ecx
        je      3f

        call_fn fputc, $' , stream(%rsp)

2:      mov     idx(%rsp), %rcx

        mov     header_size(%rbx,%rcx), %rax
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        addq    $POINTER_SIZE, idx(%rsp)
        jmp     1b

3:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

        ## 6.4. Control features
        .globl is_procedure

is_procedure:                   # obj
        has_tag TAG_PROCEDURE, %rdi
        box_boolean_internal
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
        cmp     $0, %rax
        setg    %al
        box_boolean_internal
        ret

is_output_port:                 # obj
        unbox_pointer_internal %rdi
        call_fn __fwritable, %rax
        cmp     $0, %rax
        setg    %al
        box_boolean_internal
        ret

        ## 6.6.2. Input
        .globl read, read_char, peek_char, is_eof_object

read:                           # port
        prologue str
        mov     stdin, %r11
        has_tag TAG_PORT, %rdi
        cmovz   %r11, %rdi

        unbox_pointer_internal %rdi, %rdi
        lea     str(%rsp), %rdx
        call_fn fscanf, %rdi, $read_format, %rdx
        perror

        call_fn box_string, str(%rsp)
        call_fn register_for_gc, %rax
        mov     %rax, %rbx
        call_fn free, str(%rsp)

        unbox_pointer_internal %rbx
        add     $header_size, %rax
        mov     %rax, str(%rsp)

        call_fn string_to_number, %rbx
        mov     $FALSE, %r11
        cmp     %r11, %rax
        jne     1f

        add     $header_size, %rax
        call_fn strcmp, $false_c_string, str(%rsp)
        mov     $FALSE, %rax
        je      1f

        add     $header_size, %rax
        call_fn strcmp, $true_c_string, str(%rsp)
        mov     $TRUE, %rax
        je      1f

        mov     str(%rsp), %rax
        cmpb    $'\", (%rax)
        mov     %rbx, %rax
        je      1f

        call_fn string_to_symbol, %rbx
1:      return

read_char:                      # port
        minimal_prologue
        mov     stdin, %r11
        has_tag TAG_PORT, %rdi
        cmovz   %r11, %rdi

        unbox_pointer_internal %rdi
        call_fn fgetc, %rax
        tag     TAG_CHAR, %rax
        return

peek_char:                      # port
        prologue
        mov     stdin, %r11
        has_tag TAG_PORT, %rdi
        cmovz   %r11, %rdi

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
        mov     stdout, %r11
        has_tag TAG_PORT, %rsi
        cmovz   %r11, %rsi

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
        mov     %edi, %edi
        mov     stdout, %r11
        has_tag TAG_PORT, %rsi
        cmovz   %r11, %rsi

        unbox_pointer_internal %rsi, %rbx
        call_fn fputc, %rdi, %rbx
        tag     TAG_CHAR, %rax
        return

        ## Runtime

        .globl init_runtime, allocate_code, set, lookup_global_symbol
        .globl int_format, double_format, box_int, box_string, unbox, to_string

init_runtime:
        prologue

        movq    $OBJECT_SPACE_INITIAL_SIZE, object_space_size
        call_fn aligned_alloc, $POINTER_SIZE, object_space_size
        perror
        mov     %rax, object_space

        movq    $OBJECT_SPACE_INITIAL_SIZE, mark_stack_size
        call_fn aligned_alloc, $POINTER_SIZE, mark_stack_size
        perror
        mov     %rax, mark_stack

        call_fn box_string, $port_c_string
        mov     %rax, port_string
        call_fn box_string, $true_c_string
        mov     %rax, true_string
        call_fn box_string, $false_c_string
        mov     %rax, false_string

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

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $double_to_string
        store_pointer $TAG_BOOLEAN, $boolean_to_string
        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_INT, $integer_to_string
        store_pointer $TAG_SYMBOL, $symbol_to_string
        store_pointer $TAG_STRING, $string_to_string
        store_pointer $TAG_PAIR, $pair_to_string
        store_pointer $TAG_VECTOR, $vector_to_string
        store_pointer $TAG_PORT, $port_to_string

        lea     unbox_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $unbox_double
        store_pointer $TAG_BOOLEAN, $unbox_boolean
        store_pointer $TAG_CHAR, $unbox_char
        store_pointer $TAG_INT, $unbox_integer
        store_pointer $TAG_SYMBOL, $unbox_symbol
        store_pointer $TAG_STRING, $unbox_string
        store_pointer $TAG_PAIR, $unbox_pair
        store_pointer $TAG_VECTOR, $unbox_vector
        store_pointer $TAG_PORT, $unbox_port

        lea     integer_to_string_format_table, %rbx
        store_pointer $8, $oct_format
        store_pointer $10, $int_format
        store_pointer $16, $hex_format

        return

register_for_gc:                # ptr
        prologue old_object_space, old_object_space_size
        mov     %rdi, %rbx
        mov     object_space_top, %rcx
        cmp     object_space_size, %rcx
        jl      1f
        mov     object_space, %rax
        mov     %rax, old_object_space(%rsp)
        mov     %rcx, old_object_space_size(%rsp)
        shlq    object_space_size
        call_fn aligned_alloc, $POINTER_SIZE, object_space_size
        perror
        mov     %rax, object_space
        call_fn memcpy object_space, old_object_space(%rsp), old_object_space_size(%rsp)
        perror
1:      mov     object_space, %rax
        mov     object_space_top, %rcx
        mov     %rbx, (%rax,%rcx)
        add     $POINTER_SIZE, object_space_top
        return  %rbx

pair_to_string:                 # pair
        prologue pair, str, size, stream
        mov     %rdi, pair(%rsp)

        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)
        mov     $NIL, %rbx
1:      cmp     %rbx, pair(%rsp)
        je      2f

        call_fn car, pair(%rsp)
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        call_fn cdr, pair(%rsp)
        mov     %rax, pair(%rsp)
        cmp     %rbx, %rax
        je      2f

        call_fn fputc, $' , stream(%rsp)

        has_tag TAG_PAIR, pair(%rsp), store=false
        je      1b

        call_fn fputc, $'., stream(%rsp)
        call_fn fputc, $' , stream(%rsp)
        call_fn to_string, pair(%rsp)
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

2:      call_fn fputc, $'), stream(%rsp)

        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

char_to_string:                 # char
        prologue str, size, stream
        mov     %edi, %ebx
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        xor     %al, %al
        call_fn fprintf, stream(%rsp), $char_format, %rbx
        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

char_to_machine_readable_string: # char
        prologue str, size, stream
        mov     %edi, %ebx
        cmp     $(SPACE_CHAR & INT_MASK), %bx
        jg      1f
        mov     char_table(,%ebx,POINTER_SIZE), %rax
        test    %eax, %eax
        jz      1f
        tag     TAG_STRING, %rax
        return
1:      open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        xor     %al, %al
        call_fn fprintf, stream(%rsp), $machine_readable_char_format, %rbx
        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

integer_to_string:              # int, radix
        prologue str, size, stream, format

        mov     $10, %r11
        has_tag TAG_INT, %rsi
        cmovz   %r11, %rsi
        cmovnz  %esi, %esi
        mov     integer_to_string_format_table(,%rsi,8), %rax
        mov     %rax, format(%rsp)

        movsx   %edi, %rbx
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        xor     %al, %al
        call_fn fprintf, stream(%rsp), format(%rsp), %rbx
        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

double_to_string:               # double
        prologue str, size, stream
        movq   %rdi, %xmm0
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        mov    stream(%rsp), %rdi
        mov    $double_format, %rsi
        mov    $1, %al         # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        call   fprintf
        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
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
        prologue idx, str, size, stream

        unbox_pointer_internal %rdi, %rbx
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)

        call_fn fputc, $'\", stream(%rsp)
        test    %ebx, %ebx
        jz      4f

        movq    $header_size, idx(%rsp)
1:      mov     idx(%rsp), %rcx

        xor     %eax, %eax
        mov     (%rbx,%rcx), %al
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

3:      incq    idx(%rsp)
        jmp     1b

4:      call_fn fputc, $'\", stream(%rsp)

        string_buffer_to_string str(%rsp), stream(%rsp)
        call_fn register_for_gc, %rax
        return

port_to_string:                 # port
        tag     TAG_STRING, port_string
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
        prologue str, size, stream
        mov     %edi, %ebx
        open_string_buffer str(%rsp), size(%rsp), stream(%rsp)
        xor     %al, %al
        call_fn fprintf, stream(%rsp), $string_format, %rbx
        string_buffer_to_string str(%rsp), stream(%rsp)
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
        .zero   128

        .align  16
to_string_jump_table:
        .zero   TAG_MASK * POINTER_SIZE

        .align  16
unbox_jump_table:
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
object_space:
        .quad   0
object_space_top:
        .quad   0
object_space_size:
        .quad   0
mark_stack:
        .quad   0
mark_stack_top:
        .quad   0
mark_stack_size:
        .quad   0
