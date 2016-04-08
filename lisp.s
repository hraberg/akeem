        .include "macros.s"

        .text

        ## R7RS

        ## 5.5. Record-type definitions

make_record:                    # k, type
        prologue
        arity_check 2
        assert_tag TAG_PAIR, %rdi, not_a_pair_string
        assert_tag TAG_SYMBOL, %rsi, not_a_symbol_string
        mov     %rsi, %rbx
        call_scm list_to_vector, %rdi
        unbox_pointer_internal %rax
        mov     %bx, header_object_type(%rax)
        tag     TAG_OBJECT, %rax
        return

record_ref:                     # record, k
        arity_check 2
        assert_tag TAG_OBJECT, %rdi, not_a_record_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     header_size(%rax,%rsi,POINTER_SIZE), %rax
        ret

record_set:                     # record, k, obj
        arity_check 3
        assert_tag TAG_OBJECT, %rdi, not_a_record_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     %rdx, header_size(%rax,%rsi,POINTER_SIZE)
        mov     $VOID, %rax
        ret

        ## 6. Standard procedures
        ## 6.1. Equivalence predicates

is_eq:                          # obj1, obj2
is_eqv:                         # obj1, obj2
        arity_check 2
        eq_internal %rdi, %rsi
        box_boolean_internal
        ret

        ## 6.2. Numbers
        ## 6.2.6. Numerical operations

is_number:                      # obj
is_complex:                     # obj
is_real:                        # obj
is_rational:                    # obj
        arity_check 1
        is_double_internal %rdi
        je      1f
        has_tag TAG_INT, %rdi
1:      box_boolean_internal
        ret

is_integer:                     # obj
        arity_check 1
        movq    %rdi, %xmm0
        has_tag TAG_INT, %rdi, store=false
        jne     1f
        mov     $TRUE, %rax
        ret
1:      is_double_internal %rdi
        jne     2f
        maybe_round_to_int
        has_tag TAG_INT, %rax
        box_boolean_internal
        ret
2:      mov     $FALSE, %rax
        ret

is_exact:                       # z
        arity_check 1
        has_tag TAG_INT, %rdi
        box_boolean_internal
        ret

is_inexact:                     # z
        arity_check 1
        is_double_internal %rdi
        box_boolean_internal
        ret

is_infinite:                    # z
        minimal_prologue
        arity_check 1
        is_double_internal %rdi
        jne     1f
        movq    %rdi, %xmm0
        call    isinf
        setnz   %al
        box_boolean_internal
        return
1:      return  $FALSE

is_nan:                         # z
        minimal_prologue
        arity_check 1
        is_double_internal %rdi
        jne     1f
        movq    %rdi, %xmm0
        call    isnan
        setnz   %al
        box_boolean_internal
        return
1:      return  $FALSE

equal:                          # z1, z2
        binary_comparsion equals, sete, sete
equal_size:
        .quad   . - equal - RET_SIZE

less_than:                      # z1, z2
        binary_comparsion less_than, setb, setl
less_than_size:
        .quad   . - less_than - RET_SIZE

greater_than:                   # z1, z2
        binary_comparsion greater_than, seta, setg
greater_than_size:
        .quad   . - greater_than - RET_SIZE

less_than_or_equal:             # z1, z2
        binary_comparsion less_than_or_equals, setbe, setle
less_than_or_equal_size:
        .quad   . - less_than_or_equal - RET_SIZE

greater_than_or_equal:          # z1, z2
        binary_comparsion greater_than_or_equals, setae, setge
greater_than_or_equal_size:
        .quad   . - greater_than_or_equal - RET_SIZE

plus:                           # z1, z2
        binary_op plus, addsd, add
plus_size:
        .quad   . - plus - RET_SIZE

minus:                          # z1, z2
        binary_op minus, subsd, sub
minus_size:
        .quad   . - minus - RET_SIZE

multiply:                       # z1, z2
        binary_op multiply, mulsd, imul
multiply_size:
        .quad   . - multiply - RET_SIZE

divide:                         # z1, z2
        arity_check 2
        binary_op_jump divide
divide_int_int:
        cvtsi2sd %edi, %xmm0
        cvtsi2sd %esi, %xmm1
        divsd   %xmm1, %xmm0
        maybe_round_to_int
        jmp     divide_return
divide_op:
        divsd   %xmm1, %xmm0
        movq    %xmm0, %rax
divide_return:
        ret
divide_size:
        .quad   . - divide - RET_SIZE

quotient:                       # n1, n2
        arity_check 2
        integer_division
        box_int_internal
        ret

remainder:                      # n1, n2
        prologue
        arity_check 2
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
        arity_check 2
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

atan2_:                         # y, x
        math_library_binary_call atan2

sqrt_:                          # z
        math_library_unary_call sqrt, round=true

expt_:                          # z1, z2
        math_library_binary_call pow, round=true

inexact:                        # z
        arity_check 1
        has_tag TAG_INT, %rdi, store=false
        jne     1f
        cvtsi2sd %edi, %xmm0
        movq    %xmm0, %rax
        ret
1:      mov     %rdi, %rax
        ret

exact:                          # z
        arity_check 1
        has_tag TAG_INT, %rdi, store=false
        je      1f
        movq    %rdi, %xmm0
        roundsd $ROUNDING_MODE_TRUNCATE, %xmm0, %xmm0
        cvtsd2si %xmm0, %rax
        box_int_internal
        ret
1:      mov     %rdi, %rax
        ret

        ## 6.2.7. Numerical input and output

number_to_string:               # z, radix
        minimal_prologue
        optional_arg 2, $DECIMAL_RADIX_INT, %rsi
        assert_tag TAG_INT, %rsi, not_an_integer_string
        is_double_internal %rdi
        je      1f
        assert_tag TAG_INT, %rdi, not_a_number_string
        call_fn integer_to_string_internal %rdi, %rsi
        return
1:      call_fn double_to_string, %rdi, %rsi
        return

string_to_number:               # string, radix
        prologue tail
        optional_arg 2, $DECIMAL_RADIX_INT, %rsi
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
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

        ## 6.3. Booleans

not:                            # obj
        arity_check 1
        mov     $FALSE, %rax
        eq_internal %rax, %rdi
        box_boolean_internal
        ret

        ## 6.4. Pairs and lists

is_pair:                        # obj
        arity_check 1
        is_nil_internal %rdi
        jne     1f
        mov     $FALSE, %rax
        ret
1:      has_tag TAG_PAIR, %rdi
        box_boolean_internal
        ret

cons:                           # obj1, obj2
        prologue
        arity_check 2
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
        minimal_prologue
        arity_check 1
        assert_pair %rdi
        car     %rdi
        return
car_size:
        .quad   . - car - RET_SIZE

cdr:                            # pair
        minimal_prologue
        arity_check 1
        assert_pair %rdi
        cdr     %rdi
        return
cdr_size:
        .quad   . - cdr - RET_SIZE

set_car:                        # pair, obj
        minimal_prologue
        arity_check 2
        assert_pair %rdi
        unbox_pointer_internal %rdi
        mov     %rsi, pair_car(%rax)
        mov     $VOID, %rax
        return

set_cdr:                        # pair, obj
        minimal_prologue
        arity_check 2
        assert_pair %rdi
        unbox_pointer_internal %rdi
        mov     %rsi, pair_cdr(%rax)
        mov     $VOID, %rax
        return

is_null:                        # obj
        arity_check 1
        is_nil_internal %rdi, store=true
        box_boolean_internal
        ret

length:                         # list
        prologue
        arity_check 1
        mov     %rdi, %rax
        xor     %ebx, %ebx

1:      is_nil_internal %rax
        je      2f

        cdr     %rax
        inc     %rbx
        jmp     1b

2:      box_int_internal %ebx
        return

append:                        # list1, list2
        prologue
        arity_check 2
        mov     %rsi, %r12
        call_scm reverse, %rdi
        mov     %rax, %rbx
1:      is_nil_internal %rbx
        je      2f

        car     %rbx, %rdi
        call_scm cons, %rdi, %r12
        mov     %rax, %r12

        cdr     %rbx, %rbx
        jmp     1b

2:      return  %r12

reverse:                        # list
        prologue
        arity_check 1
        mov     %rdi, %rbx
        mov     $NIL, %r12
1:      is_nil_internal %rbx
        je      2f

        car     %rbx, %rdi
        call_scm cons, %rdi, %r12
        mov     %rax, %r12

        cdr     %rbx, %rbx
        jmp     1b

2:      return  %r12

        ## 6.5. Symbols

is_symbol:                      # obj
        arity_check 1
        has_tag TAG_SYMBOL, %rdi
        box_boolean_internal
        ret

symbol_to_string:               # symbol
        arity_check 1
        assert_tag TAG_SYMBOL, %rdi, not_a_symbol_string
        call_fn symbol_to_string_internal
        ret

string_to_symbol:               # string
        prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        unbox_pointer_internal %rdi, %r12
        mov     symbol_next_id, %rbx

1:      test    %rbx, %rbx
        jz      2f

        dec     %rbx
        mov     symbol_table_names(,%rbx,POINTER_SIZE), %rax
        unbox_pointer_internal %rax

        test    %rax, %rax
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

        ## 6.6. Characters

is_char:                        # obj
        arity_check 1
        is_eof_object_internal %rdi, store=false
        je      1f
        has_tag TAG_CHAR, %rdi
        box_boolean_internal
        ret
1:      mov     $FALSE, %rax
        ret

char_to_integer:                # char
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %eax
        box_int_internal
        ret

integer_to_char:                # n
        arity_check 1
        assert_tag TAG_INT, %rdi, not_an_integer_string
        xor     %eax, %eax
        mov     %di, %ax
        tag     TAG_CHAR, %rax
        ret

is_char_alphabetic:             # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn isalpha, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_numeric:                # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn isdigit, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_whitespace:             # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn isspace, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_upper_case:             # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn isupper, %rdi
        setnz   %al
        box_boolean_internal
        return

is_char_lower_case:             # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn islower, %rdi
        setnz   %al
        box_boolean_internal
        return

char_upcase:                    # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn toupper, %rdi
        tag     TAG_CHAR, %rax
        return

char_downcase:                  # char
        minimal_prologue
        arity_check 1
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        mov    %edi, %edi
        call_fn tolower, %rdi
        tag     TAG_CHAR, %rax
        return

        ## 6.7. Strings

is_string:                      # obj
        arity_check 1
        has_tag TAG_STRING, %rdi
        box_boolean_internal
        ret

make_string:                    # k, fill
        prologue
        optional_arg 2, $SPACE_CHAR, %rsi
        assert_tag TAG_INT, %rdi, not_an_integer_string
        assert_tag TAG_CHAR, %rsi, not_a_character_string
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
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        dec     %eax
        box_int_internal %eax
        ret
string_length_size:
        .quad   . - string_length - RET_SIZE

string_ref:                     # string, k
        arity_check 2
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        movzxb  header_size(%rax,%rsi), %eax
        tag     TAG_CHAR, %rax
        ret
string_ref_size:
        .quad   . - string_ref - RET_SIZE

string_set:                     # string, k, char
        arity_check 3
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        assert_tag TAG_CHAR, %rdx, not_a_character_string
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        mov     %dl, header_size(%rax,%rsi)
        mov     $VOID, %rax
        ret
string_set_size:
        .quad   . - string_set - RET_SIZE

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

        ## 6.8. Vectors

is_vector:                      # obj
        arity_check 1
        has_tag TAG_VECTOR, %rdi
        box_boolean_internal %rax
        ret

make_vector:                    # k, fill
        prologue
        optional_arg 2, $VOID, %rsi
        assert_tag TAG_INT, %rdi, not_an_integer_string
        mov     %rsi, %r12
        shl     $POINTER_SIZE_SHIFT, %edi
        mov     %edi, %ebx
        add     $header_size, %edi
        call_fn gc_allocate_memory, %rdi
        movw    $TAG_VECTOR, header_object_type(%rax)
        mov     %ebx, header_object_size(%rax)

2:      test    %ebx, %ebx
        jz      3f
        sub     $POINTER_SIZE, %ebx
        mov     %r12, header_size(%rax,%rbx)
        jmp     2b

3:      tag     TAG_VECTOR, %rax
        register_for_gc
        return

vector_length:                  # vector
        arity_check 1
        assert_tag TAG_VECTOR, %rdi, not_a_vector_string
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        shr     $POINTER_SIZE_SHIFT, %eax
        box_int_internal
        ret
vector_length_size:
        .quad   . - vector_length - RET_SIZE

vector_ref:                     # vector, k
        arity_check 2
        assert_tag TAG_VECTOR, %rdi, not_a_vector_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     header_size(%rax,%rsi,POINTER_SIZE), %rax
        ret
vector_ref_size:
        .quad   . - vector_ref - RET_SIZE

vector_set:                     # vector, k, obj
        arity_check 3
        assert_tag TAG_VECTOR, %rdi, not_a_vector_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     %rdx, header_size(%rax,%rsi,POINTER_SIZE)
        mov     $VOID, %rax
        ret
vector_set_size:
        .quad   . - vector_set - RET_SIZE

list_to_vector:                 # list
        prologue vec
        arity_check 1
        mov     %rdi, %r12
        call_scm length, %rdi
        mov     %rax, %rdi
        call_scm make_vector, %rdi, $VOID
        mov     %rax, vec(%rsp)

        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      2f

        box_int_internal %ebx
        mov     %rax, %rsi
        car     %r12, %rdx
        call_scm vector_set, vec(%rsp), %rsi, %rdx
        cdr     %r12, %r12

        inc     %rbx
        jmp     1b

2:      return  vec(%rsp)

        ## 6.9. Bytevectors

is_bytevector:                  # obj
        minimal_prologue
        arity_check 1
        call_scm class_of, %rdi
        eq_internal $TAG_BYTEVECTOR, %eax
        box_boolean_internal %rax
        return

make_bytevector:                # k, byte
        prologue
        optional_arg 2, $ZERO_INT, %rsi
        assert_tag TAG_INT, %rdi, not_an_integer_string
        assert_tag TAG_INT, %rsi, not_an_integer_string

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
        arity_check 1
        assert_object %rdi, TAG_BYTEVECTOR, not_a_bytevector_string
        unbox_pointer_internal %rdi
        mov     header_object_size(%rax), %eax
        box_int_internal
        ret
bytevector_length_size:
        .quad   . - bytevector_length - RET_SIZE

bytevector_u8_ref:              # bytevector, k
        arity_check 2
        assert_object %rdi, TAG_BYTEVECTOR, not_a_bytevector_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        xor     %r11d, %r11d
        mov     header_size(%rax,%rsi), %r11b
        tag     TAG_INT, %r11
        ret
bytevector_u8_ref_size:
        .quad   . - bytevector_u8_ref - RET_SIZE

bytevector_u8_set:              # bytevector, k, byte
        arity_check 3
        assert_object %rdi, TAG_BYTEVECTOR, not_a_bytevector_string
        assert_tag TAG_INT, %rsi, not_an_integer_string
        assert_tag TAG_INT, %rdx, not_an_integer_string
        unbox_pointer_internal %rdi
        mov     %esi, %esi
        mov     %dl, header_size(%rax,%rsi)
        mov     $VOID, %rax
        ret
bytevector_u8_set_size:
        .quad   . - bytevector_u8_set - RET_SIZE

list_to_bytevector:             # list
        prologue vec
        arity_check 1
        mov     %rdi, %r12
        call_scm length, %rdi
        call_scm make_bytevector, %rax, $ZERO_INT
        mov     %rax, vec(%rsp)

        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      2f

        box_int_internal %ebx
        car     %r12, %rdx
        call_scm bytevector_u8_set, vec(%rsp), %rax, %rdx
        cdr     %r12, %r12

        inc     %rbx
        jmp     1b

2:      return  vec(%rsp)

        ## 6.10. Control features

is_procedure:                   # obj
        arity_check 1
        has_tag TAG_PROCEDURE, %rdi
        box_boolean_internal
        ret

apply:                          # proc, args
        prologue
        arity_check 2
        assert_tag TAG_PROCEDURE, %rdi, not_a_procedure_string
        assert_tag TAG_PAIR, %rsi, not_a_pair_string
        unbox_pointer_internal %rdi, %rbx
        call_scm reverse, %rsi
        mov     %rax, %r12
        call_scm length, %r12
        mov     %eax, %r10d

1:      is_nil_internal %r12
        je      2f
        car     %r12
        push    %rax
        cdr     %r12, %r12
        jmp     1b

2:      cmp     $1, %r10d
        jl      3f
        pop     %rdi
        cmp     $2, %r10d
        jl      3f
        pop     %rsi
        cmp     $3, %r10d
        jl      3f
        pop     %rdx
        cmp     $4, %r10d
        jl      3f
        pop     %rcx
        cmp     $5, %r10d
        jl      3f
        pop     %r8
        cmp     $6, %r10d
        jl      3f
        pop     %r9

3:      mov     %r10d, %eax
        mov     %r10d, %r12d
        call    *%rbx

        sub     $MAX_REGISTER_ARGS, %r12d
        js      4f

        shl     $POINTER_SIZE_SHIFT, %r12d
        add     %r12, %rsp

4:      return

call_with_current_continuation: # proc
        prologue stack_depth, continuation, dynamic_extent, rbx, r12, rbp
        mov     %rbx, rbx(%rsp)
        mov     %r12, r12(%rsp)
        mov     %rbp, rbp(%rsp)
        arity_check 1
        assert_tag TAG_PROCEDURE, %rdi, not_a_procedure_string
        unbox_pointer_internal %rdi, %rbx

        mov     execution_stack_top, %rax
        mov     %rsp, %r11
        add     $stack_frame_size, %r11
        sub     %r11, %rax
        mov     %eax, stack_depth(%rsp)
        shr     $POINTER_SIZE_SHIFT, %eax
        add     $CONTINUATION_SAVED_VALUES, %eax
        box_int_internal %eax
        call_scm make_vector, %rax, $VOID
        unbox_pointer_internal
        movw    $TAG_CONTINUATION, header_object_type(%rax)
        lea     header_size(%rax), %r12
        tag     TAG_OBJECT, %rax
        mov     %rax, continuation(%rsp)
        call_fn jit_maybe_add_to_constant_pool, %rax

        parameter_value dynamic_extent_stack_symbol
        call_scm reverse, %rax
        mov     %rax, dynamic_extent(%rsp)

        .irp saved_value, dynamic_extent, rbx, r12, rbp
        mov     \saved_value(%rsp), %rax
        mov     %rax, (%r12)
        add     $POINTER_SIZE, %r12
        .endr

        mov     execution_stack_top, %r11
        mov     stack_depth(%rsp), %eax
        sub     %rax, %r11
        call_fn memcpy, %r12, %r11, %rax
        perror

        call_fn jit_call_with_current_continuation_escape_factory, continuation(%rsp)
        tag     TAG_PROCEDURE, %rax
        mov     %rax, %r11
        call_scm *%rbx, %r11
        return

values:                         # obj ...
        arity_check 1, jge
        mov     $1, %r10d
        push    %rbp
        mov     %rsp, %rbp
        call_fn jit_rt_lambda_collect_varargs
        pop     %rbp
        push    %rdi

        call_scm make_vector, $ONE_INT, %rsi
        unbox_pointer_internal
        movw    $TAG_VALUES, header_object_type(%rax)
        tag     TAG_OBJECT, %rax
        mov     %rax, %rdx
        pop     %rax
        ret

call_with_values:               # producer, consumer
        prologue vals
        arity_check 2
        assert_tag TAG_PROCEDURE, %rdi, not_a_procedure_string
        assert_tag TAG_PROCEDURE, %rsi, not_a_procedure_string

        unbox_pointer_internal %rdi, %rbx
        mov     %rsi, %r12

        xor     %edx, %edx
        call_scm *%rbx
        mov     %rax, %rbx
        mov     %rdx, vals(%rsp)

        call_scm class_of, %rdx
        cmp     $TAG_VALUES, %eax
        je      1f
        mov     $NIL, %rax
        jmp     2f
1:      call_scm record_ref, vals(%rsp), $ZERO_INT
2:      call_scm cons, %rbx, %rax
        call_scm apply, %r12, %rax
        return

        ## 6.11. Exceptions

raise:                          # error
        prologue
        arity_check 1
        mov     %rdi, %rbx

        parameter_value exception_handler_stack_symbol
        is_nil_internal %rax
        je      1f

        car     %rax
        unbox_pointer_internal %rax, %r11
        call_scm *%r11, %rbx

1:      call_fn exit, $1
        return

        ## 6.12. Environments and evaluation

eval:                           # expression, environment-specifier
        prologue max_global_symbol
        arity_check 2

        mov     %esi, max_global_symbol(%rsp)

        call_fn jit_code, %rdi, $NIL, $NIL
        mov     %rax, %r11
        xor     %eax, %eax
        xor     %edx, %edx
        call    *%r11
        return

scheme_report_environment:      # version
        arity_check 0
        box_int_internal max_scheme_report_environment_symbol
        ret

null_environment:               # version
        arity_check 0
        box_int_internal max_null_environment_symbol
        ret

interaction_environment:
        arity_check 0
        box_int_internal $MAX_NUMBER_OF_SYMBOLS
        ret

        ## 6.13. Input and output
        ## 6.13.1. Ports

call_with_port:                 # port, proc
        prologue
        arity_check 2
        assert_tag TAG_PORT, %rdi, not_a_port_string
        assert_tag TAG_PROCEDURE, %rsi, not_a_procedure_string
        unbox_pointer_internal %rsi, %rbx
        mov     %rdi, %r12
        call_scm *%rbx, %r12
        mov     %rax, %rbx
        call_scm close_port, %r12
        return  %rbx

is_input_port:                  # obj
        minimal_prologue
        arity_check 1
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
        arity_check 1
        has_tag TAG_PORT, %rdi, store=false
        jne     1f
        unbox_pointer_internal %rdi
        call_fn __fwritable, %rax
        cmp     $NULL, %rax
        setg    %al
        box_boolean_internal
        return
1:      return $FALSE

open_input_file:                # filename
        minimal_prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $read_mode
        perror
        tag     TAG_PORT, %rax
        return

open_output_file:               # filename
        minimal_prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        unbox_pointer_internal %rdi
        add     $header_size, %rax
        call_fn fopen, %rax, $write_mode
        perror
        tag     TAG_PORT, %rax
        return

current_output_port:
        arity_check 0
        tag     TAG_PORT, stdout
        ret

current_input_port:
        arity_check 0
        tag     TAG_PORT, stdin
        ret

current_error_port:
        arity_check 0
        tag     TAG_PORT, stderr
        ret

close_port:                     # port
        minimal_prologue
        arity_check 1
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi
        call_fn fclose, %rax
        test    %eax, %eax
        setz    %al
        box_boolean_internal
        return

close_input_port:               # port
close_output_port:              # port
        minimal_prologue
        arity_check 1
        assert_tag TAG_PORT, %rdi, not_a_port_string
        call_scm close_port, %rdi
        return

open_input_string:              # string
        open_input_buffer_template $-1, TAG_STRING, not_a_string_string

open_input_bytevector:          # bytevector
        open_input_buffer_template $0, TAG_OBJECT, not_a_bytevector_string

        ## 6.13.2. Input

read:                           # port
        prologue
        optional_parameter_arg 1, current_input_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi
        call_fn read_datum, %rax
        return

read_char:                      # port
        prologue
        optional_parameter_arg 1, current_input_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi
        call_fn fgetc, %rax
        cmp     $EOF, %al
        je      1f
        tag     TAG_CHAR, %rax
        return
1:      return  $EOF_OBJECT

peek_char:                      # port
        prologue
        optional_parameter_arg 1, current_input_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi, %rbx
        call_fn fgetc, %rbx
        call_fn ungetc, %rax, %rbx
        cmp     $EOF, %al
        je      1f
        tag     TAG_CHAR, %rax
        return
1:      return  $EOF_OBJECT

is_eof_object:                  # obj
        arity_check 1
        is_eof_object_internal %rdi, store=true
        box_boolean_internal
        ret

eof_object:
        arity_check 0
        mov     $EOF_OBJECT, %rax
        ret

read_u8:                        # port
        prologue
        optional_parameter_arg 1, current_input_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi
        call_fn fgetc, %rax
        cmp     $EOF, %al
        je      1f
        tag     TAG_INT, %rax
        return
1:      return  $EOF_OBJECT

peek_u8:                        # port
        prologue
        optional_parameter_arg 1, current_input_port_symbol, %rdi
        unbox_pointer_internal %rdi, %rbx
        call_fn fgetc, %rbx
        call_fn ungetc, %rax, %rbx
        cmp     $EOF, %al
        je      1f
        tag     TAG_INT, %rax
        return
1:      return  $EOF_OBJECT

        ## 6.13.3. Output

write:                          # obj, port
        prologue obj
        mov     %rdi, obj(%rsp)

        optional_parameter_arg 2, current_output_port_symbol, %rsi
        assert_tag TAG_PORT, %rsi, not_a_port_string

        lea     to_string_jump_table, %rbx
        store_pointer $TAG_CHAR, $char_to_machine_readable_string
        store_pointer $TAG_STRING, $string_to_machine_readable_string

        call_scm display, obj(%rsp), %rsi

        store_pointer $TAG_CHAR, $char_to_string
        store_pointer $TAG_STRING, $string_to_string

        return  $VOID

display:                        # obj, port
        prologue obj
        mov     %rdi, obj(%rsp)
        optional_parameter_arg 2, current_output_port_symbol, %rsi
        assert_tag TAG_PORT, %rsi, not_a_port_string
        unbox_pointer_internal %rsi, %rbx
        call_fn to_string, obj(%rsp)
        unbox_pointer_internal %rax, %rdi
        xor     %al, %al
        add     $header_size, %rdi
        call_fn fprintf, %rbx, %rdi
        call_fn fflush, %rbx
        return  $VOID

newline:                        # port
        minimal_prologue
        optional_parameter_arg 1, current_output_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        call_scm write_char, $NEWLINE_CHAR, %rdi
        return

write_char:                     # char, port
        prologue char, port
        optional_parameter_arg 2, current_output_port_symbol, %rsi
        assert_tag TAG_CHAR, %rdi, not_a_character_string
        assert_tag TAG_PORT, %rsi, not_a_port_string
        mov     %edi, char(%rsp)
        unbox_pointer_internal %rsi
        mov     char(%rsp), %edi
        call_fn fputc, %rdi, %rax
        return  $VOID

write_u8:                       # byte, port
        prologue byte
        mov     %edi, byte(%rsp)
        optional_parameter_arg 2, current_output_port_symbol, %rsi
        assert_tag TAG_INT, %rdi, not_an_integer_string
        assert_tag TAG_PORT, %rsi, not_a_port_string
        unbox_pointer_internal %rsi
        mov     byte(%rsp), %edi
        call_fn fputc, %rdi, %rax
        return  $VOID

flush_output_port:              # port
        prologue
        optional_parameter_arg 1, current_output_port_symbol, %rdi
        assert_tag TAG_PORT, %rdi, not_a_port_string
        unbox_pointer_internal %rdi
        call_fn fflush, %rax
        return  $VOID

        ## 6.14. System interface

load:                           # filename, environment-specifier
        prologue
        arity_check 2, jle
        assert_tag TAG_STRING, %rdi, not_a_string_string
        call_scm open_input_file, %rdi
        mov     %rax, %rbx
        call_scm read_all, %rbx
        call_scm close_input_port, %rbx
        return  $VOID

is_file_exists:                 # filename
        minimal_prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        call_fn unbox_string, %rdi
        call_fn access, %rax, $F_OK
        cmp     $-1, %rax
        setne   %al
        and     $C_TRUE, %rax
        box_boolean_internal
        return

delete_file:                    # filename
        minimal_prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        call_fn unbox_string, %rdi
        call_fn unlink, %rax
        perror  jge
        return  $VOID

command_line:
        arity_check 0
        mov     command_line_arguments, %rax
        ret

exit_:                          # obj
        prologue
        arity_check 1

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

2:      assert_tag TAG_INT, %rdi, not_an_integer_string
        mov     %edi, %edi

3:      mov     %rdi, %rbx
        call_fn exit_dynamic_extent

        call_fn exit, %rbx
        return

emergency_exit:                 # obj
        minimal_prologue
        arity_check 1
        assert_tag TAG_INT, %rdi, not_an_integer_string
        mov     %edi, %edi
        call_fn exit, %rdi
        return

get_environment_variables:
        arity_check 0
        mov     environment_alist, %rax
        ret

current_second:
        prologue tv_sec, tv_usec
        arity_check 0
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
        arity_check 0
        call_fn clock
        box_int_internal
        return

jiffies_per_second:
        arity_check 0
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

        mov     $CPUID_FEATURE_INFORMATION, %rax
        cpuid
        test    $(SSE4_1 | SSE4_2), %ecx
        jnz     1f
        xor     %al, %al
        call_fn printf, $cpuid_error
        return  $1

1:      call_fn init_pointer_stack, $object_space, $POINTER_STACK_INITIAL_SIZE
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
        store_pointer $TAG_OBJECT, $gc_mark_queue_object

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
        intern_symbol continuation_symbol, "continuation", id=TAG_CONTINUATION
        intern_symbol values_symbol, "values", id=TAG_VALUES
        intern_symbol handle_symbol, "handle", id=TAG_HANDLE
        intern_symbol c_procedure_symbol, "c-procedure", id=TAG_C_PROCEDURE

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

        intern_symbol quote_internal_symbol, "quote-internal"
        intern_symbol lambda_internal_symbol, "lambda-internal"
        intern_symbol if_internal_symbol, "if-internal"
        intern_symbol set_internal_symbol, "set!-internal"
        intern_symbol let_internal_symbol, "let-internal"
        intern_symbol letrec_internal_symbol, "letrec-internal"
        intern_symbol define_syntax_internal_symbol, "define-syntax-internal"

        intern_symbol car_symbol, "car"
        intern_symbol cdr_symbol, "cdr"

        intern_symbol equal_symbol, "="
        intern_symbol less_than_symbol, "<"
        intern_symbol less_than_or_equal_symbol, "<="
        intern_symbol greater_than_symbol, ">"
        intern_symbol greater_than_or_equal_symbol, ">="

        intern_symbol plus_symbol, "+"
        intern_symbol minus_symbol, "-"
        intern_symbol multiply_symbol, "*"
        intern_symbol divide_symbol, "/"

        intern_symbol string_length_symbol, "string-length"
        intern_symbol string_ref_symbol, "string-ref"
        intern_symbol string_set_symbol, "string-set!"

        intern_symbol vector_length_symbol, "vector-length"
        intern_symbol vector_ref_symbol, "vector-ref"
        intern_symbol vector_set_symbol, "vector-set!"

        intern_symbol bytevector_length_symbol, "bytevector-length"
        intern_symbol bytevector_u8_ref_symbol, "bytevector-u8-ref"
        intern_symbol bytevector_u8_set_symbol, "bytevector-u8-set!"

        mov     symbol_next_id, %rax
        mov     %rax, max_null_environment_symbol

        intern_symbol dot_symbol, "."
        intern_symbol void_symbol, "void"
        intern_symbol eof_symbol, "eof"
        intern_symbol error_symbol, "error"
        intern_symbol exception_handler_stack_symbol, "exception-handler-stack"
        intern_symbol dynamic_extent_stack_symbol, "dynamic-extent-stack"

        intern_symbol current_output_port_symbol, "current-output-port"
        intern_symbol current_input_port_symbol, "current-input-port"
        intern_symbol current_error_port_symbol, "current-error-port"

        intern_string empty_string, ""

        intern_string signal_error_string, "Signal:"
        intern_string read_error_string, "Unexpected input:"
        intern_string code_space_error_string, "Code space exceeded:"
        intern_string not_a_character_string, "Not a character:"
        intern_string not_a_number_string, "Not a number:"
        intern_string not_an_integer_string, "Not an integer:"
        intern_string not_a_pair_string, "Not a pair:"
        intern_string not_a_string_string, "Not a string:"
        intern_string not_a_symbol_string, "Not a symbol:"
        intern_string not_a_vector_string, "Not a vector:"
        intern_string not_a_procedure_string, "Not a procedure:"
        intern_string not_a_port_string, "Not a port:"
        intern_string not_a_bytevector_string, "Not a bytevector:"
        intern_string not_a_record_string, "Not a record:"
        intern_string not_a_handle_string, "Not a handle:"
        intern_string not_a_c_procedure_string, "Not a C procedure:"
        intern_string symbol_not_defined_string, "Symbol not defined:"
        intern_string arity_check_error_string, "Unexpected number of arguments:"
        intern_string too_high_arity_error_string, "Maximum arity is 6, was:"

        intern_string false_string, "#f"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_FALSE
        intern_string true_string, "#t"
        mov     %rax, boolean_string_table + POINTER_SIZE * C_TRUE

        intern_string eof_object_char, "#\\eof"

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
        movb    $7, 97(%rbx)
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
        store_pointer $TAG_SYMBOL, $symbol_to_string_internal
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
        store_pointer $TAG_PROCEDURE, $unbox_procedure
        store_pointer $TAG_PORT, $unbox_port
        store_pointer $TAG_STRING, $unbox_string
        store_pointer $TAG_PAIR, $unbox_pair
        store_pointer $TAG_VECTOR, $unbox_vector
        store_pointer $TAG_OBJECT, $unbox_object

        lea     box_jump_table, %rbx
        store_pointer $TAG_DOUBLE, $box_double
        store_pointer $TAG_BOOLEAN, $box_boolean
        store_pointer $TAG_CHAR, $box_char
        store_pointer $TAG_INT, $box_integer
        store_pointer $TAG_PROCEDURE, $box_procedure
        store_pointer $TAG_PORT, $box_port
        store_pointer $TAG_STRING, $box_string

        lea     integer_to_string_format_table, %rbx
        store_pointer $8, $oct_format
        store_pointer $10, $int_format
        store_pointer $16, $hex_format

        lea     read_datum_jump_table, %rbx
        store_pointer $'\#, $read_hash
        store_pointer $'(, $read_list
        store_pointer $'[, $read_list
        store_pointer $'\", $read_string
        .irp digit, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9
        store_pointer $(\digit + '0), $read_number
        .endr
        store_pointer $'+, $read_number_or_symbol
        store_pointer $'-, $read_number_or_symbol
        store_pointer $'., $read_number_or_symbol
        store_pointer $'', $read_quote
        store_pointer $'`, $read_quasiquote
        store_pointer $',, $read_unquote
        store_pointer $'), $NULL
        store_pointer $'], $NULL

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
        store_pointer $R11, $jit_pop_r11

        lea     jit_pop_register_size_table, %rbx
        store_pointer $RAX, $jit_pop_rax_size
        store_pointer $RCX, jit_pop_rcx_size
        store_pointer $RDX, jit_pop_rdx_size
        store_pointer $RSI, jit_pop_rsi_size
        store_pointer $RDI, jit_pop_rdi_size
        store_pointer $R8, jit_pop_r8_size
        store_pointer $R9, jit_pop_r9_size
        store_pointer $R11, jit_pop_r11_size

        lea     jit_literal_to_register_table, %rbx
        store_pointer $RAX, $jit_literal_to_rax
        store_pointer $RCX, $jit_literal_to_rcx
        store_pointer $RDX, $jit_literal_to_rdx
        store_pointer $RSI, $jit_literal_to_rsi
        store_pointer $RDI, $jit_literal_to_rdi
        store_pointer $R8, $jit_literal_to_r8
        store_pointer $R9, $jit_literal_to_r9
        store_pointer $R10, $jit_literal_to_r10
        store_pointer $R11, $jit_literal_to_r11

        lea     jit_literal_to_register_size_table, %rbx
        store_pointer $RAX, jit_literal_to_rax_size
        store_pointer $RCX, jit_literal_to_rcx_size
        store_pointer $RDX, jit_literal_to_rdx_size
        store_pointer $RSI, jit_literal_to_rsi_size
        store_pointer $RDI, jit_literal_to_rdi_size
        store_pointer $R8, jit_literal_to_r8_size
        store_pointer $R9, jit_literal_to_r9_size
        store_pointer $R10, jit_literal_to_r10_size
        store_pointer $R11, jit_literal_to_r11_size

        lea     jit_rax_to_register_table, %rbx
        store_pointer $RAX, $jit_rax_to_rax
        store_pointer $RCX, $jit_rax_to_rcx
        store_pointer $RDX, $jit_rax_to_rdx
        store_pointer $RSI, $jit_rax_to_rsi
        store_pointer $RDI, $jit_rax_to_rdi
        store_pointer $R8, $jit_rax_to_r8
        store_pointer $R9, $jit_rax_to_r9
        store_pointer $11, $jit_rax_to_r11

        lea     jit_rax_to_register_size_table, %rbx
        store_pointer $RAX, jit_rax_to_rax_size
        store_pointer $RCX, jit_rax_to_rcx_size
        store_pointer $RDX, jit_rax_to_rdx_size
        store_pointer $RSI, jit_rax_to_rsi_size
        store_pointer $RDI, jit_rax_to_rdi_size
        store_pointer $R8, jit_rax_to_r8_size
        store_pointer $R9, jit_rax_to_r9_size
        store_pointer $R11, jit_rax_to_r11_size

        lea     jit_local_to_register_table, %rbx
        store_pointer $RAX, $jit_local_to_rax
        store_pointer $RCX, $jit_local_to_rcx
        store_pointer $RDX, $jit_local_to_rdx
        store_pointer $RSI, $jit_local_to_rsi
        store_pointer $RDI, $jit_local_to_rdi
        store_pointer $R8, $jit_local_to_r8
        store_pointer $R9, $jit_local_to_r9
        store_pointer $R11, $jit_local_to_r11

        lea     jit_local_to_register_size_table, %rbx
        store_pointer $RAX, jit_local_to_rax_size
        store_pointer $RCX, jit_local_to_rcx_size
        store_pointer $RDX, jit_local_to_rdx_size
        store_pointer $RSI, jit_local_to_rsi_size
        store_pointer $RDI, jit_local_to_rdi_size
        store_pointer $R8, jit_local_to_r8_size
        store_pointer $R9, jit_local_to_r9_size
        store_pointer $R11, jit_local_to_r11_size

        lea     jit_register_to_local_table, %rbx
        store_pointer $RAX, $jit_rax_to_local
        store_pointer $RCX, $jit_rcx_to_local
        store_pointer $RDX, $jit_rdx_to_local
        store_pointer $RSI, $jit_rsi_to_local
        store_pointer $RDI, $jit_rdi_to_local
        store_pointer $R8, $jit_r8_to_local
        store_pointer $R9, $jit_r9_to_local
        store_pointer $R11, $jit_r11_to_local

        lea     jit_register_to_local_size_table, %rbx
        store_pointer $RAX, jit_rax_to_local_size
        store_pointer $RCX, jit_rcx_to_local_size
        store_pointer $RDX, jit_rdx_to_local_size
        store_pointer $RSI, jit_rsi_to_local_size
        store_pointer $RDI, jit_rdi_to_local_size
        store_pointer $R8, jit_r8_to_local_size
        store_pointer $R9, jit_r9_to_local_size
        store_pointer $R11, jit_r11_to_local_size

        define "make-record", $make_record
        define "record-ref", $record_ref
        define "record-set!", $record_set

        lea     jit_syntax_jump_table, %rbx
        .irp symbol, quote, if, set, lambda, begin, let, letrec, define_syntax
        store_pointer \symbol\()_symbol, $jit_\symbol
        .endr

        lea     jit_syntax_jump_table, %rbx
        .irp symbol, quote, if, set, lambda, let, letrec, define_syntax
        store_pointer \symbol\()_internal_symbol, $jit_\symbol
        .endr

        .irp name, eq, eqv, number, complex, real, rational, integer, exact, inexact, infinite, nan
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

        .irp name, ceiling, truncate, round, floor, exp, log, sin, cos, tan, asin, acos, atan, atan2, sqrt, expt
        define "\name", $\name\()_
        .endr

        define "inexact", $inexact
        define "exact", $exact

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

        define "bytevector?", $is_bytevector
        define "make-bytevector", $make_bytevector
        define "bytevector-length", $bytevector_length
        define "bytevector-u8-ref", $bytevector_u8_ref
        define "bytevector-u8-set!", $bytevector_u8_set
        define "list->bytevector", $list_to_bytevector

        define "procedure?", $is_procedure
        define "apply", $apply
        define "call-with-current-continuation", $call_with_current_continuation
        define "values", $values
        define "call-with-values", $call_with_values

        define "raise", $raise

        define "eval", $eval
        define "scheme-report-environment", $scheme_report_environment
        define "null-environment", $null_environment
        define "interaction-environment", $interaction_environment

        define "call-with-port", $call_with_port
        define "input-port?", $is_input_port
        define "output-port?", $is_output_port
        define "current-input-port", $current_input_port
        define "current-output-port", $current_output_port
        define "current-error-port", $current_error_port
        define "open-input-file", $open_input_file
        define "open-output-file", $open_output_file
        define "close-port", $close_port
        define "close-input-port", $close_input_port
        define "close-output-port", $close_output_port
        define "open-input-string", $open_input_string
        define "open-input-bytevector", $open_input_bytevector

        define "read", $read
        define "read-char", $read_char
        define "peek-char", $peek_char
        define "eof-object?", $is_eof_object
        define "eof-object", $eof_object
        define "read-u8", $read_u8
        define "peek-u8", $peek_u8

        define "write", $write
        define "display", $display
        define "newline", $newline
        define "write-char", $write_char
        define "write-u8", $write_u8
        define "flush-output-port", $flush_output_port

        define "load", $load
        define "file-exists?", $is_file_exists
        define "delete-file", $delete_file
        define "command-line", $command_line
        define "exit", $exit_
        define "emergency-exit", $emergency_exit
        define "get-environment-variables", $get_environment_variables
        define "current-second", $current_second
        define "current-jiffy", $current_jiffy
        define "jiffies-per-second", $jiffies_per_second

        ## .irp symbol, plus, minus, multiply, divide, equal, less_than, less_than_or_equal, greater_than, greater_than_or_equal
        ## lea     jit_inline_table, %rbx
        ## store_pointer \symbol\()_symbol, $\symbol
        ## lea     jit_inline_size_table, %rbx
        ## store_pointer \symbol\()_symbol, \symbol\()_size
        ## .endr

        ## .irp symbol, car, cdr, string_length, string_ref, string_set, vector_length, vector_ref, vector_set
        ## lea     jit_inline_table, %rbx
        ## store_pointer \symbol\()_symbol, $\symbol
        ## lea     jit_inline_size_table, %rbx
        ## store_pointer \symbol\()_symbol, \symbol\()_size
        ## .endr

        ## .irp symbol, bytevector_length, bytevector_u8_ref, bytevector_u8_set
        ## lea     jit_inline_table, %rbx
        ## store_pointer \symbol\()_symbol, $\symbol
        ## lea     jit_inline_size_table, %rbx
        ## store_pointer \symbol\()_symbol, \symbol\()_size
        ## .endr

        call_fn box_string, $boot_scm
        call_scm open_input_string, %rax
        call_scm read_all, %rax

        call_fn box_string, $r7rs_scm
        call_scm open_input_string, %rax
        call_scm read_all, %rax

        mov     symbol_next_id, %rax
        mov     %rax, max_scheme_report_environment_symbol

        define "read-all", $read_all
        define "gc", $gc
        define "object-space-size", $object_space_size
        define "class-of", $class_of

        define "dlopen", $dlopen_
        define "dlsym", $dlsym_
        define "ffi-call", $ffi_call

        call_fn box_string_array_as_list, argv(%rsp)
        mov     %rax, command_line_arguments

        call_fn box_string_array_as_list, environ
        call_fn build_environment_alist, %rax
        mov     %rax, environment_alist

        call_scm gc
        call_fn signal, $SIGSEGV, $segv_handler

        call_scm length, command_line_arguments
        cmp     $1, %eax
        jg      2f

        call_fn malloc, $JMP_BUF_SIZE
        perror
        mov     %rax, segv_jmp_buffer
        call_fn setjmp, segv_jmp_buffer
        test    %eax, %eax
        jz      2f

        box_int_internal %eax
        call_scm internal_error, signal_error_string, %rax
        return  $1

2:      call_fn box_string, $init_scm
        call_scm open_input_string, %rax
        call_scm read_all, %rax

        return  $0

exit_dynamic_extent:
        prologue
        parameter_value dynamic_extent_stack_symbol
        mov     %rax, %rbx

1:      is_nil_internal %rbx
        je      2f
        car     %rbx
        cdr     %rax
        unbox_pointer_internal %rax, %r11
        call_scm *%r11
        cdr     %rbx, %rbx
        jmp     1b

2:      return


segv_handler:                   # signal
        prologue stacktrace
        mov     %rdi, %rbx

        call_fn malloc, $(POINTER_SIZE * STACKTRACE_SIZE)
        perror
        mov     %rax, stacktrace(%rsp)
        call_fn backtrace, stacktrace(%rsp), $STACKTRACE_SIZE
        call_fn backtrace_symbols_fd, stacktrace(%rsp), $STACKTRACE_SIZE, $STDERR_FILENO
        call_fn free, stacktrace(%rsp)

        cmp     $NULL, segv_jmp_buffer
        je      1f
        call_fn longjmp, segv_jmp_buffer, %rbx
1:      call_fn exit, $1
        return

internal_error:                 # message, irritants
        minimal_prologue
        mov     error_symbol, %r11d
        mov     symbol_table_values(,%r11d,POINTER_SIZE), %r11
        unbox_pointer_internal %r11, %r11
        cmp     $NULL, %r11
        je      1f
        call   *%r11
        return
1:      call_fn exit, $1
        return

box_string_array_as_list:       # c-string-array
        prologue strings
        mov     %rdi, %r12
        mov     $NIL, %rax
        mov     %rax, strings(%rsp)

        xor     %ebx, %ebx

1:      mov     (%r12,%rbx,POINTER_SIZE), %rax
        test    %rax, %rax
        jz      2f

        call_fn box_string, %rax
        call_scm cons, %rax, strings(%rsp)
        mov     %rax, strings(%rsp)

        inc     %ebx
        jmp     1b

2:      call_scm reverse, strings(%rsp)
        call_fn jit_maybe_add_to_constant_pool, %rax
        return  %rax

build_environment_alist:        # list
        prologue env_var, env_var_name, save_ptr
        mov     %rdi, %rbx
        mov     %rdi, %r12
1:      is_nil_internal %r12
        je      3f

        car     %r12
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
        call_scm cons, env_var_name(%rsp), %rax
        call_scm set_car, %r12, %rax

        call_fn free, env_var(%rsp)

        cdr     %r12, %r12
        jmp     1b

3:      return  %rbx

        ## Public API

read_all:                       # port
        prologue
        arity_check 1
        assert_tag TAG_PORT, %rdi, not_a_port_string
        mov     %rdi, %rbx
1:      call_scm read, %rbx
        is_eof_object_internal %rax
        je      2f
        call_scm eval, %rax, $MAX_NUMBER_OF_SYMBOLS
        jmp     1b
2:      return  $TRUE

class_of:                       # obj
        prologue
        arity_check 1
        mov     %rdi, %rbx
        extract_tag
        cmp     $TAG_OBJECT, %rax
        je      1f
        tag     TAG_SYMBOL, %rax
        return
1:      xor     %eax, %eax
        unbox_pointer_internal %rbx, %r11
        test    %r11, %r11
        jz      2f
        mov     header_object_type(%r11), %ax
        tag     TAG_SYMBOL, %rax
        return
2:      return  void_symbol

object_space_size:
        arity_check 0
        mov     stack_top_offset + object_space, %rax
        shr     $POINTER_SIZE_SHIFT, %rax
        box_int_internal
        ret

dlopen_:                        # filename
        minimal_prologue
        arity_check 1
        assert_tag TAG_STRING, %rdi, not_a_string_string
        call_fn unbox_string, %rdi
        call_fn dlopen, %rax
        perror  jz
        call_scm make_vector, $ONE_INT, %rax
        unbox_pointer_internal
        movw    $TAG_HANDLE, header_object_type(%rax)
        tag     TAG_OBJECT, %rax
        return

dlsym_:                         # symbol-string, handle
        prologue
        mov     %rdi, %rbx
        cmp     $1, %al
        jne     1f
        assert_tag TAG_STRING, %rdi, not_a_string_string
        mov     $RTLD_DEFAULT, %r12
        jmp     2f

1:      arity_check 2
        assert_tag TAG_STRING, %rdi, not_a_string_string
        assert_object %rsi, TAG_HANDLE, not_a_handle_string
        mov     %rsi, %rdi
        call_scm record_ref, %rdi, $ZERO_INT
        mov     %rax, %r12

2:      call_fn dlerror
        call_fn unbox_string, %rbx
        call_fn dlsym, %r12, %rax
        mov     %rax, %rbx
        call_fn dlerror
        perror  jz
        call_scm make_vector, $ONE_INT, %rbx
        unbox_pointer_internal
        movw    $TAG_C_PROCEDURE, header_object_type(%rax)
        tag     TAG_OBJECT, %rax
        return

ffi_call:                       # c-procedure or symbol-string, return-type-symbol, args ...
        arity_check 2, jge
        mov     $2, %r10d
        push    %rbp
        mov     %rsp, %rbp
        call_fn jit_rt_lambda_collect_varargs
        pop     %rbp
        prologue
        has_tag TAG_STRING, %rdi, store=false
        jne     1f
        push    %rsi
        push    %rdx
        call_scm dlsym_, %rdi
        mov     %rax, %rdi
        pop     %rdx
        pop     %rsi

1:      assert_object %rdi, TAG_C_PROCEDURE, not_a_c_procedure_string
        assert_tag TAG_SYMBOL, %rsi, not_a_symbol_string
        assert_tag TAG_PAIR, %rdx, not_a_pair_string
        mov     %rsi, %rbx
        mov     %rdx, %r12
        call_scm record_ref, %rdi, $ZERO_INT
        call_fn ffi_apply, %rax, %r12
        xor     %edx, %edx
        cmp     void_symbol, %rbx
        je      2f
        cmp     double_symbol, %rbx
        je      3f
        call_fn box, %rax, %rbx
        return
2:      return  $VOID
3:      return  %xmm0

ffi_apply:                      # proc, args
        prologue
        push    %r14
        push    %r13
        unbox_pointer_internal %rdi, %r13
        mov     %rsi, %r12
        mov     %rsi, %r14

        xor     %ebx, %ebx
1:      is_nil_internal %r12
        je      3f
        car     %r12, %rdi
        is_double_internal %rdi
        jne     2f
        push    %rdi
        inc     %ebx
2:      cdr     %r12, %r12
        jmp     1b

3:      mov     %r14, %r12
        mov     %ebx, %r14d
        mov     $MAX_REGISTER_DOUBLE_ARGS, %eax
        sub     %ebx, %eax
        js      ffi_apply_pop_doubles
        lea     ffi_apply_pop_doubles(,%eax,FFI_APPLY_DOUBLES_JUMP_ALIGNMENT), %rax
        jmp     *%rax

        .align  16
ffi_apply_pop_doubles:
        .irp reg, xmm7, xmm6, xmm5, xmm4, xmm3, xmm2, xmm1, xmm0
        pop     %rax
        movq    %rax, %\reg
        .align  FFI_APPLY_DOUBLES_JUMP_ALIGNMENT
        .endr

        xor     %ebx, %ebx
4:      is_nil_internal %r12
        je      6f
        car     %r12, %rdi
        is_double_internal %rdi
        je      5f
        call_fn unbox, %rdi
        push    %rax
        inc     %ebx
5:      cdr     %r12, %r12
        jmp     4b

6:      mov     $MAX_REGISTER_ARGS, %eax
        sub     %ebx, %eax
        js      ffi_apply_pop_ints
        lea     ffi_apply_pop_ints(,%eax,FFI_APPLY_INTS_JUMP_ALIGNMENT), %rax
        jmp     *%rax

        .align  16
ffi_apply_pop_ints:
        .irp reg, r9, r8, rcx, rdx, rsi, rdi
        pop     %\reg
        .align  FFI_APPLY_INTS_JUMP_ALIGNMENT
        .endr
        mov     %r14b, %al
        call    *%r13
        pop     %r13
        pop     %r14
        return

        ## Boxing from C

box_boolean:                    # c-boolean
        cmp     $0, %edi
        je      1f
        mov     $TRUE, %rax
        ret
1:      mov     $FALSE, %rax
        ret

box_integer:                    # c-int
        box_int_internal %edi
        ret

box_char:                       # c-char
        mov     %edi, %edi
        tag     TAG_CHAR, %rdi
        ret

box_double:                     # c-double
        movq    %rdi, %rax
        ret

box_string:                     # c-string
        prologue str, size
        mov     %rdi, %rbx
        cmp     $NULL, %rbx
        je      3f

1:      open_string_buffer str(%rsp), size(%rsp), %r12
        cmpb    $0, (%rbx)
        je      2f
        xor     %al, %al
        call_fn fprintf, %r12, $string_format, %rbx
2:      string_buffer_to_string str(%rsp), size(%rsp), %r12
        return

3:      return empty_string

box_procedure:                  # c-procedure
        tag     TAG_PROCEDURE, %rdi
        ret

box_port:                       # c-port
        tag     TAG_PORT, %rdi
        ret

box:                            # value, tag
        minimal_prologue
        mov     box_jump_table(,%esi,POINTER_SIZE), %rax
        call_fn *%rax, %rdi
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
        is_nil_internal %rdi
        je      1f
        unbox_pointer_internal %rdi
        add     $header_size, %rax
1:      mov     $NULL, %rax
        ret

unbox_object:                   # object
        unbox_pointer_internal %rdi
        test    %rax, %rax
        jz      1f
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
        call_scm gc
        call_fn malloc, %rbx
        perror
1:      mov     %rax, %r12
        call_fn memset, %r12, $NULL, %rbx
        return  %r12

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
        unbox_pointer_internal %rdi, %r12
        mov     header_object_size(%r12), %ebx
1:      test    %ebx, %ebx
        jz      2f

        sub     $POINTER_SIZE, %ebx
        mov     header_size(%r12,%rbx), %rdi
        call_fn gc_maybe_mark, %rdi
        jmp     1b
2:      return

gc_mark_queue_object:           # object
        prologue
        mov     %rdi, %rbx
        call_scm class_of, %rdi
        eq_internal $TAG_BYTEVECTOR, %eax, store=false
        je      1f

        call_fn gc_mark_queue_vector, %rbx
1:      return

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

        .type gc_mark_queue_stack, @function
        .size gc_mark_queue_stack, . - gc_mark_queue_stack

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
        arity_check 0
        call_fn gc_mark
        call_fn gc_sweep

        call_scm object_space_size
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

2:      mov     header_size(%rbx,%r12), %dl
        xor     %al, %al
        call_fn fprintf, stream(%rsp), $int_format, %rdx

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

        car     %r12
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        add     $header_size, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        cdr     %r12, %r12
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

symbol_to_string_internal:      # symbol
        mov     symbol_table_names(,%edi,POINTER_SIZE), %rax
        ret

char_to_string:                 # char
        prologue str, size
        mov     %edi, %ebx
        cmp     $EOF, %ebx
        jne     1f
        return  eof_object_char

1:      open_string_buffer str(%rsp), size(%rsp), %r12
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

1:      cmp     $EOF, %ebx
        jne     2f
        return  eof_object_char

2:      mov     char_to_string_table(,%ebx,POINTER_SIZE), %rax
        test    %rax, %rax
        jz      3f
        return
3:      open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
        call_fn fprintf, %r12, $machine_readable_char_format, %rbx
        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

integer_to_string:              # int
        minimal_prologue
        call_fn integer_to_string_internal %rdi, $DECIMAL_RADIX_INT
        return

integer_to_string_internal:     # int, radix
        prologue str, size, format
        mov     integer_to_string_format_table(,%esi,POINTER_SIZE), %rax
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
        movq    %rdi, %xmm0
        open_string_buffer str(%rsp), size(%rsp), %r12
        mov     %r12, %rdi
        mov     $double_format, %rsi
        mov     $1, %al         # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        call    fprintf
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
        test    %rbx, %rbx
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
        call_scm class_of, %rdi
        mov     %rax, %rbx
        mov     to_string_jump_table(,%eax,POINTER_SIZE), %r11
        cmp     $object_to_string, %r11
        je      1f
        test    %r11, %r11
        jnz     2f
1:      call_fn to_string, %rbx
        call_fn unbox_string, %rax
        mov     %rax, %rbx
        open_string_buffer str(%rsp), size(%rsp), %r12
        xor     %al, %al
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

        cmp     $';, %r12b
        je      3f
        call_fn ungetc, %r12, %rbx
        return

2:      tag     TAG_CHAR, %rax
        return

3:      call_fn ungetc, %r12, %rbx
        call_fn read_comment, %rbx
        jmp     1b

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

        call_fn read_whitespace, %rbx
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

        cmpb    $0, (%rbx)
        je      1f

        call_scm string_to_symbol, str(%rsp)
        mov     %rax, %rbx
        register_for_gc str(%rsp)
        return  %rbx

1:      return  $VOID

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

        mov     unescape_char_table(%eax), %r11b
        test    %r11b, %r11b
        jz      8f
        mov     %r11, %rax
        jmp     3f

2:      lea     hex(%rsp), %rdx
        call_fn fscanf, %rbx, $hex_format, %rdx
        perror  jge

        call_fn fputc, hex(%rsp), %r12
        call_fn fgetc, %rbx
        cmp     $';, %al
        jne     8f

        jmp     1b

3:      call_fn fputc, %rax, %r12
        jmp     1b

5:      cmpq    $0, idx(%rsp)
        je      6f

        string_buffer_to_string str(%rsp), size(%rsp), %r12
        register_for_gc
        return

6:      return  empty_string

7:      call_fn read_error, $EOF_OBJECT
        return

8:      tag     TAG_CHAR, %rax
        call_fn read_error, %rax
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
        call_scm string_length, %r12
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
        jnz     3f

        tag     TAG_CHAR, $0
        return

3:      tag     TAG_STRING, %r12
        call_fn read_error, %rax
        return

4:      call_scm string_ref, %r12, $ZERO_INT
        return

read_quote:                     # c-stream
        minimal_prologue
        call_fn read_datum, %rdi
        call_scm cons, %rax, $NIL
        call_scm cons, quote_symbol, %rax
        return

read_quasiquote:                # c-stream
        minimal_prologue
        call_fn read_datum, %rdi
        call_scm cons, %rax, $NIL
        call_scm cons, quasiquote_symbol, %rax
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
        call_scm cons, %rax, $NIL
        call_scm cons, %r12, %rax
        return

read_vector:                    # c-stream
        prologue
        call_fn read_list, %rdi, $'(
        call_scm list_to_vector, %rax
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

        call_fn read_list, %rbx, $'(
        call_scm list_to_bytevector, %rax
        return

1:      tag     TAG_CHAR, %rax
        call_fn read_error, %rax
        return

read_list:                      # c-stream, c-char
        prologue head, closing
        mov     %rdi, %rbx

        movb    $'), closing(%rsp)
        cmp     $'(, %esi
        je      1f

        movb    $'], closing(%rsp)

1:      mov     $NIL, %r12
        mov     %r12, head(%rsp)
2:      call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     closing(%rsp), %al
        je      5f

        call_fn ungetc, %rax, %rbx

        call_fn read_datum, %rbx
        cmp     dot_symbol, %rax
        je      4f

        call_scm cons, %rax, $NIL
        is_nil_internal %r12
        jne     3f
        mov     %rax, %r12
        mov     %r12, head(%rsp)
        jmp     2b

3:      mov     %r12, %rdi
        mov     %rax, %r12
        call_scm set_cdr, %rdi, %r12
        jmp     2b

4:      call_fn read_datum, %rbx
        call_scm set_cdr, %r12, %rax

        call_fn read_whitespace, %rbx
        call_fn fgetc, %rbx
        cmp     closing(%rsp), %al
        je      5f
        tag     TAG_CHAR, %rax
        call_fn read_error, %rax

5:      return  head(%rsp)

read_error:                     # error
        minimal_prologue
        call_scm internal_error, read_error_string, %rdi
        return

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
1:      mov     jit_code_space_next_address, %rax
        sub     jit_code_space, %rax
        box_int_internal
        call_scm internal_error, code_space_error_string, %rax
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

        lea     code(%rsp), %r8
        call_fn jit_procedure, %r12, %rbx, env(%rsp), args(%rsp), %r8
        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        mov     %rax, %rbx
        call_fn free, code(%rsp)
        return  %rbx

jit_datum:                      # form, c-stream, environment, register, tail
        minimal_prologue
        tagged_jump jit_jump_table
        return

        ## 4.1.1. Variable references

jit_index_of_local:             # environment, symbol
        minimal_prologue
        xor     %ecx, %ecx
1:      is_nil_internal %rdi
        je      2f

        car     %rdi
        cmp     %rsi, %rax
        je      3f

        cdr     %rdi, %rdi
        inc     %rcx
        jmp     1b

2:      mov     $-1, %rcx
3:      return  %rcx

jit_symbol:                     # symbol, c-stream, environment, register, tail
        prologue env, symbol_address, symbol, local, register
        mov     %rdi, symbol(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)

        call_fn jit_index_of_local, env(%rsp), symbol(%rsp)
        cmp     $0, %rax
        jge     3f

2:      call_fn jit_literal, symbol(%rsp), %r12, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_global_symbol_in_rax_to_rax, $1, jit_global_symbol_in_rax_to_rax_size, %r12
        mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        jmp     4f

3:      mov     %rax, %rbx
        call_scm length, env(%rsp)
        sub     %eax, %ebx
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

4:      call_scm length, env(%rsp)
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

        call_fn push_pointer_on_stack, $constant_pool, %rbx
1:      return

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

        cdr     %rbx
        car     %rax
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

        mov     $1, %eax
        call_fn length, env(%rsp)
        return

jit_pair:                       # form, c-stream, environment, register, tail
        prologue env, symbol, syntax, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        is_nil_internal %rbx
        je      6f

        car     %rbx
        mov     %rax, symbol(%rsp)
        has_tag TAG_SYMBOL, symbol(%rsp), store=false
        jne     3f

        call_fn jit_index_of_local, env(%rsp), symbol(%rsp)
        cmp     $0, %rax
        jge     3f

2:      unbox_pointer_internal symbol(%rsp)
        mov     jit_syntax_jump_table(,%rax,POINTER_SIZE), %rax
        test    %rax, %rax
        jnz     4f

3:      call_fn jit_procedure_call, %rbx, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

4:      mov     %rax, syntax(%rsp)
        has_tag TAG_PROCEDURE, %rax, store=false
        je      5f
        unbox_pointer_internal syntax(%rsp)
        mov     %rax, %r11
        call_scm *%r11, %rbx, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

5:      unbox_pointer_internal syntax(%rsp)
        mov     %rax, %r11
        call_scm *%r11, %rbx, env(%rsp)
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        return

6:      call_fn jit_literal, $NIL, %r12, $NIL, register(%rsp), $C_FALSE
        return

        ## 4.1.3. Procedure calls

jit_procedure_call:             # form, c-stream, environment, register, tail
        prologue form, len, operand, env, max_locals, register, tail, arity, argc, pushed_args_size
        mov     %rdi, %rbx
        mov     %rbx, form(%rsp)
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movl    $0, argc(%rsp)
        movl    $0, pushed_args_size(%rsp)
        movq    $0, max_locals(%rsp)

        call_scm length, %rbx
        mov     %rax, len(%rsp)
        dec     %eax
        mov     %al, arity(%rsp)

        cmpb    $MAX_REGISTER_ARGS, arity(%rsp)
        jg      16f

0:      car     %rbx
        mov     %rax, operand(%rsp)
        has_tag TAG_PAIR, %rax, store=false
        jne     1f
        call_fn jit_datum, operand(%rsp), %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

        cdr     %rbx, %rbx
        incl    argc(%rsp)

1:      is_nil_internal %rbx
        je      4f

        cmpl    $(MAX_REGISTER_ARGS + 1), argc(%rsp)
        je      4f

        car     %rbx
        has_tag TAG_PAIR, %rax, store=false
        je      2f

        jmp     3f

2:      car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

3:      cdr     %rbx, %rbx
        incl    argc(%rsp)
        jmp     1b

4:      call_scm cdr, form(%rsp)
        call_scm reverse, %rax
        mov     %rax, %rbx
        decl    len(%rsp)

5:      is_nil_internal %rbx
        je      8f
        decl    len(%rsp)
        mov     len(%rsp), %ecx

        cmp     $MAX_REGISTER_ARGS, %ecx
        jge     7f

        car     %rbx
        has_tag TAG_PAIR, %rax, store=false
        je      6f

        xor     %r11d, %r11d
        mov     jit_argument_to_register_id_table(%rcx), %r11b
        car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), %r11, $C_FALSE
        update_max_locals max_locals(%rsp)

        jmp     7f

6:      xor     %r11d, %r11d
        mov     jit_argument_to_register_id_table(%rcx), %r11b
        mov     jit_pop_register_table(,%r11,POINTER_SIZE), %rax
        mov     jit_pop_register_size_table(,%r11,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

7:      cdr     %rbx, %rbx
        jmp     5b

8:      cmpb    $2, arity(%rsp)
        jg      9f
        call_fn fwrite, $jit_clear_multiple_returns_in_rdx, $1, jit_clear_multiple_returns_in_rdx_size, %r12

9:      has_tag TAG_SYMBOL, operand(%rsp), store=false
        je      15f

        has_tag TAG_PAIR, operand(%rsp), store=false
        jne     10f

        call_fn fwrite, $jit_pop_r11, $1, jit_pop_r11_size, %r12
        jmp     11f

10:     call_fn jit_datum, operand(%rsp), %r12, env(%rsp), $R11, $C_FALSE
        update_max_locals max_locals(%rsp)

11:     call_fn fwrite, $jit_unbox_r11, $1, jit_unbox_r11_size, %r12
        call_fn fwrite, $jit_arity_to_al, $1, jit_arity_to_al_size, %r12
        lea     arity(%rsp), %rax
        call_fn fwrite, %rax, $1, $BYTE_SIZE, %r12

        cmpb    $MAX_REGISTER_ARGS, arity(%rsp)
        jg      12f

        cmp     $C_TRUE, tail(%rsp)
        je      13f

12:     call_fn fwrite, $jit_call_r11, $1, jit_call_r11_size, %r12
        cmpl    $0, pushed_args_size(%rsp)
        jg      20f

        jmp     14f

13:     call_fn fwrite, $jit_epilogue, $1, jit_epilogue_size, %r12
        call_fn fwrite, $jit_jump_r11, $1, jit_jump_r11_size, %r12

        return  max_locals(%rsp)

14:     mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12
        return  max_locals(%rsp)

15:     unbox_pointer_internal operand(%rsp), %rbx
        mov     jit_inline_table(,%rbx,POINTER_SIZE), %rax
        test    %rax, %rax
        jz      10b

        mov     jit_inline_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

        jmp     14b

16:     xor     %eax, %eax
        mov     arity(%rsp), %al
        sub     $MAX_REGISTER_ARGS, %al
        shl     $POINTER_SIZE_SHIFT, %eax
        mov     %eax, pushed_args_size(%rsp)

        xor     %r11d, %r11d
        mov     arity(%rsp), %r11b
        movl    %r11d, argc(%rsp)
        cdr     %rbx, %rdi
        call_scm reverse, %rdi
        mov     %rax, %rbx

17:     is_nil_internal %rbx
        je      19f

        cmpl    $MAX_REGISTER_ARGS, argc(%rsp)
        je      19f

        car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_push_rax, $1, jit_push_rax_size, %r12

18:     cdr     %rbx, %rbx
        decl    argc(%rsp)
        jmp     17b

19:     mov     form(%rsp), %rbx
        movl    $0, argc(%rsp)
        jmp     0b

20:     call_fn fwrite, $jit_caller_cleanup, $1, jit_caller_cleanup_size, %r12
        lea     pushed_args_size(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12

        jmp     14b

        ## 4.1.4. Procedures

jit_procedure:                  # form, c-stream, environment, arguments, code-pointer
        prologue env, args, env_size, frame_size, local_idx, local, end_offset, flat_args, varargs_idx, arity, code_pointer
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, args(%rsp)
        mov     %r8, code_pointer(%rsp)
        movl    $0, local_idx(%rsp)
        call_fn jit_lambda_flatten_arguments, args(%rsp)
        mov     %rax, flat_args(%rsp)

        call_fn jit_lambda_varargs_index, args(%rsp)
        mov     %eax, varargs_idx(%rsp)

        call_scm length, env(%rsp)
        mov     %eax, env_size(%rsp)

        call_scm length, flat_args(%rsp)
        mov     %eax, arity(%rsp)
        cmp     $MAX_REGISTER_ARGS, %eax
        jg      5f

        call_fn fwrite, $jit_prologue, $1, jit_prologue_size, %r12
        cmp     $-1, varargs_idx(%rsp)
        jne     4f

        mov     arity(%rsp), %eax
        call_fn jit_literal, %rax, %r12, $NIL, $R10, $C_FALSE
        call_fn fwrite, $jit_arity_check_r10b_with_al, $1, jit_arity_check_r10b_with_al_size, %r12

1:      mov     local_idx(%rsp), %ecx
        cmp     %ecx, arity(%rsp)
        je      3f

2:      xor     %r11d, %r11d
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

        incl    local_idx(%rsp)
        jmp     1b

3:      call_scm reverse, flat_args(%rsp)
        call_scm append, %rax, env(%rsp)
        call_fn jit_datum, %rbx, %r12, %rax, $RAX, $C_TRUE

        shl     $POINTER_SIZE_SHIFT, %eax
        add     $POINTER_SIZE, %eax
        and     $-(2 * POINTER_SIZE), %eax
        mov     %eax, frame_size(%rsp)

        call_fn ftell, %r12
        mov     %rax, end_offset(%rsp)
        mov     jit_prologue_size, %rax
        sub     $INT_SIZE, %rax
        call_fn fseek, %r12, %rax, $SEEK_SET

        lea     frame_size(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12
        call_fn fseek, %r12, end_offset(%rsp), $SEEK_SET

        call_fn fwrite, $jit_epilogue, $1, jit_epilogue_size, %r12
        call_fn fwrite, $jit_return, $1, jit_return_size, %r12
        return

4:      mov     arity(%rsp), %eax
        dec     %eax
        call_fn jit_literal, %rax, %r12, $NIL, $R10, $C_FALSE
        call_fn fwrite, $jit_varargs_arity_check_r10b_with_al, $1, jit_varargs_arity_check_r10b_with_al_size, %r12

        mov     varargs_idx(%rsp), %eax
        call_fn jit_literal, %rax, %r12, $NIL, $R10, $C_FALSE
        call_fn jit_literal, $jit_rt_lambda_collect_varargs, %r12, $NIL, $R11, $C_FALSE
        call_fn fwrite, $jit_call_r11, $1, jit_call_r11_size, %r12
        jmp     1b

5:      call_fn fclose, %r12
        mov     code_pointer(%rsp), %rax
        call_fn free, (%rax)
        mov     arity(%rsp), %eax
        box_int_internal %eax
        call_scm internal_error, too_high_arity_error_string, %rax
        return

jit_lambda_factory_code:        # lambda, c-stream, closure-bitmask
        prologue  rbp, local, local_idx, closure_bitmask, closure_idx, closure_env_size
        unbox_pointer_internal %rdi, %rbx
        mov     %rsi, %r12
        mov     %rbp, rbp(%rsp)
        movq    $0, closure_idx(%rsp)
        movq    $0, local_idx(%rsp)
        mov     %rdx, closure_bitmask(%rsp)
        popcnt  %rdx, %rax
        mov     %rax, closure_env_size(%rsp)

        call_fn jit_literal, %rbx, %r12, $NIL, $R10, $C_FALSE

1:      mov     closure_idx(%rsp), %rax
        cmp     %rax, closure_env_size(%rsp)
        je      2f

        sub     $POINTER_SIZE, rbp(%rsp)
        mov     local_idx(%rsp), %rax
        incl    local_idx(%rsp)
        bt      %rax, closure_bitmask(%rsp)
        jnc     1b

        mov     rbp(%rsp), %rax
        call_fn jit_maybe_add_to_constant_pool, (%rax)
        mov     rbp(%rsp), %rax
        call_fn jit_literal, (%rax), %r12, $NIL, $R11, $C_FALSE

        incl    closure_idx(%rsp)
        mov     closure_idx(%rsp), %ecx
        shl     $POINTER_SIZE_SHIFT, %rcx
        add     $POINTER_SIZE, %rcx
        neg     %ecx
        mov     %ecx, local(%rsp)
        call_fn fwrite, $jit_r11_to_closure, $1, jit_r11_to_closure_size, %r12
        lea     local(%rsp), %rax
        call_fn fwrite, %rax, $1, $INT_SIZE, %r12

        jmp     1b

2:      call_fn fwrite, $jit_jump_r10, $1, jit_jump_r10_size, %r12
        return

jit_lambda_closure_environment: # environment, closure_bitmask
        prologue env, local_idx
        mov     %rsi, %r12
        movq    $0, local_idx(%rsp)
        mov     $NIL, %rax
        mov     %rax, env(%rsp)
        call_scm reverse, %rdi
        mov     %rax, %rbx

1:      is_nil_internal %rbx
        je      3f

        mov     local_idx(%rsp), %rax
        bt      %rax, %r12
        jnc     2f

        car     %rbx, %rdi
        call_scm cons, %rdi, env(%rsp)
        mov     %rax, env(%rsp)

2:      cdr     %rbx, %rbx
        incq    local_idx(%rsp)
        jmp     1b

3:      return  env(%rsp)

jit_lambda_bit_of_closed_over_variable: # symbol, environment, closure_environment
        prologue env, closure_env, env_size
        mov     %rdi, %rbx
        mov     %rsi, env(%rsp)
        mov     %rdx, closure_env(%rsp)
        call_scm length, env(%rsp)
        dec     %eax
        mov     %rax, env_size(%rsp)

        call_fn jit_index_of_local, closure_env(%rsp), %rbx
        cmp     $0, %rax
        jge     1f

        call_fn jit_index_of_local, env(%rsp), %rbx
        cmp     $-1, %rax
        je      1f
        sub     env_size(%rsp), %rax
        neg     %rax
        return  %rax
1:      return  $-1

jit_lambda_closure_environment_bitmask: # form, environment, closure_environment, closure_bitmask
        prologue env, closure_env
        mov     %rdi, %rbx
        mov     %rdx, closure_env(%rsp)
        mov     %rcx, %r12
        mov     %rsi, env(%rsp)

1:      is_nil_internal %rbx
        je      4f

        has_tag TAG_PAIR, %rbx, store=false
        jne     5f

        car     %rbx
        has_tag TAG_PAIR, %rax, store=false
        jne     2f

        car     %rbx
        call_fn jit_lambda_closure_environment_bitmask, %rax, env(%rsp), closure_env(%rsp), %r12
        mov     %rax, %r12
        jmp     3f

2:      car     %rbx
        call_fn jit_lambda_bit_of_closed_over_variable, %rax, env(%rsp), closure_env(%rsp)
        cmp     $-1, %rax
        je      3f

        bts     %rax, %r12
3:      cdr     %rbx, %rbx
        jmp     1b

4:      return  %r12

5:      call_fn jit_lambda_bit_of_closed_over_variable, %rbx, env(%rsp), closure_env(%rsp)
        cmp     $-1, %rax
        je      4b

        bts     %rax, %r12
        jmp     4b

jit_lambda_flatten_arguments:   # arguments
        prologue
        mov     %rdi, %rbx
        mov     $NIL, %r12

1:      is_nil_internal %rbx
        je      2f

        has_tag TAG_PAIR, %rbx, store=false
        jne     3f

        car     %rbx, %rdi
        call_scm cons, %rdi, %r12
        mov     %rax, %r12

        cdr     %rbx, %rbx
        jmp     1b

2:      call_scm reverse, %r12
        return

3:      call_scm cons, %rbx, %r12
        mov     %rax, %r12
        jmp     2b

jit_lambda_varargs_index:       # arguments
        prologue
        mov     %rdi, %rbx
        xor     %r12d, %r12d

1:      is_nil_internal %rbx
        je      2f

        has_tag TAG_PAIR, %rbx, store=false
        jne     3f

        inc     %r12d
        cdr     %rbx, %rbx
        jmp     1b

2:      return  $-1

3:      return  %r12

jit_lambda:                     # form, c-stream, environment, register, tail
        prologue env, args, form, lambda, register, closure_env_bitmask
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)

        cdr     %rbx
        car     %rax
        mov     %rax, args(%rsp)

        call_fn jit_lambda_flatten_arguments, args(%rsp)
        call_fn jit_lambda_closure_environment_bitmask, %rbx, env(%rsp), %rax, $0
        mov     %rax, closure_env_bitmask(%rsp)

        cdr     %rbx, %rbx
        cdr     %rbx, %rsi
        call_scm cons, begin_symbol, %rsi
        mov     %rax, %rbx

        call_fn jit_lambda_closure_environment, env(%rsp), closure_env_bitmask(%rsp)
        call_fn jit_code, %rbx, %rax, args(%rsp)
        mov     %rax, lambda(%rsp)

        cmp     $0, closure_env_bitmask(%rsp)
        je      1f

        call_fn jit_literal, lambda(%rsp), %r12, $NIL, $RDI, $C_FALSE
        call_fn jit_literal, closure_env_bitmask(%rsp), %r12, $NIL, $RSI, $C_FALSE

        call_fn jit_literal, $jit_rt_lambda_factory, %r12, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12
        mov     register(%rsp), %rbx
        mov     jit_rax_to_register_table(,%rbx,POINTER_SIZE), %rax
        mov     jit_rax_to_register_size_table(,%rbx,POINTER_SIZE), %r11
        call_fn fwrite, %rax, $1, %r11, %r12

        call_scm length, env(%rsp)
        return

1:      tag     TAG_PROCEDURE, lambda(%rsp)
        call_fn jit_literal, %rax, %r12, env(%rsp), register(%rsp), $C_FALSE
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

        cdr     %rbx, %rbx
        car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $FALSE
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_rax_is_false_jump, $1, jit_rax_is_false_jump_size, %r12

        call_fn ftell, %r12
        mov     %rax, if_offset(%rsp)

        cdr     %rbx, %rbx
        car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        call_fn fwrite, $jit_jump, $1, jit_jump_size, %r12

        patch_jump %r12, else_offset(%rsp), if_offset(%rsp), jump_offset(%rsp)

        cdr     %rbx
        is_nil_internal %rax
        jne     1f
        mov     $VOID, %rax
        jmp     2f
1:      car     %rax
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
        call_scm length, %rdx
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
        cdr     %rbx, %rbx
        car     %rbx
        mov     %rax, symbol(%rsp)

        cdr     %rbx
        car     %rax
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
        car     %rbx
        car     %rax, %rdi
        call_scm cons, %rdi, %r12
        mov     %rax, %r12

        cdr     %rbx
        mov     %rax, %rbx
        jmp     1b

2:      return  %r12

jit_let_bindings:               # bindings, c-stream, environment, bindings-environment, init-to-void
        prologue env, bindings_env, max_locals, init_to_void
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, bindings_env(%rsp)
        movq    $0, max_locals(%rsp)
        mov     %r8, init_to_void(%rsp)

1:      is_nil_internal %rbx
        je      4f

        car     %rbx
        car     %rax, %rdi
        call_scm cons, %rdi, env(%rsp)
        mov     %rax, env(%rsp)

        cmp     $C_TRUE, init_to_void(%rsp)
        jne     2f

        call_fn jit_literal, $VOID, %r12, $NIL, $RAX, $C_FALSE
        jmp     3f

2:      car     %rbx
        cdr     %rax
        car     %rax
        call_fn jit_datum, %rax, %r12, bindings_env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)

3:      car     %rbx
        car     %rax
        call_fn jit_set_with_rax_as_value, %rax, %r12, env(%rsp)

        cdr     %rbx, %rbx
        jmp     1b

4:      return max_locals(%rsp)

jit_let:                        # form, c-stream, environment, register, tail
        prologue env, max_locals, register, tail
        cdr     %rdi
        mov     %rax, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        car     %rbx
        call_fn jit_let_bindings %rax, %r12, env(%rsp), env(%rsp), $C_FALSE
        update_max_locals max_locals(%rsp)

        car     %rbx
        call_fn jit_let_collect_bindings, %rax
        call_scm append, %rax, env(%rsp)
        mov    %rax, env(%rsp)

        cdr     %rbx, %rsi
        call_scm cons, begin_symbol, %rsi
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        return  max_locals(%rsp)

jit_is_lambda:                  # form
        minimal_prologue
        is_nil_internal %rdi
        je      2f
        has_tag TAG_PAIR, %rdi, store=false
        jne     2f
        car     %rdi
        cmp     lambda_symbol, %rax
        jne     1f
        return  $C_TRUE
1:      eq_internal lambda_internal_symbol, %rax
        return
2:      return  $C_FALSE

jit_letrec:                     # form, c-stream, environment, register, tail
        prologue form, env, full_env, max_locals, register, tail, closure_env, lambda
        cdr     %rdi
        mov     %rax, form(%rsp)
        mov     %rax, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        car     %rbx, %rbx
        call_fn jit_let_collect_bindings, %rbx
        call_scm append, %rax, env(%rsp)
        mov    %rax, full_env(%rsp)

        call_fn jit_let_bindings %rbx, %r12, env(%rsp), full_env(%rsp), $C_TRUE
        call_fn jit_let_bindings %rbx, %r12, env(%rsp), full_env(%rsp), $C_FALSE
        update_max_locals max_locals(%rsp)

1:      is_nil_internal %rbx
        je      3f

        car     %rbx
        cdr     %rax
        car     %rax
        call_fn jit_is_lambda, %rax
        cmp     $C_TRUE, %rax
        jne     2f

        car      %rbx
        car      %rax
        call_fn jit_datum, %rax, %r12, full_env(%rsp), $RDI, $C_FALSE

        car     %rbx
        cdr     %rax
        car     %rax
        mov     %rax, lambda(%rsp)

        cdr     %rax
        car     %rax
        call_fn jit_lambda_flatten_arguments, %rax
        call_fn jit_lambda_closure_environment_bitmask, lambda(%rsp), full_env(%rsp), %rax, $0
        test    %rax, %rax
        jz      2f

        call_fn jit_literal, %rax, %r12, $NIL, $RSI, $C_FALSE
        call_fn jit_literal, $jit_rt_lambda_patch_factory, %r12, $NIL, $RAX, $C_FALSE
        call_fn fwrite, $jit_call_rax, $1, jit_call_rax_size, %r12

2:      cdr     %rbx, %rbx
        jmp     1b

3:      call_scm cdr, form(%rsp)
        call_scm cons, begin_symbol, %rax
        call_fn jit_datum, %rax, %r12, full_env(%rsp), register(%rsp), tail(%rsp)
        update_max_locals max_locals(%rsp)
        return  max_locals(%rsp)

        ## 4.2.3. Sequencing

jit_begin:                     # form, c-stream, environment, register, tail
        prologue env, max_locals, register, tail
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        mov     %rcx, register(%rsp)
        mov     %r8, tail(%rsp)
        movq    $0, max_locals(%rsp)

        cdr     %rbx, %rbx
        is_nil_internal %rbx
        jne     2f

        call_scm cons, $VOID, $NIL
        mov     %rax, %rbx

2:      is_nil_internal %rbx
        je      5f

        cdr     %rbx
        is_nil_internal %rax
        je      3f

        car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), $C_FALSE
        jmp     4f

3:      car     %rbx
        call_fn jit_datum, %rax, %r12, env(%rsp), register(%rsp), tail(%rsp)

4:      update_max_locals max_locals(%rsp)
        cdr     %rbx, %rbx
        jmp     2b

5:      return  max_locals(%rsp)

        ## 5.4. Syntax definitions

jit_define_syntax:              # form, c-stream, environment, register, tail
        prologue env, max_locals, syntax_address
        mov     %rdi, %rbx
        mov     %rsi, %r12
        mov     %rdx, env(%rsp)
        movq    $0, max_locals(%rsp)

        cdr     %rbx, %rbx

        cdr     %rbx
        car     %rax
        call_fn jit_datum, %rax, %r12, env(%rsp), $RAX, $C_FALSE
        update_max_locals max_locals(%rsp)

        car     %rbx
        lea     jit_syntax_jump_table(,%eax,POINTER_SIZE), %rax
        mov     %rax, syntax_address(%rsp)

        call_fn fwrite, $jit_rax_to_global, $1, jit_rax_to_global_size, %r12
        lea     syntax_address(%rsp), %rax
        call_fn fwrite, %rax, $1, $POINTER_SIZE, %r12
        call_fn fwrite, $jit_void_to_rax, $1, jit_void_to_rax_size, %r12
        return  max_locals(%rsp)

        ## 6.10. Control features

jit_call_with_current_continuation_escape_factory: # continuation
        prologue  code, size
        mov     %rdi, %r12
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %rbx

        call_fn jit_literal, %r12, %rbx, $NIL, $R10, $C_FALSE
        call_fn jit_literal, $jit_rt_call_with_current_continuation_escape, %rbx, $NIL, $R11, $C_FALSE
        call_fn fwrite, $jit_jump_r11, $1, jit_jump_r11_size, %rbx

        call_fn fclose, %rbx
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        mov     %rax, %rbx
        call_fn free, code(%rsp)
        return  %rbx

        ## JIT Runtime Support

jit_rt_symbol_not_defined:      # symbol
        minimal_prologue
        call_scm internal_error, symbol_not_defined_string, %rdi
        return

jit_rt_lambda_factory:          # lambda, closure-bitmask
        prologue code, size, closure_bitmask
        mov     %rdi, %rbx
        mov     %rsi, closure_bitmask(%rsp)
        lea     code(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %r12

        call_fn jit_lambda_factory_code, %rbx, %r12, closure_bitmask(%rsp)
        call_fn fclose, %r12
        perror  je

        mov     size(%rsp), %r11d
        call_fn jit_allocate_code, code(%rsp), %r11
        mov     %rax, %rbx
        call_fn free, code(%rsp)
        tag     TAG_PROCEDURE, %rbx
        return

jit_rt_lambda_patch_factory:    # lambda-factory, closure-bitmask
        prologue closure_bitmask, lambda, patch_code, patch_size
        unbox_pointer_internal %rdi, %rbx
        mov     %rsi, closure_bitmask(%rsp)

        mov     %rbx, %rax
        add     jit_literal_to_rax_size, %rax
        mov     (%rax), %rax
        mov     %rax, lambda(%rsp)

        lea     patch_code(%rsp), %rdi
        lea     patch_size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        perror
        mov     %rax, %r12

        call_fn jit_lambda_factory_code, lambda(%rsp), %r12, closure_bitmask(%rsp)
        call_fn fclose, %r12
        perror  je

        mov     patch_size(%rsp), %r11d
        call_fn memcpy, %rbx, patch_code(%rsp), %r11
        perror
        call_fn free, patch_code(%rsp)
        tag     TAG_PROCEDURE, %rbx
        return

jit_rt_lambda_arity_check_error: # arity in al, expected-arity in r10b
        minimal_prologue
        mov     %rax, %rdi
        mov     %r10d, %esi

        box_int_internal %edi
        mov     %rax, %rdi
        box_int_internal %esi
        mov     %rax, %rsi

        call_scm internal_error, arity_check_error_string, %rdi, %rsi
        return

jit_rt_lambda_collect_varargs:  # arity in rax, varargs-idx in r10
        .irp reg, rbx, r12, r13, r14
        push    %\reg
        .endr

        mov     %rax, %r13
        mov     %r10, %r14

        .irp reg, rdi, rsi, rdx, rcx, r8, r9
        push    %\reg
        .endr

        mov     $NIL, %rbx
        mov     %r13, %r12
        sub     $MAX_REGISTER_ARGS, %r12
        mov     $NIL, %rbx

1:      cmp     $0, %r12d
        jle     2f

        mov     %r12, %rcx
        inc     %rcx
        mov     (%rbp,%rcx,POINTER_SIZE), %rdi

        call_scm cons, %rdi, %rbx
        mov     %rax, %rbx

        dec     %r12d
        jmp     1b

2:      .irp reg, r9, r8, rcx, rdx, rsi, rdi
        pop    %\reg
        .endr

        .irp reg, rdi, rsi, rdx, rcx, r8, r9
        push    %\reg
        .endr

        mov     %r13, %r12
        sub     %r14, %r12

        mov     %r14, %r11
        shl     $VARARGS_JUMP_ALIGNMENT_SHIFT, %r11
        add     $varargs_load, %r11
        jmp     *%r11

        .align  VARARGS_JUMP_ALIGNMENT
varargs_load:
        .irp    reg, rdi, rsi, rdx, rcx, r8, r9
        test    %r12d, %r12d
        jz      3f
        push    %\reg
        dec     %r12d
        .align  VARARGS_JUMP_ALIGNMENT
        .endr

3:      mov     %r14, %r12

4:      cmp     $MAX_REGISTER_ARGS, %r12
        je      5f
        cmp     %r13, %r12
        je      5f

        pop     %rdi
        call_scm cons, %rdi, %rbx
        mov     %rax, %rbx

        inc     %r12d
        jmp     4b

5:      .irp reg, r9, r8, rcx, rdx, rsi, rdi
        pop    %\reg
        .endr

        mov     %r14, %r11
        shl     $VARARGS_JUMP_ALIGNMENT_SHIFT, %r11
        add     $varargs_store, %r11
        jmp     *%r11

        .align  VARARGS_JUMP_ALIGNMENT
varargs_store:
        .irp    reg, rdi, rsi, rdx, rcx, r8, r9
        mov     %rbx, %\reg
        jmp     6f
        .align  VARARGS_JUMP_ALIGNMENT
        .endr

6:      .irp reg, r14, r13, r12, rbx
        pop    %\reg
        .endr
        ret

jit_rt_call_with_current_continuation_execute_dynamic_extent: # dynamic-extent
        prologue
        mov     %rdi, %rbx
1:      is_nil_internal %rbx
        je      2f
        car     %rbx
        car     %rax
        unbox_pointer_internal %rax, %r11
        call_scm *%r11
        cdr     %rbx, %rbx
        jmp     1b
2:      return

jit_rt_call_with_current_continuation_escape: # obj ..., continuation in r10
        arity_check 1, jge
        mov     %r10, %rbx
        mov     $1, %r10d
        push    %rbp
        mov     %rsp, %rbp
        call_fn jit_rt_lambda_collect_varargs
        pop     %rbp
        mov     %rbx, %r10
        mov     %rdi, %rbx
        mov     %rsi, %r12

        push    %r10
        ## TODO: this should probably just exit as far as necessary.
        call_fn exit_dynamic_extent
        pop     %r10

1:      unbox_pointer_internal %r10, %r11
        lea     header_size(%r11), %rsi

        mov     header_object_size(%r11), %edx
        sub     $(CONTINUATION_SAVED_VALUES * POINTER_SIZE), %edx
        mov     execution_stack_top, %rsp
        sub     %rdx, %rsp
        mov     %rsp, %rdi

        push    %rbx
        push    %r12

        mov     $CONTINUATION_SAVED_VALUES, %ecx
2:      test    %ecx, %ecx
        jz      3f
        pushq   (%rsi)
        add     $POINTER_SIZE, %rsi
        dec     %ecx
        jmp     2b

3:      call_fn memcpy, %rdi, %rsi, %rdx

        pop     %rbp
        pop     %r12
        pop     %rbx

        pop     %rdi
        call_fn jit_rt_call_with_current_continuation_execute_dynamic_extent, %rdi

        pop     %rdx
        pop     %rax
        ret

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
box_jump_table:
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
jit_inline_table:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE

        .align  16
jit_inline_size_table:
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
segv_jmp_buffer:
        .quad   0

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
        .string "%a[^][ \f\n\r\t\v()\";#]"
char_format:
        .string "%c"
machine_readable_char_format:
        .string "#\\%c"
machine_readable_escape_code_format:
        .string "\\%c"
pointer_format:
        .string "%lx\n"
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
cpuid_error:
        .string "No SSE4.2 support.\n"
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
jit_epilogue_size:
        .quad   . - jit_epilogue

        .align  16
jit_arity_check_r10b_with_al:
        cmp     %r10b, %al
        je      1f
        mov     $jit_rt_lambda_arity_check_error, %r11
        call   *%r11
1:
jit_arity_check_r10b_with_al_size:
        .quad   (. - jit_arity_check_r10b_with_al)

        .align  16
jit_varargs_arity_check_r10b_with_al:
        cmp     %r10b, %al
        jge     1f
        mov     $jit_rt_lambda_arity_check_error, %r11
        call   *%r11
1:
jit_varargs_arity_check_r10b_with_al_size:
        .quad   (. - jit_varargs_arity_check_r10b_with_al)

        .align  16
jit_clear_multiple_returns_in_rdx:
        xor     %edx, %edx
jit_clear_multiple_returns_in_rdx_size:
        .quad   . - jit_clear_multiple_returns_in_rdx

        .align  16
jit_return:
        ret
jit_return_size:
        .quad   . - jit_return

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
        .align  16
jit_literal_to_\reg\():
        mov     $0x1122334455667788, %\reg
jit_literal_to_\reg\()_size:
        .quad   (. - jit_literal_to_\reg - POINTER_SIZE)
        .endr

        .align  16
jit_rax_is_false_jump:
        mov     $FALSE, %r11
        cmp     %rax, %r11
        je      0
jit_rax_is_false_jump_size:
        .quad   (. - jit_rax_is_false_jump)

        .align  16
jit_jump:
        jmp     0
jit_jump_size:
        .quad   (. - jit_jump)

        .irp reg, rax, r10, r11
        .align  16
jit_push_\reg\():
        push    %\reg
jit_push_\reg\()_size:
        .quad   . - jit_push_\reg

        .align  16
jit_unbox_\reg\():
        unbox_pointer_internal %\reg, %\reg
jit_unbox_\reg\()_size:
        .quad   . - jit_unbox_\reg

        .align  16
jit_call_\reg\():
        call    *%\reg
jit_call_\reg\()_size:
        .quad   . - jit_call_\reg

        .align  16
jit_jump_\reg\():
        jmp     *%\reg
jit_jump_\reg\()_size:
        .quad   (. - jit_jump_\reg\())
        .endr

        .align  16
jit_caller_cleanup:
        add     $0x11223344, %rsp
jit_caller_cleanup_size:
        .quad   (. - jit_caller_cleanup) - INT_SIZE

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9, r11
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

        .irp reg, rdi, rsi, rdx, rcx, r8, r9, r11
        .align  16
jit_rax_to_\reg\():
        mov     %rax, %\reg
jit_rax_to_\reg\()_size:
        .quad   . - jit_rax_to_\reg
        .endr

        .align  16
jit_void_to_rax:
        mov     $VOID, %rax
jit_void_to_rax_size:
        .quad   . - jit_void_to_rax

        .align  16
jit_global_symbol_in_rax_to_rax:
        mov     symbol_table_values(,%eax,POINTER_SIZE), %r10
        is_void_internal %r10, store=false
        jne     1f
        mov     $jit_rt_symbol_not_defined, %r11
        call_fn *%r11, %rax
1:      mov     %r10, %rax
jit_global_symbol_in_rax_to_rax_size:
        .quad   (. - jit_global_symbol_in_rax_to_rax)

        .align  16
jit_rax_to_global:
        mov     %rax, 0x1122334455667788
jit_rax_to_global_size:
        .quad   (. - jit_rax_to_global) - POINTER_SIZE

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9, r11
        .align  16
jit_local_to_\reg\():
        mov     -0x11223344(%rbp), %\reg
jit_local_to_\reg\()_size:
        .quad   (. - jit_local_to_\reg) - INT_SIZE
        .endr

        .irp reg, rax, rdi, rsi, rdx, rcx, r8, r9, r11
        .align  16
jit_\reg\()_to_local:
        mov     %\reg, -0x11223344(%rbp)
jit_\reg\()_to_local_size:
        .quad   (. - jit_\reg\()_to_local) - INT_SIZE
        .endr

        .align  16
jit_r11_to_closure:
        mov     %r11, -0x11223344(%rsp)
jit_r11_to_closure_size:
        .quad   (. - jit_r11_to_closure) - INT_SIZE

        .align  16
jit_arity_to_al:
        xor     %eax, %eax
        mov     $0x11, %al
jit_arity_to_al_size:
        .quad   (. - jit_arity_to_al) - BYTE_SIZE

        .irp file, boot, r7rs, init
        .align  16
\file\()_scm:
        .incbin "\file\().scm"
        .byte   0
        .endr
