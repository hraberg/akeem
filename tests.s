        .include "macros.s"

        .data
empty_string:
        .string ""
PI:
        .double 3.14159
E:
        .double 2.71828
ZERO:
        .double 0.0
PLUS_ONE:
        .double 1.0
MINUS_ONE:
        .double -1.0
MINUS_TWO:
        .double -2.0
FIVE:
        .double 5.0
NAN:
        .quad NAN_MASK

forty_two_string:
        .string "42"
pi_string:
        .string "3.14159"
foo_name:
        .string "foo"
strlen_name:
        .string "strlen"
allocate_code_name:
        .string "allocate_code"

println:
        prologue
        call_fn display, %rdi
        call_fn newline
        return

example_code:
        mov     %rdi, %rax
        add     $4, %rax
        ret
        .equ example_code_size, (. - example_code)
        assert_equals 8, example_code_size

        .text
main:
        prologue vec
        call_fn init_runtime

        call_fn box_string, $strlen_name
        call_fn string_to_symbol, %rax
        call_fn println, %rax

        call_fn dlsym, $RTLD_DEFAULT, $strlen_name
        call_fn *%rax, $int_format
        call_fn box_int, %rax
        call_fn println, %rax

        call_fn dlsym, $RTLD_DEFAULT, $allocate_code_name
        call_fn *%rax, $example_code, $example_code_size
        call_fn *%rax, $2
        call_fn box_int, %rax
        call_fn println, %rax

        call_fn cons, $1, $NIL
        call_fn is_pair, %rax
        call_fn println, %rax

        call_fn cons, $1, $2
        call_fn is_pair, %rax
        call_fn println, %rax

        call_fn is_pair, $NIL
        call_fn println, %rax

        call_fn is_pair, PI
        call_fn println, %rax

        call_fn cons, $1, $NIL
        call_fn is_exact, %rax
        call_fn println, %rax

        call_fn box_int, $3
        call_fn cons, %rax, $NIL
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn cons, %rax, %rbx
        mov     %rax, %rbx
        call_fn box_int, $1
        call_fn cons, %rax, %rbx
        call_fn println, %rax

        call_fn cons, $3, $NIL
        call_fn cons, $2, %rax
        call_fn cons, $1, %rax
        call_fn length, %rax
        call_fn println, %rax

        call_fn length, $NIL
        call_fn println, %rax

        call_fn box_int, $2
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn cons, %rax, %rbx
        call_fn println, %rax

        call_fn box_int, $42
        call_fn println, %rax

        call_fn box_int, $3
        call_fn println, %rax

        call_fn box_int, $1
        call_fn is_exact, %rax
        call_fn println, %rax

        call_fn box_int, $1
        call_fn is_boolean, %rax
        call_fn println, %rax

        call_fn box_string, $strlen_name
        call_fn is_string, %rax
        call_fn println, %rax

        call_fn box_string, $strlen_name
        call_fn string_length, %rax
        call_fn println, %rax

        call_fn box_string, $strlen_name
        mov     %rax, %rbx
        call_fn box_int $0
        call_fn string_ref, %rbx, %rax
        mov     %rax, %rbx
        call_fn println, %rax

        call_fn char_to_integer, %rbx
        call_fn println, %rax

        call_fn is_char, %rbx
        call_fn println, %rax

        call_fn is_char, $NIL
        call_fn println, %rax

        call_fn box_int, $65
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn box_int, $8
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn box_int, $9
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn box_int, $10
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn box_int, $13
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn box_int, $32
        call_fn integer_to_char, %rax
        call_fn println, %rax

        call_fn is_string, PI
        call_fn println, %rax

        call_fn println, $TRUE
        call_fn println, $FALSE

        call_fn is_boolean, $TRUE
        call_fn println, %rax

        call_fn is_boolean, $FALSE
        call_fn println, %rax

        call_fn is_symbol, $NIL
        call_fn println, %rax

        call_fn println, $NIL
        call_fn println, $TRUE

        call_fn is_null, $NIL
        call_fn println, %rax

        call_fn is_null, PI
        call_fn println, %rax

        call_fn is_inexact, PI
        call_fn println, %rax

        call_fn is_inexact, $TRUE
        call_fn println, %rax

        call_fn is_eq, $TRUE, $TRUE
        call_fn println, %rax

        call_fn is_eq, E, PI
        call_fn println, %rax

        call_fn box_int, $42
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn is_inexact, %rax
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn is_exact, %rax
        call_fn println, %rax

        call_fn box_int, $0
        call_fn println, %rax
        call_fn println, ZERO

        call_fn println, PI
        call_fn println, NAN

        call_fn box_string, $strlen_name
        call_fn println, %rax

        call_fn unbox, $TRUE
        mov     %rax, %rdi
        xor     %al, %al
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, $FALSE
        mov     %rax, %rdi
        xor     %al, %al
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, $NIL
        mov     %rax, %rdi
        xor     %al, %al
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn box_int, $-1
        call_fn unbox, %rax
        mov     %rax, %rdi
        xor     %al, %al
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, PI
        movq    %rax, %xmm0
        mov     $1, %al
        mov     $double_format, %rdi
        call    printf
        call_fn puts, $empty_string

        call_fn box_int, $2
        call_fn make_vector, %rax
        mov     %rax, vec(%rsp)

        call_fn vector_length, %rax
        call_fn println, %rax

        call_fn is_vector, vec(%rsp)
        call_fn println, %rax

        call_fn is_vector, $NIL
        call_fn println, %rax

        call_fn box_int, $16
        call_fn box_int, $0
        call_fn vector_set, vec(%rsp), %rax, E
        call_fn box_int, $1
        call_fn vector_set, vec(%rsp), %rax, PI

        call_fn box_int, $0
        call_fn vector_ref, vec(%rsp), %rax
        call_fn println, %rax
        call_fn box_int, $1
        call_fn vector_ref, vec(%rsp), %rax
        call_fn println, %rax

        call_fn println, vec(%rsp)

        call_fn box_int, $1
        call_fn neg, %rax
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn neg, %rax
        call_fn println, %rax

        call_fn neg, PLUS_ONE
        call_fn println, %rax

        call_fn neg, MINUS_ONE
        call_fn println, %rax

        call_fn is_inexact, PLUS_ONE
        call_fn println, %rax
        call_fn is_inexact, MINUS_ONE
        call_fn println, %rax
        call_fn is_inexact, NAN
        call_fn println, %rax

        call_fn box_int, $1
        call_fn exact_to_inexact, %rax
        call_fn println %rax

        call_fn inexact_to_exact, PI
        call_fn println %rax

        call_fn is_number, PLUS_ONE
        call_fn println, %rax

        call_fn is_number, $1
        call_fn println, %rax

        call_fn is_number, $TRUE
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn plus, %rax, %r11
        call_fn println, %rax

        call_fn box_int, $4
        mov     %rax, %rbx
        call_fn box_int, $-8
        call_fn plus, %rax, %rbx
        call_fn println, %rax

        call_fn plus, PI, E
        call_fn println, %rax

        call_fn box_int, $1
        call_fn plus, %rax, PI
        call_fn println, %rax

        call_fn box_int, $2
        call_fn plus, PI, %rax
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn minus, %rax, %r11
        call_fn println, %rax

        call_fn minus, E, PI
        call_fn println, %rax

        call_fn box_int, $1
        call_fn minus, PI, %rax
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn minus, %rax, MINUS_ONE
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn multiply, %rax, %r11
        call_fn println, %rax

        call_fn multiply, E, PI
        call_fn println, %rax

        call_fn box_int, $42
        call_fn multiply, MINUS_ONE, %rax
        call_fn println, %rax

        call_fn box_int, $2
        call_fn multiply, %rax, E
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn divide, %rbx, %rax
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %rbx
        call_fn box_int, $-2
        call_fn divide, %rbx, %rax
        call_fn println, %rax

        call_fn box_int, $2
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn divide, %rbx, %rax
        call_fn println, %rax

        call_fn divide, E, PI
        call_fn println, %rax

        call_fn box_int, $2
        call_fn divide, MINUS_ONE, %rax
        call_fn println, %rax

        call_fn box_int, $42
        call_fn divide, %rax, PI
        call_fn println, %rax

        call_fn box_int, $42
        call_fn equal, %rax, %rax
        call_fn println, %rax

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn equal, %rax, %rbx
        call_fn println, %rax

        call_fn box_int, $42
        call_fn equal, %rax, PI
        call_fn println, %rax

        call_fn box_int, $42
        call_fn equal, PI, %rax
        call_fn println, %rax

        call_fn equal, PI, PI
        call_fn println, %rax

        call_fn equal, PI, E
        call_fn println, %rax

        call_fn box_int $1
        call_fn equal, %rax, PLUS_ONE
        call_fn println, %rax

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn less_than, %rax, %rbx
        call_fn println, %rax

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn greater_than, %rax, %rbx
        call_fn println, %rax

        call_fn greater_than PI, E
        call_fn println %rax

        call_fn less_than E, PI
        call_fn println %rax

        call_fn less_than_or_equal PI, PI
        call_fn println %rax

        call_fn greater_than_or_equal E, PI
        call_fn println %rax

        call_fn box_int, $1
        call_fn less_than %rax, PI
        call_fn println %rax

        call_fn box_int, $-1
        mov     %rax, %rbx
        call_fn box_int $1
        call_fn less_than_or_equal %rbx, %rax
        call_fn println %rax

        call_fn box_int, $-1
        call_fn less_than %rax, PI
        call_fn println %rax

        call_fn greater_than PI, MINUS_ONE
        call_fn println %rax

        call_fn less_than_or_equal MINUS_ONE, PLUS_ONE
        call_fn println %rax

        call_fn box_int, $2
        call_fn greater_than E, %rax
        call_fn println %rax

        call_fn floor_, PI
        call_fn println %rax

        call_fn box_int, $1
        call_fn floor_, %rax
        call_fn println %rax

        call_fn sqrt_, PI
        call_fn println %rax

        call_fn box_int, $2
        call_fn sqrt_, %rax
        call_fn println %rax

        call_fn box_int, $4
        call_fn sqrt_, %rax
        call_fn println %rax

        call_fn box_int, $2
        call_fn expt, %rax, PI
        call_fn println %rax

        call_fn box_int, $5
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn expt, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $5
        mov     %rax, %rbx
        call_fn box_int $2
        call_fn quotient, %rbx, %rax
        call_fn println %rax

        call_fn box_int, $5
        call_fn quotient, %rax, E
        call_fn println %rax

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn modulo, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn remainder, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn modulo, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn remainder, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn modulo, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int, $-4
        call_fn remainder, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn modulo, %rax, %rbx
        call_fn println %rax

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn remainder, %rax, %rbx
        call_fn println %rax

        call_fn remainder, FIVE, MINUS_TWO
        call_fn println %rax

        call_fn string_to_symbol, $foo_name
        mov     %rax, %rbx
        call_fn set, %rbx, PI

        call_fn lookup_global_symbol, %rbx
        call_fn println, %rax

        call_fn set, %rbx, $TRUE
        call_fn lookup_global_symbol, %rbx
        call_fn println, %rax

        call_fn box_string, $forty_two_string
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_exact, %rax
        call_fn println, %rax
        call_fn println, %rbx

        call_fn box_string, $pi_string
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_inexact, %rax
        call_fn println, %rax
        call_fn println, %rbx

        call_fn box_string, $foo_name
        call_fn string_to_number, %rax
        call_fn println, %rax

        return  $0

        .globl main
