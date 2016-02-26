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
assertion_failed_format:
        .string "expected: '%s' but was: '%s'\n"
test_case_prefix:
        .string ";;; "

        .macro assert_equals expected actual
        .if (\expected != \actual)
        .error "Assertion failed: \expected \actual"
        .endif
        .endm

        .macro test_case str
        .data
tmp_string_\@:
        .string "\str"
        .text
        call_fn fprintf, stderr, $tmp_string_\@
        call_fn fputc, $'\n, stderr

        call_fn printf, $test_case_prefix
        call_fn puts, $tmp_string_\@
        .endm

        .macro assert value=%rax
        call_fn display, \value
        call_fn newline
        .endm

        .macro is expected, actual=%rax
        .data
test_string_\@:
        .string "\expected"
        .text
        call_fn to_string, \actual
        call_fn unbox %rax
        mov     %rax, %rbx
        call_fn strcmp %rax, $test_string_\@
        jz      1f
        call_fn printf, $assertion_failed_format, $test_string_\@, %rbx
        call_fn exit, $1
1:      nop
        .endm


example_code:
        mov     %rdi, %rax
        add     $4, %rax
        ret
        .equ example_code_size, (. - example_code)
        assert_equals 8, example_code_size

        .text
main:
        prologue vec

        test_case "test suite start"

        call_fn init_runtime

        call_fn box_string, $strlen_name
        call_fn string_to_symbol, %rax
        assert

        call_fn dlsym, $RTLD_DEFAULT, $strlen_name
        call_fn *%rax, $int_format
        call_fn box_int, %rax
        assert

        call_fn dlsym, $RTLD_DEFAULT, $allocate_code_name
        call_fn *%rax, $example_code, $example_code_size
        call_fn *%rax, $2
        call_fn box_int, %rax
        assert

        call_fn cons, $1, $NIL
        call_fn is_pair, %rax
        assert

        call_fn cons, $1, $2
        call_fn is_pair, %rax
        assert

        call_fn is_pair, $NIL
        assert

        call_fn is_pair, PI
        assert

        call_fn cons, $1, $NIL
        call_fn is_exact, %rax
        assert

        call_fn box_int, $3
        call_fn cons, %rax, $NIL
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn cons, %rax, %rbx
        mov     %rax, %rbx
        call_fn box_int, $1
        call_fn cons, %rax, %rbx
        assert

        call_fn cons, $3, $NIL
        call_fn cons, $2, %rax
        call_fn cons, $1, %rax
        call_fn length, %rax
        assert

        call_fn length, $NIL
        assert

        call_fn box_int, $2
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn cons, %rax, %rbx
        assert

        call_fn box_int, $42
        assert

        call_fn box_int, $3
        assert

        call_fn box_int, $1
        call_fn is_exact, %rax
        assert

        call_fn box_int, $1
        call_fn is_boolean, %rax
        assert

        call_fn box_string, $strlen_name
        call_fn is_string, %rax
        assert

        call_fn box_string, $strlen_name
        call_fn string_length, %rax
        assert

        call_fn box_string, $strlen_name
        mov     %rax, %rbx
        call_fn box_int $0
        call_fn string_ref, %rbx, %rax
        mov     %rax, %rbx
        assert

        call_fn char_to_integer, %rbx
        assert

        call_fn is_char, %rbx
        assert

        call_fn is_char, $NIL
        assert

        call_fn box_int, $65
        call_fn integer_to_char, %rax
        assert

        call_fn box_int, $8
        call_fn integer_to_char, %rax
        assert

        call_fn box_int, $9
        call_fn integer_to_char, %rax
        assert

        call_fn box_int, $10
        call_fn integer_to_char, %rax
        assert

        call_fn box_int, $13
        call_fn integer_to_char, %rax
        assert

        call_fn box_int, $32
        call_fn integer_to_char, %rax
        assert

        call_fn is_string, PI
        assert

        assert $TRUE
        assert $FALSE

        call_fn is_boolean, $TRUE
        assert

        call_fn is_boolean, $FALSE
        assert

        call_fn is_symbol, $NIL
        assert

        assert $NIL
        assert $TRUE

        call_fn is_null, $NIL
        assert

        call_fn is_null, PI
        assert

        call_fn is_inexact, PI
        assert

        call_fn is_inexact, $TRUE
        assert

        call_fn is_eq, $TRUE, $TRUE
        assert

        call_fn is_eq, E, PI
        assert

        call_fn box_int, $42
        assert

        call_fn box_int, $-1
        assert

        call_fn box_int, $-1
        call_fn is_inexact, %rax
        assert

        call_fn box_int, $-1
        call_fn is_exact, %rax
        assert

        call_fn box_int, $0
        assert
        assert ZERO

        assert PI
        assert NAN

        call_fn box_string, $strlen_name
        assert

        call_fn unbox, $TRUE
        call_fn box_int, %rax
        assert

        call_fn unbox, $FALSE
        call_fn box_int, %rax
        assert

        call_fn unbox, $NIL
        call_fn box_int, %rax
        assert

        call_fn box_int, $-1
        call_fn unbox, %rax
        call_fn box_int, %rax
        assert

        call_fn unbox, PI
        movq    %rax, %xmm0
        mov     $1, %al
        mov     $double_format, %rdi
        call    printf
        call_fn putchar, $'\n

        call_fn box_int, $2
        call_fn make_vector, %rax
        mov     %rax, vec(%rsp)

        call_fn vector_length, %rax
        assert

        call_fn is_vector, vec(%rsp)
        assert

        call_fn is_vector, $NIL
        assert

        call_fn box_int, $16
        call_fn box_int, $0
        call_fn vector_set, vec(%rsp), %rax, E
        call_fn box_int, $1
        call_fn vector_set, vec(%rsp), %rax, PI

        call_fn box_int, $0
        call_fn vector_ref, vec(%rsp), %rax
        assert
        call_fn box_int, $1
        call_fn vector_ref, vec(%rsp), %rax
        assert

        assert vec(%rsp)

        call_fn box_int, $4
        call_fn make_vector, %rax, PI
        mov     %rax, vec(%rsp)

        call_fn vector_length, %rax
        assert

        call_fn box_int, $0
        call_fn vector_ref, vec(%rsp), %rax
        assert

        call_fn box_int, $3
        call_fn vector_ref, vec(%rsp), %rax
        assert

        call_fn box_int, $4
        call_fn make_string, %rax, $'A
        mov     %rax, %rbx

        call_fn string_length, %rax
        assert

        call_fn box_int, $0
        call_fn string_ref, %rbx, %rax
        assert

        call_fn box_int, $3
        call_fn string_ref, %rbx, %rax
        assert

        call_fn box_int, $1
        call_fn neg, %rax
        assert

        call_fn box_int, $-1
        call_fn neg, %rax
        assert

        call_fn neg, PLUS_ONE
        assert

        call_fn neg, MINUS_ONE
        assert

        call_fn is_inexact, PLUS_ONE
        assert
        call_fn is_inexact, MINUS_ONE
        assert
        call_fn is_inexact, NAN
        assert

        call_fn box_int, $1
        call_fn exact_to_inexact, %rax
        assert

        call_fn inexact_to_exact, PI
        assert

        call_fn is_number, PLUS_ONE
        assert

        call_fn is_number, $1
        assert

        call_fn is_number, $TRUE
        assert

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn plus, %rax, %r11
        assert

        call_fn box_int, $4
        mov     %rax, %rbx
        call_fn box_int, $-8
        call_fn plus, %rax, %rbx
        assert

        call_fn plus, PI, E
        assert

        call_fn box_int, $1
        call_fn plus, %rax, PI
        assert

        call_fn box_int, $2
        call_fn plus, PI, %rax
        assert

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn minus, %rax, %r11
        assert

        call_fn minus, E, PI
        assert

        call_fn box_int, $1
        call_fn minus, PI, %rax
        assert

        call_fn box_int, $-1
        call_fn minus, %rax, MINUS_ONE
        assert

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn multiply, %rax, %r11
        assert

        call_fn multiply, E, PI
        assert

        call_fn box_int, $42
        call_fn multiply, MINUS_ONE, %rax
        assert

        call_fn box_int, $2
        call_fn multiply, %rax, E
        assert

        call_fn box_int, $16
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn divide, %rbx, %rax
        assert

        call_fn box_int, $16
        mov     %rax, %rbx
        call_fn box_int, $-2
        call_fn divide, %rbx, %rax
        assert

        call_fn box_int, $2
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn divide, %rbx, %rax
        assert

        call_fn divide, E, PI
        assert

        call_fn box_int, $2
        call_fn divide, MINUS_ONE, %rax
        assert

        call_fn box_int, $42
        call_fn divide, %rax, PI
        assert

        call_fn box_int, $42
        call_fn equal, %rax, %rax
        assert

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn equal, %rax, %rbx
        assert

        call_fn box_int, $42
        call_fn equal, %rax, PI
        assert

        call_fn box_int, $42
        call_fn equal, PI, %rax
        assert

        call_fn equal, PI, PI
        assert

        call_fn equal, PI, E
        assert

        call_fn box_int $1
        call_fn equal, %rax, PLUS_ONE
        assert

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn less_than, %rax, %rbx
        assert

        call_fn box_int, $42
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn greater_than, %rax, %rbx
        assert

        call_fn greater_than PI, E
        assert

        call_fn less_than E, PI
        assert

        call_fn less_than_or_equal PI, PI
        assert

        call_fn greater_than_or_equal E, PI
        assert

        call_fn box_int, $1
        call_fn less_than %rax, PI
        assert

        call_fn box_int, $-1
        mov     %rax, %rbx
        call_fn box_int $1
        call_fn less_than_or_equal %rbx, %rax
        assert

        call_fn box_int, $-1
        call_fn less_than %rax, PI
        assert

        call_fn greater_than PI, MINUS_ONE
        assert

        call_fn less_than_or_equal MINUS_ONE, PLUS_ONE
        assert

        call_fn box_int, $2
        call_fn greater_than E, %rax
        assert

        call_fn floor_, PI
        assert

        call_fn box_int, $1
        call_fn floor_, %rax
        assert

        call_fn sqrt_, PI
        assert

        call_fn box_int, $2
        call_fn sqrt_, %rax
        assert

        call_fn box_int, $4
        call_fn sqrt_, %rax
        assert

        call_fn box_int, $2
        call_fn expt, %rax, PI
        assert

        call_fn box_int, $5
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn expt, %rax, %rbx
        assert

        call_fn box_int, $5
        mov     %rax, %rbx
        call_fn box_int $2
        call_fn quotient, %rbx, %rax
        assert

        call_fn box_int, $5
        call_fn quotient, %rax, E
        assert

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn modulo, %rax, %rbx
        assert

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn remainder, %rax, %rbx
        assert

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $4
        call_fn modulo, %rax, %rbx
        assert

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int, $4
        call_fn remainder, %rax, %rbx
        assert

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn modulo, %rax, %rbx
        assert

        call_fn box_int, $3
        mov     %rax, %rbx
        call_fn box_int, $-4
        call_fn remainder, %rax, %rbx
        assert

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn modulo, %rax, %rbx
        assert

        call_fn box_int, $-3
        mov     %rax, %rbx
        call_fn box_int $-4
        call_fn remainder, %rax, %rbx
        assert

        call_fn remainder, FIVE, MINUS_TWO
        assert

        call_fn string_to_symbol, $foo_name
        mov     %rax, %rbx
        call_fn set, %rbx, PI

        call_fn lookup_global_symbol, %rbx
        assert

        call_fn set, %rbx, $TRUE
        call_fn lookup_global_symbol, %rbx
        assert

        call_fn box_string, $forty_two_string
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_exact, %rax
        assert
        assert %rbx

        call_fn box_string, $forty_two_string
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn string_to_number, %rbx, %rax
        assert

        call_fn box_string, $pi_string
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_inexact, %rax
        assert
        assert %rbx

        call_fn box_string, $foo_name
        call_fn string_to_number, %rax
        assert

        call_fn box_int, $10
        call_fn number_to_string, %rax
        assert

        call_fn box_int, $10
        mov     %rax, %rbx
        call_fn box_int, $8
        call_fn number_to_string, %rbx, %rax
        assert

        call_fn box_int, $10
        mov     %rax, %rbx
        call_fn box_int, $10
        call_fn number_to_string, %rbx, %rax
        assert

        call_fn box_int, $10
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn number_to_string, %rbx, %rax
        assert

        call_fn box_int, $-1
        call_fn is_eof_object, %rax
        assert

        tag     TAG_CHAR, $-1
        call_fn is_eof_object, %rax
        assert

        call_fn box_int, $0
        call_fn is_eof_object, %rax
        assert

        call_fn current_input_port
        call_fn is_input_port, %rax
        assert

        call_fn current_output_port
        call_fn is_input_port, %rax
        assert

        call_fn current_input_port
        call_fn is_output_port, %rax
        assert

        call_fn current_output_port
        call_fn is_output_port, %rax
        assert

        test_case "test suite end"

        return  $0

        .globl main
