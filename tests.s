        .include "macros.s"

        .data
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

not_reachable_string:
        .string "not reachable"
empty_string:
        .string ""
forty_two_string:
        .string "42"
minus_forty_two_string:
        .string "-42"
pi_string:
        .string "3.14159"
minus_pi_string:
        .string "-3.14159"
plus_string:
        .string "+"
minus_plus_string:
        .string "-+"
escape_codes:
        .string "H\be\"l\rlo\nW\\o\'rld\t!"
escape_codes_string:
        .string "\"H\\be\\\"l\\rlo\\nW\\\\o\\'rld\\t!\""
string_string:
        .string "\"Hello World\""
empty_string_string:
        .string "\"\""
char_string:
        .string "#\\a"
empty_list_string:
        .string "()"
list_with_booleans_string:
        .string "(#t #f)"
pair_with_booleans_string:
        .string "(#t . #f)"
irregular_list_string:
        .string "(1 2 . 3)"
foo_name:
        .string "foo"
strlen_name:
        .string "strlen"
allocate_code_name:
        .string "allocate_code"
test_file:
        .string "test.txt"
print_foo_string:
        .string "print_foo"
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

        .macro assert value=%rax write=false
        .ifc \write, true
        call_fn write, \value
        .else
        call_fn display, \value
        .endif
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
        jz      is_\@
        call_fn printf, $assertion_failed_format, $test_string_\@, %rbx
        call_fn exit, $1
is_\@:
        nop
        .endm

print_foo:
        prologue
        mov     %rdi, %rbx
        call_fn box_string, $foo_name
        call_fn display, %rax, %rbx
        call_fn box_string $print_foo_string
        return

read_foo:
        minimal_prologue
        call_fn read_char, %rdi
        return

returns_42_non_local:
        prologue
        mov     %rdi, %rbx
        call_fn box_int, $42
        call_fn *%rbx, %rax

        call_fn box_string, $not_reachable_string
        assert
        return

returns_42_local:
        prologue
        call_fn box_int, $42
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

        test_case "test suite start"

        call_fn init_runtime, %rsp

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
        mov     %rax, %rbx
        assert

        call_fn reverse, %rbx
        assert

        call_fn list_to_vector, %rbx
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

        call_fn box_int, $65
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_int, $8
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_int, $9
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_int, $10
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_int, $13
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_int, $32
        call_fn integer_to_char, %rax
        assert  write=true

        call_fn box_string, $foo_name
        assert  write=true

        call_fn box_string, $escape_codes
        assert  write=true

        call_fn box_string, $empty_string
        assert  write=true

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
        call_fn string_length, %rax
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

        call_fn current_output_port
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

        call_fn box_string $test_file
        call_fn open_output_file, %rax
        mov     %rax, %rbx
        assert
        call_fn is_output_port, %rbx
        assert
        call_fn is_input_port, %rbx
        assert

        call_fn box_string, $foo_name
        call_fn display, %rax, %rbx
        call_fn close_output_port, %rbx

        call_fn box_string $test_file
        call_fn open_input_file, %rax
        mov     %rax, %rbx
        call_fn is_output_port, %rbx
        assert
        call_fn is_input_port, %rbx
        assert

        call_fn peek_char, %rbx
        assert
        call_fn read_char, %rbx
        assert
        call_fn peek_char, %rbx
        assert
        call_fn read_char, %rbx
        assert
        call_fn peek_char, %rbx
        assert
        call_fn read_char, %rbx
        assert
        call_fn peek_char, %rbx
        call_fn is_eof_object, %rax
        assert
        call_fn read_char, %rbx
        call_fn is_eof_object, %rax
        assert
        call_fn peek_char, %rbx
        call_fn is_eof_object, %rax
        assert
        call_fn read_char, %rbx
        call_fn is_eof_object, %rax
        assert
        call_fn close_input_port, %rbx
        call_fn unlink, $test_file

        call_fn box_string $test_file
        call_fn call_with_output_file, %rax, $print_foo
        assert

        call_fn box_string $test_file
        call_fn call_with_input_file, %rax, $read_foo
        assert
        call_fn unlink, $test_file

        call_fn box_string $test_file
        call_fn with_output_to_file, %rax, $print_foo
        assert

        call_fn box_string $test_file
        call_fn with_input_from_file, %rax, $read_foo
        assert
        call_fn unlink, $test_file

        tag     TAG_PROCEDURE, $read_foo
        assert

        call_fn call_with_current_continuation, $returns_42_non_local
        assert

        call_fn call_with_current_continuation, $returns_42_local
        assert

        call_fn cons, $NIL, $NIL
        mov     %rax, %rbx
        call_fn gc_has_mark, %rbx
        assert

        call_fn gc_mark
        call_fn gc_has_mark, %rbx
        assert

        call_fn box_int, $0
        call_fn make_string, %rax
        mov     %rax, %rbx
        call_fn gc_has_mark, %rbx
        assert

        call_fn gc_mark
        call_fn gc_has_mark, %rbx
        assert

        call_fn box_int, $0
        call_fn make_vector, %rax
        mov     %rax, %rbx
        call_fn gc_has_mark, %rbx
        assert

        call_fn gc_mark
        call_fn gc_has_mark, %rbx
        assert
        xor     %rbx, %rbx
        call_fn gc_sweep

        call_fn gc
        call_fn object_space_size
        assert
        call_fn gc

        call_fn box_int, $0
        call_fn make_string, %rax
        call_fn cons, %rax, $NIL
        mov     %rax, %rbx
        call_fn object_space_size
        assert

        call_fn gc
        xor     %rbx, %rbx
        call_fn object_space_size
        assert

        call_fn gc
        call_fn object_space_size
        assert
        call_fn gc

        call_fn box_int, $1
        call_fn make_vector, %rax
        mov     %rax, vec(%rsp)

        call_fn box_int, $0
        mov     %rax, %rbx
        call_fn make_string, %rax
        call_fn vector_set, vec(%rsp), %rbx, %rax
        mov     vec(%rsp), %rbx

        call_fn object_space_size
        assert

        call_fn gc
        xor     %rbx, %rbx
        call_fn object_space_size
        assert

        call_fn gc
        call_fn object_space_size
        assert

        call_fn box_string, $char_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $true_string_c
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $false_string_c
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $forty_two_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $minus_forty_two_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $pi_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $minus_pi_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $plus_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $minus_plus_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $foo_name
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $empty_string_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $string_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $escape_codes_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $empty_list_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $pair_with_booleans_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $list_with_booleans_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn box_string, $irregular_list_string
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        test_case "test suite end"

        return  $0

        .globl main
