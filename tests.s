        .include "macros.s"

        .section .rodata

double_format:
        .string "%f"
true_string_c:
        .string "#t"
false_string_c:
        .string "#f"
test_case_prefix:
        .string ";;; "

        .macro test_case str
        .section .rodata
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

        .text
print_foo:
        prologue
        mov     %rdi, %rbx
        call_fn display, foo_name, %rbx
        string_literal "print_foo"
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

        string_literal "not reachable"
        assert
        return

returns_42_local:
        prologue
        call_fn box_int, $42
        return

main:
        prologue vec

        test_case "test suite start"

        call_fn init_runtime, %rsp, $C_TRUE

        intern_double PI, 3.14159
        intern_double E, 2.71828
        intern_double ZERO, 0.0
        intern_double PLUS_ONE, 1.0
        intern_double MINUS_ONE, -1.0
        intern_double MINUS_TWO, -2.0
        intern_double FIVE, 5.0

        intern_string strlen_name, "strlen"
        intern_string foo_name, "foo"
        intern_string test_file, "test.txt"

        call_fn string_to_symbol, strlen_name
        mov     %rax, %rbx
        assert

        call_fn string_to_symbol, strlen_name
        call_fn is_eq, %rax, %rbx
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

        call_fn is_string, strlen_name
        assert

        call_fn string_length, strlen_name
        assert

        call_fn box_int $0
        call_fn string_ref, strlen_name, %rax
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

        mov     foo_name, %rax
        assert  write=true

        string_literal "H\be\"l\rlo\nW\\o\'rld\t!"
        assert  write=true

        string_literal ""
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
        assert $NAN_MASK

        mov   strlen_name, %rax
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

        call_fn box_int, $0
        call_fn make_vector, %rax
        assert

        call_fn box_int, $1
        call_fn make_vector, %rax, PI
        assert

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
        mov     %rax, %rbx
        call_fn box_int, $0
        call_fn minus, %rax, %rbx
        assert

        call_fn box_int, $-1
        mov     %rax, %rbx
        call_fn box_int, $0
        call_fn minus, %rax, %rbx
        assert

        call_fn minus, ZERO, PLUS_ONE
        assert

        call_fn minus, ZERO, MINUS_ONE
        assert

        call_fn is_inexact, PLUS_ONE
        assert
        call_fn is_inexact, MINUS_ONE
        assert
        call_fn is_inexact, $NAN_MASK
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
        call_fn expt_, %rax, PI
        assert

        call_fn box_int, $5
        mov     %rax, %rbx
        call_fn box_int, $2
        call_fn expt_, %rax, %rbx
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

        call_fn string_to_symbol, foo_name
        mov     %rax, %rbx
        call_fn set, %rbx, PI

        call_fn lookup_global_symbol, %rbx
        assert

        call_fn set, %rbx, $TRUE
        call_fn lookup_global_symbol, %rbx
        assert

        string_literal "cons"
        call_fn string_to_symbol, %rax
        call_fn lookup_global_symbol, %rax
        assert

        string_literal "42"
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_exact, %rax
        assert
        assert %rbx

        string_literal "42"
        mov     %rax, %rbx
        call_fn box_int, $16
        call_fn string_to_number, %rbx, %rax
        assert

        string_literal "3.14159"
        call_fn string_to_number, %rax
        mov     %rax, %rbx
        call_fn is_inexact, %rax
        assert
        assert %rbx

        call_fn string_to_number, foo_name
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

        call_fn open_output_file, test_file
        mov     %rax, %rbx
        assert
        call_fn is_output_port, %rbx
        assert
        call_fn is_input_port, %rbx
        assert

        call_fn display, foo_name, %rbx
        call_fn close_output_port, %rbx

        call_fn open_input_file, test_file
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
        call_fn unlink, $test_file_c

        call_fn call_with_output_file, test_file, $print_foo
        assert

        call_fn call_with_input_file, test_file, $read_foo
        assert
        call_fn unlink, $test_file_c

        call_fn with_output_to_file, test_file, $print_foo
        assert

        call_fn with_input_from_file, test_file, $read_foo
        assert
        call_fn unlink, $test_file_c

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

        string_literal "#\\a"
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

        string_literal "42"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "-42"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "3.14159"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "-3.14159"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "+"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "-+"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn open_input_string, foo_name
        call_fn read, %rax
        assert  write=true

        string_literal "\"\""
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "\"Hello World\""
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "\"H\\be\\\"l\\rlo\\nW\\\\o\\'rld\\t!\""
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "()"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "(#t . #f)"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "(#t #f)"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "(1 2 . 3)"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "#\\newline"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        string_literal "#(1)"
        call_fn open_input_string, %rax
        call_fn read, %rax
        assert  write=true

        call_fn eval, $TRUE
        assert  write=true

        call_fn box_int, $42
        call_fn eval, %rax
        assert  write=true

        call_fn eval, PI
        assert  write=true

        call_fn eval, foo_name
        assert  write=true

        call_fn box_int, $65
        call_fn integer_to_char, %rax
        call_fn eval, %rax
        assert  write=true

        call_fn cons, foo_name, $NIL
        call_fn cons, %rax, $NIL
        mov     %rax, %rbx
        string_literal "quote"
        call_fn string_to_symbol, %rax
        call_fn cons, %rax, %rbx

        call_fn eval, %rax
        assert  write=true

        call_fn cons, foo_name, $NIL
        call_fn list_to_vector, %rax
        call_fn eval, %rax
        assert  write=true

        call_fn string_to_symbol, foo_name
        call_fn cons, %rax, $NIL
        mov     %rax, %rbx
        string_literal "quote"
        call_fn string_to_symbol, %rax
        call_fn cons, %rax, %rbx
        call_fn eval, %rax
        assert  write=true

        string_literal "cons"
        call_fn string_to_symbol, %rax
        call_fn eval, %rax
        assert  write=true

        test_case "test suite end"

        return  $0

        .globl main
