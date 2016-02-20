        .include "constants.s"
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
NAN:
        .quad NAN_MASK

strlen_name:
        .string "strlen"
allocate_code_name:
        .string "allocate_code"

example_code:
        mov     %rdi, %rax
        add     $4, %rax
        ret
        .equ example_code_size, (. - example_code)
        assert_equals 8, example_code_size

        .text
main:
        .equ array, 0
        prologue 1
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

        call_fn cons, $1, $NIL
        call_fn is_int, %rax
        call_fn println, %rax

        call_fn box_int, $3
        call_fn cons, %rax, $NIL
        mov     %rax, %r11
        call_fn box_int, $2
        call_fn cons, %rax, %r11
        mov     %rax, %r11
        call_fn box_int, $1
        call_fn cons, %rax, %r11
        call_fn println, %rax

        call_fn cons, $3, $NIL
        call_fn cons, $2, %rax
        call_fn cons, $1, %rax
        call_fn pair_length, %rax
        call_fn println, %rax

        call_fn box_int, $2
        mov     %rax, %r11
        call_fn box_int, $4
        call_fn cons, %rax, %r11
        call_fn println, %rax

        call_fn box_int, $42
        call_fn println, %rax

        call_fn box_int, $3
        call_fn println, %rax

        call_fn box_int, $1
        call_fn is_int, %rax
        call_fn println, %rax

        call_fn box_int, $1
        call_fn is_boolean, %rax
        call_fn println, %rax

        call_fn box_pointer, $1
        call_fn is_int, %rax
        call_fn println, %rax

        call_fn println, $TRUE
        call_fn println, $FALSE

        call_fn is_boolean, $FALSE
        call_fn println, %rax

        call_fn println, $NIL
        call_fn println, $TRUE

        call_fn is_double, PI
        call_fn println, %rax

        call_fn is_double, $TRUE
        call_fn println, %rax

        call_fn box_int, $42
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn is_double, %rax
        call_fn println, %rax

        call_fn box_int, $-1
        call_fn is_int, %rax
        call_fn println, %rax

        call_fn box_int, $0
        call_fn println, %rax
        call_fn println, ZERO

        call_fn println, PI
        call_fn println, NAN

        call_fn box_pointer, $strlen_name
        call_fn println, %rax

        call_fn unbox, $TRUE
        mov     %rax, %rdi
        xor     %rax, %rax
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, $FALSE
        mov     %rax, %rdi
        xor     %rax, %rax
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, $NIL
        mov     %rax, %rdi
        xor     %rax, %rax
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn box_int, $-1
        call_fn unbox, %rax
        mov     %rax, %rdi
        xor     %rax, %rax
        call_fn printf, $int_format, %rdi
        call_fn puts, $empty_string

        call_fn unbox, PI
        movq    %rax, %xmm0
        mov     $1, %rax
        mov     $double_format, %rdi
        call    printf
        call_fn puts, $empty_string

        call_fn object_array, $2
        mov     %rax, array(%rsp)

        call_fn box_int, $16
        call_fn aset, array(%rsp), $0, E
        call_fn aset, array(%rsp), $1, PI

        call_fn aget, array(%rsp), $0
        call_fn println, %rax
        call_fn aget, array(%rsp), $1
        call_fn println, %rax

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

        call_fn is_double, PLUS_ONE
        call_fn println, %rax
        call_fn is_double, MINUS_ONE
        call_fn println, %rax
        call_fn is_double, NAN
        call_fn println, %rax

        call_fn box_int, $16
        mov     %rax, %r11
        call_fn add, %rax, %r11
        call_fn println, %rax

        call_fn add, PI, E
        call_fn println, %rax

        call_fn box_int, $1
        call_fn add, %rax, PI
        call_fn println, %rax

        call_fn box_int, $2
        call_fn add, PI, %rax
        call_fn println, %rax

        epilogue $0

        .globl main
