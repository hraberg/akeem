        .include "macros.s"

        .equ NULL, 0
        .equ PAGE_SIZE, 4096

        ## dlfcn.h
        .equ RTLD_DEFAULT, 0

        ## sys/mman.h
        .equ PROT_READ, 0x1
        .equ PROT_WRITE, 0x2
        .equ PROT_EXEC, 0x4
        .equ MAP_PRIVATE, 0x02
        .equ MAP_ANONYMOUS, 0x20

        .equ NAN_MASK, 0x7FF8000000000000
        .equ TAG_SHIFT, 47
        .equ TAG_MASK, 0xf << TAG_SHIFT

        .equ PAYLOAD_MASK, (1 << TAG_SHIFT) - 1
        .equ PAYLOAD_SIGN, 1 << 63

        .equ TAG_LONG, 1 << TAG_SHIFT
        .equ TAG_POINTER, 2 << TAG_SHIFT
        .equ TAG_BOOLEAN, 3 << TAG_SHIFT
        .equ TAG_NIL, 4 << TAG_SHIFT
        .equ TAG_PAIR, 5 << TAG_SHIFT

        .equ TRUE, (NAN_MASK | TAG_BOOLEAN | 1)
        .equ FALSE, (NAN_MASK | TAG_BOOLEAN | 0)
        .equ NIL, (NAN_MASK | TAG_NIL)

        .data
PI:
        .double 3.1415

strlen_name:
        .string "strlen"
allocate_code_name:
        .string "allocate_code"

long_format:
        .string "%ld"
double_format:
        .string "%lf"
empty_string:
        .string ""
true_string:
        .string "true"
false_string:
        .string "false"
nil_string:
        .string "nil"

example_code:
        mov     %rdi, %rax
        add     $4, %rax
        ret
        .equ example_code_size, (. - example_code)
        assert_equals 8, example_code_size

enter_fn_code:
        push    %rbp
        mov     %rsp, %rbp
        sub     $0, %rsp
        .equ enter_fn_code_size, (. - enter_fn_code)
        .equ enter_fn_locals_index, (enter_fn_code_size - 1)
        assert_equals 8, enter_fn_code_size
        assert_equals 7, enter_fn_locals_index

to_s_jump_table:
        .quad   0, long_to_s, unbox_pointer, boolean_to_s, nil_to_s, pair_to_s

        .struct 0
pair_car:
        .struct pair_car + 8
pair_cdr:
        .struct pair_cdr + 8
pair_size:

        .text

allocate_code:                  # source_code, source_size
        enter_fn 3
        .equ source_code, -24
        .equ source_size, -16
        .equ destination_code, -8
        mov     %rdi, source_code(%rbp)
        mov     %rsi, source_size(%rbp)
        call_fn mmap, $NULL, $PAGE_SIZE, $(PROT_READ | PROT_WRITE), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
	mov     %rax, destination_code(%rbp)

        call_fn memcpy, destination_code(%rbp), source_code(%rbp), source_size(%rbp)
        call_fn mprotect, destination_code(%rbp), $PAGE_SIZE, $(PROT_READ | PROT_EXEC)
        return  destination_code(%rbp)

cons:                           # car, cdr
        enter_fn 3
        .equ car, -24
        .equ cdr, -16
        .equ pair, -8
        mov     %rdi, car(%rbp)
        mov     %rsi, cdr(%rbp)
        call_fn malloc, $pair_size
        call_fn tag $TAG_PAIR, %rax
        mov     %rax, pair(%rbp)

        call_fn set_car, pair(%rbp), car(%rbp)
        call_fn set_cdr, pair(%rbp), cdr(%rbp)
        return  pair(%rbp)

car:                            # pair
        mov     $PAYLOAD_MASK, %rax
        and     %rax, %rdi
        mov     pair_car(%rdi), %rax
        ret

cdr:                            # pair
        mov     $PAYLOAD_MASK, %rax
        and     %rax, %rdi
        mov     pair_cdr(%rdi), %rax
        ret

set_car:                        # pair, x
        mov     $PAYLOAD_MASK, %rax
        and     %rax, %rdi
        mov     %rsi, pair_car(%rdi)
        mov     %rsi, %rax
        ret

set_cdr:                        # pair, x
        mov     $PAYLOAD_MASK, %rax
        and     %rax, %rdi
        mov     %rsi, pair_cdr(%rdi)
        mov     %rsi, %rax
        ret

pair_to_s:                      # pair
        enter_fn 4
        .equ stream, -32
        .equ size, -24
        .equ str, -16
        .equ pair, -8
        mov     %rdi, pair(%rbp)

        mov     %rbp, %rdi
        add     $str, %rdi
        mov     %rbp, %rsi
        add     $size, %rsi
        call_fn open_memstream, %rdi, %rsi
        mov     %rax, stream(%rbp)

        call_fn fputc, $'(, stream(%rbp)
1:
        mov     $NIL, %r11
        cmp     %r11, pair(%rbp)
        je      2f
        call_fn car, pair(%rbp)
        call_fn to_s, %rax
        call_fn fprintf, stream(%rbp), %rax
        call_fn cdr, pair(%rbp)
        mov     %rax, pair(%rbp)
        mov     $NIL, %r11
        cmp     %r11, pair(%rbp)
        je      2f
        call_fn fputc, $' , stream(%rbp)
        call_fn is_pair, pair(%rbp)
        mov     $TRUE, %r11
        cmp     %r11, %rax
        je      1b
        call_fn fputc, $'., stream(%rbp)
        call_fn fputc, $' , stream(%rbp)
        call_fn to_s, pair(%rbp)
        call_fn fprintf, stream(%rbp), %rax
2:
        call_fn fputc, $'), stream(%rbp)
        call_fn fclose, stream(%rbp)
        return  str(%rbp)

pair_length:                    # pair
        mov     %rdi, %rax
        xor     %rcx, %rcx
        mov     $NIL, %r11
1:
        cmp     %r11, %rax
        je      2f
        call_fn cdr, %rax
        inc     %rcx
        jmp     1b
2:
        call_fn box_long %rcx
        ret

unbox_long:                     # long
        mov     $PAYLOAD_SIGN, %rax
        mov     $PAYLOAD_MASK, %r11
        or      %r11, %rax
        and     %rdi, %rax
        jns     1f
        not     %r11
        or      %r11, %rax
1:      ret

unbox_pointer:                  # ptr
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        ret

long_to_s:                      # long
        enter_fn 2
        .equ long, -16
        .equ str, -8
        call_fn unbox_long, %rdi
        mov     %rax, long(%rbp)
        mov     %rbp, %rax
        add     $str, %rax
        call_fn asprintf %rax, $long_format, long(%rbp)
        return  str(%rbp)

double_to_s:                    # double
        enter_fn 1
        .equ str, -8
        movq    %rdi, %xmm0
        mov     $1, %rax        # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        mov     %rbp, %rdi
        add     $str, %rdi
        mov     $double_format, %rsi
        call    asprintf
        return  str(%rbp)

boolean_to_s:
        mov     $TRUE, %rax
        cmp     %rdi, %rax
        jne     1f
        mov     $true_string, %rax
        ret
1:      mov     $false_string, %rax
        ret

nil_to_s:
        mov $nil_string, %rax
        ret

tagged_jump:                    # table, value
        enter_fn
        mov     $TAG_MASK, %rax
        and     %rsi, %rax
        shr     $TAG_SHIFT, %rax
        mov     (%rdi,%rax,8), %rax
        call_fn *%rax, %rsi
        return  %rax

to_s:                           # value
        enter_fn 1
        .equ value, -8
        mov     %rdi, value(%rbp)
        call_fn is_double, value(%rbp)
        mov     $TRUE, %r11
        cmp     %r11, %rax
        jne     1f
        call_fn double_to_s, value(%rbp)
        return  %rax
1:
        call_fn tagged_jump, $to_s_jump_table, value(%rbp)
        return  %rax

println:                        # value
        enter_fn
        call_fn to_s, %rdi
        call_fn puts, %rax
        return  $NIL

eq:                             # x, y
        enter_fn
        cmp     %rdi, %rsi
        jne     1f
        return  $TRUE
1:      return  $FALSE

not:                            # x
        enter_fn
        mov     $TRUE, %rax
        cmp     %rdi, %rax
        jne     1f
        return  $FALSE
1:      return  $TRUE

tag:                            # tag value
        mov     $NAN_MASK, %rax
        or      %rsi, %rax
        or      %rdi, %rax
        ret

has_tag:                        # tag, value
        mov     $TAG_MASK, %rax
        and     %rax, %rsi
        call_fn eq, %rdi, %rsi
        ret

box_long:                       # value
        enter_fn
        mov     $PAYLOAD_MASK, %rax
        mov     $PAYLOAD_SIGN, %rsi
        or      %rsi, %rax
        and     %rdi, %rax
        call_fn tag, $TAG_LONG, %rax
        return %rax

box_pointer:                    # value
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        call_fn tag, $TAG_POINTER, %rax
        ret

is_long:                        # value
        mov     %rdi, %rax
        call_fn has_tag, $TAG_LONG, %rax
        ret

is_pointer:                     # value
        mov     %rdi, %rax
        call_fn has_tag, $TAG_POINTER, %rax
        ret

is_boolean:                     # value
        mov     %rdi, %rax
        call_fn has_tag, $TAG_BOOLEAN, %rax
        ret

is_nil:                         # value
        mov     %rdi, %rax
        call_fn has_tag, $TAG_NIL, %rax
        ret

is_pair:                        # value
        mov     %rdi, %rax
        call_fn has_tag, $TAG_PAIR, %rax
        ret

is_double:                      # value
        mov     $NAN_MASK, %rax
        and     %rdi, %rax
        call_fn eq, %rax, $NAN_MASK
        call_fn not, %rax
        ret

main:
        enter_fn
        call_fn dlsym, $RTLD_DEFAULT, $strlen_name
        call_fn *%rax, $long_format
        call_fn box_long, %rax
        call_fn println, %rax

        call_fn dlsym, $RTLD_DEFAULT, $allocate_code_name
        call_fn *%rax, $example_code, $example_code_size
        call_fn *%rax, $2
        call_fn box_long, %rax
        call_fn println, %rax

        call_fn cons, $1, $NIL
        call_fn is_pair, %rax
        call_fn println, %rax

        call_fn cons, $1, $NIL
        call_fn is_long, %rax
        call_fn println, %rax

        call_fn box_long, $3
        call_fn cons, %rax, $NIL
        mov     %rax, %r11
        call_fn box_long, $2
        call_fn cons, %rax, %r11
        mov     %rax, %r11
        call_fn box_long, $1
        call_fn cons, %rax, %r11
        call_fn println, %rax

        call_fn cons, $3, $NIL
        call_fn cons, $2, %rax
        call_fn cons, $1, %rax
        call_fn pair_length, %rax
        call_fn println, %rax

        call_fn box_long, $2
        mov     %rax, %r11
        call_fn box_long, $4
        call_fn cons, %rax, %r11
        call_fn println, %rax

        call_fn box_long, $42
        call_fn println, %rax

        call_fn box_long, $3
        call_fn println, %rax

        call_fn box_long, $1
        call_fn is_long, %rax
        call_fn println, %rax

        call_fn box_long, $1
        call_fn is_boolean, %rax
        call_fn println, %rax

        call_fn tag, $TAG_POINTER, $1
        call_fn is_long, %rax
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

        call_fn box_long, $42
        call_fn println, %rax

        call_fn box_long, $-1
        call_fn println, %rax

        call_fn box_long, $-1
        call_fn is_double, %rax
        call_fn println, %rax

        call_fn box_long, $-1
        call_fn is_long, %rax
        call_fn println, %rax

        call_fn box_long, $0
        call_fn println, %rax

        call_fn println, PI

        call_fn box_pointer, $strlen_name
        call_fn println, %rax

        return  $0

        .globl main, allocate_code
