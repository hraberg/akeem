        .include "constants.s"
        .include "macros.s"

        .data
long_format:
        .string "%ld"
double_format:
        .string "%lf"
true_string:
        .string "true"
false_string:
        .string "false"
nil_string:
        .string "nil"

to_s_jump_table:
        .quad   0, long_to_s, unbox_pointer, boolean_to_s, nil_to_s, pair_to_s

unbox_jump_table:
        .quad   0, unbox_long, unbox_pointer, unbox_pointer, unbox_pointer, unbox_pointer

        .struct 0
pair_car:
        .struct pair_car + POINTER_SIZE
pair_cdr:
        .struct pair_cdr + POINTER_SIZE
pair_size:

        .text

allocate_code:                  # source_code, source_size
        enter_fn 3
        .equ source_code, -(POINTER_SIZE * 3)
        .equ source_size, -(POINTER_SIZE * 2)
        .equ destination_code, -POINTER_SIZE
        mov     %rdi, source_code(%rbp)
        mov     %rsi, source_size(%rbp)
        call_fn mmap, $NULL, $PAGE_SIZE, $(PROT_READ | PROT_WRITE), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
	mov     %rax, destination_code(%rbp)

        call_fn memcpy, destination_code(%rbp), source_code(%rbp), source_size(%rbp)
        call_fn mprotect, destination_code(%rbp), $PAGE_SIZE, $(PROT_READ | PROT_EXEC)
        return  destination_code(%rbp)

cons:                           # car, cdr
        enter_fn 3
        .equ car, -(POINTER_SIZE * 3)
        .equ cdr, -(POINTER_SIZE * 2)
        .equ pair, -POINTER_SIZE
        mov     %rdi, car(%rbp)
        mov     %rsi, cdr(%rbp)
        call_fn malloc, $pair_size
        call_fn tag, $TAG_PAIR, %rax
        mov     %rax, pair(%rbp)

        call_fn set_car, pair(%rbp), car(%rbp)
        call_fn set_cdr, pair(%rbp), cdr(%rbp)
        return  pair(%rbp)

car:                            # pair
        call_fn unbox_pointer, %rdi
        mov     pair_car(%rax), %rax
        ret

cdr:                            # pair
        call_fn unbox_pointer, %rdi
        mov     pair_cdr(%rax), %rax
        ret

set_car:                        # pair, x
        call_fn unbox_pointer, %rdi
        mov     %rsi, pair_car(%rax)
        mov     %rsi, %rax
        ret

set_cdr:                        # pair, x
        call_fn unbox_pointer, %rdi
        mov     %rsi, pair_cdr(%rax)
        mov     %rsi, %rax
        ret

pair_to_s:                      # pair
        enter_fn 4
        .equ stream, -(POINTER_SIZE * 4)
        .equ size, -(POINTER_SIZE * 3)
        .equ str, -(POINTER_SIZE * 2)
        .equ pair, -POINTER_SIZE
        mov     %rdi, pair(%rbp)

        lea     str(%rbp), %rdi
        lea     size(%rbp), %rsi
        call_fn open_memstream, %rdi, %rsi
        mov     %rax, stream(%rbp)

        call_fn fputc, $'(, stream(%rbp)
1:      mov     $NIL, %r11
        cmp     %r11, pair(%rbp)
        je      2f

        call_fn car, pair(%rbp)
        call_fn to_s, %rax
        call_fn fputs, %rax, stream(%rbp)

        call_fn cdr, pair(%rbp)
        mov     %rax, pair(%rbp)
        mov     $NIL, %r11
        cmp     %r11, pair(%rbp)
        je      2f

        call_fn fputc, $' , stream(%rbp)

        call_fn is_pair, pair(%rbp)
        test    $C_TRUE, %rax
        jnz     1b

        call_fn fputc, $'., stream(%rbp)
        call_fn fputc, $' , stream(%rbp)
        call_fn to_s, pair(%rbp)
        call_fn fputs, %rax, stream(%rbp)

2:      call_fn fputc, $'), stream(%rbp)
        call_fn fclose, stream(%rbp)
        return  str(%rbp)

pair_length:                    # pair
        mov     %rdi, %rax
        xor     %rcx, %rcx
        mov     $NIL, %r11
1:      cmp     %r11, %rax
        je      2f

        call_fn cdr, %rax
        inc     %rcx
        jmp     1b

2:      call_fn box_long, %rcx
        ret

byte_array:                     # length
        call_fn malloc, %rdi
        call_fn box_pointer, %rax
        ret

object_array:                   # length
        imul    $POINTER_SIZE, %rdi
        call_fn byte_array, %rdi
        ret

aget:                           # array, idx
        call_fn unbox_pointer, %rdi
        mov     (%rax,%rsi,POINTER_SIZE), %rax
        ret

aset:                           # array, idx, value
        call_fn unbox_pointer, %rdi
        mov     %rdx, (%rax,%rsi,POINTER_SIZE)
        mov     %rdx, %rax
        ret

unbox_long:                     # long
        mov     $(PAYLOAD_SIGN | PAYLOAD_MASK), %rax
        mov     $-1, %r11
        and     %rax, %rdi
        cmovns  %r11, %rax
        not     %rax
        or      %rdi, %rax
        ret

unbox_pointer:                  # ptr
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        ret

long_to_s:                      # long
        enter_fn 1
        .equ str, -POINTER_SIZE
        call_fn unbox_long, %rdi
        mov     %rax, %rdx
        xor     %rax, %rax
        lea     str(%rbp), %rdi
        call_fn asprintf, %rdi, $long_format, %rdx
        return  str(%rbp)

double_to_s:                    # double
        enter_fn 1
        .equ str, -POINTER_SIZE
        movq    %rdi, %xmm0
        mov     $1, %rax        # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        lea     str(%rbp), %rdi
        mov     $double_format, %rsi
        call    asprintf
        return  str(%rbp)

boolean_to_s:
        test    $C_TRUE, %rdi
        jz      1f
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
        mov     (%rdi,%rax,POINTER_SIZE), %rax
        call_fn *%rax, %rsi
        return  %rax

to_s:                           # value
        enter_fn 1
        .equ value, -POINTER_SIZE
        mov     %rdi, value(%rbp)
        call_fn is_double, value(%rbp)
        test    $C_TRUE, %rax
        jz      1f
        call_fn double_to_s, value(%rbp)
        return  %rax
1:      call_fn tagged_jump, $to_s_jump_table, value(%rbp)
        return  %rax

unbox:                          # value
        enter_fn 1
        .equ value, -POINTER_SIZE
        mov     %rdi, value(%rbp)
        call_fn is_double, value(%rbp)
        test    $C_TRUE, %rax
        jz      1f
        return  value(%rbp)
1:      call_fn tagged_jump, $unbox_jump_table, value(%rbp)
        return  %rax

println:                        # value
        enter_fn
        call_fn to_s, %rdi
        call_fn puts, %rax
        return  $NIL

print:                          # value
        enter_fn
        call_fn to_s, %rdi
        call_fn printf, %rax
        return  $NIL

eq:                             # x, y
        xor     %rax, %rax
        cmp     %rdi, %rsi
        sete    %al
        call_fn box_boolean, %rax
        ret
not:                            # x
        xor     $C_TRUE, %rdi
        mov     %rdi, %rax
        ret
tag:                            # tag, value
        mov     $NAN_MASK, %rax
        or      %rsi, %rax
        or      %rdi, %rax
        ret

has_tag:                        # tag, value
        mov     $TAG_MASK, %rax
        and     %rax, %rsi
        call_fn eq, %rdi, %rsi
        ret

box_boolean:                    # value
        and     $C_TRUE, %rdi
        call_fn tag, $TAG_BOOLEAN, %rdi
        ret

box_long:                       # value
        enter_fn
        mov     $(PAYLOAD_SIGN | PAYLOAD_MASK), %rax
        and     %rdi, %rax
        call_fn tag, $TAG_LONG, %rax
        return %rax

box_pointer:                    # value
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        call_fn tag, $TAG_POINTER, %rax
        ret

is_long:                        # value
        call_fn has_tag, $TAG_LONG, %rdi
        ret

is_pointer:                     # value
        call_fn has_tag, $TAG_POINTER, %rdi
        ret

is_boolean:                     # value
        call_fn has_tag, $TAG_BOOLEAN, %rdi
        ret

is_nil:                         # value
        call_fn eq, %rdi, $NIL
        ret

is_pair:                        # value
        call_fn has_tag, $TAG_PAIR, %rdi
        ret

is_double:                      # value
        movq    %rdi, %xmm0
        call_fn isnan
        not     %rax
        call_fn box_boolean, %rax
        ret

        .globl allocate_code, cons, car, cdr, pair_length, print, println, box_long, box_pointer, is_long, is_boolean,
        .globl is_double, is_pair, unbox, tag, aget, aset, object_array, long_format, double_format
