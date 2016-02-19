        .include "constants.s"
        .include "macros.s"

        .data
int_format:
        .string "%d"
double_format:
        .string "%lf"
true_string:
        .string "true"
false_string:
        .string "false"
nil_string:
        .string "nil"

to_s_jump_table:
        .quad   double_to_s, int_to_s, unbox_pointer, boolean_to_s, nil_to_s, pair_to_s
unbox_jump_table:
        .quad   unbox_double, unbox_int, unbox_pointer, unbox_boolean, unbox_nil, unbox_pointer

        .struct 0
pair_car:
        .struct . + POINTER_SIZE
pair_cdr:
        .struct . + POINTER_SIZE
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
        call_fn tag, $(NAN_MASK | TAG_PAIR), %rax
        mov     %rax, pair(%rbp)

        call_fn set_car, pair(%rbp), car(%rbp)
        call_fn set_cdr, pair(%rbp), cdr(%rbp)
        return  pair(%rbp)

car:                            # pair
        unbox_pointer_internal %rdi
        mov     pair_car(%rax), %rax
        ret

cdr:                            # pair
        unbox_pointer_internal %rdi
        mov     pair_cdr(%rax), %rax
        ret

set_car:                        # pair, x
        unbox_pointer_internal %rdi
        mov     %rsi, pair_car(%rax)
        mov     %rsi, %rax
        ret

set_cdr:                        # pair, x
        unbox_pointer_internal %rdi
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

2:      call_fn box_int, %rcx
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
        unbox_pointer_internal %rdi
        mov     (%rax,%rsi,POINTER_SIZE), %rax
        ret

aset:                           # array, idx, value
        unbox_pointer_internal %rdi
        mov     %rdx, (%rax,%rsi,POINTER_SIZE)
        mov     %rdx, %rax
        ret

identity:                       # x
unbox_double:                   # double
        mov     %rdi, %rax
        ret

unbox_int:                      # int
unbox_boolean:                  # boolean
        unbox_int_internal %edi
        ret

unbox_nil:                      # nil
        mov     $NULL, %rax
        ret

unbox_pointer:                  # ptr
        unbox_pointer_internal %rdi
        ret

unbox:                          # value
        tagged_jump unbox_jump_table

int_to_s:                       # int
        enter_fn 1
        .equ str, -POINTER_SIZE
        unbox_int_internal %edi, %rdx
        xor     %rax, %rax
        lea     str(%rbp), %rdi
        call_fn asprintf, %rdi, $int_format, %rdx
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
        mov     $true_string, %rax
        mov     $false_string, %r11
        test    $C_TRUE, %rdi
        cmovz   %r11, %rax
        ret

nil_to_s:                       # nil
        mov     $nil_string, %rax
        ret

to_s:                           # value
        tagged_jump to_s_jump_table

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
tag:                            # masked_tag, value
        or      %rdi, %rsi
        mov     %rsi, %rax
        ret

has_tag:                        # tag, value
        mov     $TAG_MASK, %rax
        and     %rax, %rsi
        call_fn eq, %rdi, %rsi
        ret

box_boolean:                    # value
        and     $C_TRUE, %rdi
        call_fn tag, $(NAN_MASK | TAG_BOOLEAN), %rdi
        ret

box_int:                        # value
        mov     %edi, %eax
        call_fn tag, $(NAN_MASK | TAG_INT), %rax
        ret

box_pointer:                    # value
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        call_fn tag, $(NAN_MASK | TAG_POINTER), %rax
        ret

is_int:                         # value
        call_fn has_tag, $TAG_INT, %rdi
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
        mov     $(SIGN_BIT - 1), %rax
        and     %rax, %rdi
        mov     $NAN_MASK, %rax
        cmp     %rax, %rdi
        setle   %al
        call_fn box_boolean, %rax
        ret

neg:                            # value
        is_int_internal %rdi
        jnz     neg_int
neg_double:
        mov     %rdi, %rax
        mov     $SIGN_BIT, %r11
        xor     %r11, %rax
        ret
neg_int:
        neg     %edi
        box_int_internal %edi
        ret

add:                            # x, y
        is_int_internal %rdi
        mov     %rax, %rbx
        is_int_internal %rsi
        shl     %rax
        or      %rbx, %rax
        shl     %rax
        lea     add_double_double(,%rax,8), %rax
        jmp     *%rax
1:      addsd   %xmm1, %xmm0
        movq    %xmm0, %rax
        ret
        .align 16, 0
add_double_double:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        jmp     1b
        .align 16, 0
add_int_double:
        cvtsi2sd %edi, %xmm0
        movq     %rsi, %xmm1
        jmp     1b
        .align 16, 0
add_double_int:
        movq     %rdi, %xmm0
        cvtsi2sd %esi, %xmm1
        jmp     1b
        .align 16, 0
add_int_int:
        mov     %edi, %eax
        add     %esi, %eax
        box_int_internal %eax
        ret

        .globl allocate_code, cons, car, cdr, pair_length, print, println, box_int, box_pointer, is_int, is_boolean,
        .globl is_double, is_pair, unbox, tag, aget, aset, object_array, int_format, double_format, neg, add
