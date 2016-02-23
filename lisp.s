        .include "constants.s"
        .include "macros.s"

        .data
int_format:
        .string "%d"
double_format:
        .string "%f"
false_string:
        .string "#f"
true_string:
        .string "#t"

to_s_jump_table:
        .quad   double_to_s, int_to_s, unbox_pointer, symbol_to_string, pair_to_s, unbox_pointer
unbox_jump_table:
        .quad   unbox_double, unbox_int, unbox_pointer, unbox_pointer, unbox_pointer, unbox_pointer

symbol_table:
        .rept 1024
        .quad   0
        .endr

symbol_next_id:
        .quad   0

        .struct 0
pair_car:
        .struct . + POINTER_SIZE
pair_cdr:
        .struct . + POINTER_SIZE
pair_size:

        .struct 0
symbol_id:
        .struct . + POINTER_SIZE
symbol_name:
        .struct . + POINTER_SIZE
symbol_size:

        .struct 0
symbol_table_entry_value:
        .struct . + POINTER_SIZE
symbol_table_entry_symbol:
        .struct . + POINTER_SIZE
symbol_table_entry_size:

        .text
allocate_code:                  # source_code, source_size
        prologue source_code, source_size, destination_code
        mov     %rdi, source_code(%rsp)
        mov     %rsi, source_size(%rsp)
        call_fn mmap, $NULL, $PAGE_SIZE, $(PROT_READ | PROT_WRITE), $(MAP_PRIVATE | MAP_ANONYMOUS), $-1, $0
	mov     %rax, destination_code(%rsp)

        call_fn memcpy, destination_code(%rsp), source_code(%rsp), source_size(%rsp)
        call_fn mprotect, destination_code(%rsp), $PAGE_SIZE, $(PROT_READ | PROT_EXEC)
        return destination_code(%rsp)

init_runtime:
        prologue
        call_fn string_to_symbol, $false_string
        call_fn string_to_symbol, $true_string
        return

cons:                           # obj1, obj2
        prologue obj1, obj2
        mov     %rdi, obj1(%rsp)
        mov     %rsi, obj2(%rsp)
        call_fn malloc, $pair_size
        mov     obj1(%rsp), %rdi
        mov     %rdi, pair_car(%rax)
        mov     obj2(%rsp), %rsi
        mov     %rsi, pair_cdr(%rax)
        tag     TAG_PAIR, %rax
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

pair_to_s:                      # pair
        prologue pair, str, size, stream
        mov     %rdi, pair(%rsp)

        lea     str(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        mov     %rax, stream(%rsp)

        call_fn fputc, $'(, stream(%rsp)
        mov     $NIL, %rbx
1:      cmp     %rbx, pair(%rsp)
        je      2f

        call_fn car, pair(%rsp)
        call_fn to_s, %rax
        call_fn fputs, %rax, stream(%rsp)

        call_fn cdr, pair(%rsp)
        mov     %rax, pair(%rsp)
        cmp     %rbx, %rax
        je      2f

        call_fn fputc, $' , stream(%rsp)

        has_tag TAG_PAIR, pair(%rsp)
        jnz     1b

        call_fn fputc, $'., stream(%rsp)
        call_fn fputc, $' , stream(%rsp)
        call_fn to_s, pair(%rsp)
        call_fn fputs, %rax, stream(%rsp)

2:      call_fn fputc, $'), stream(%rsp)
        call_fn fclose, stream(%rsp)
        return str(%rsp)

length:                         # list
        prologue
        mov     %rdi, %rax
        xor     %rcx, %rcx
        mov     $NIL, %rbx
1:      cmp     %rbx, %rax
        je      2f

        call_fn cdr, %rax
        inc     %rcx
        jmp     1b

2:      box_int_internal %ecx
        return

make_vector:                    # k
        prologue
        mov     %rdi, %rbx
        unbox_int_internal %edi, %rdi
        inc     %rdi
        imul    $POINTER_SIZE, %rdi
        call_fn malloc, %rdi
        mov     %rbx, (%rax)
        tag     TAG_VECTOR, %rax
        return

vector_length:                  # vector
        unbox_pointer_internal %rdi
        mov     (%rax), %rax
        ret

vector_ref:                     # vector, k
        unbox_int_internal %esi, %rsi
        inc     %rsi
        unbox_pointer_internal %rdi
        mov     (%rax,%rsi,POINTER_SIZE), %rax
        ret

vector_set:                     # vector, k, obj
        unbox_int_internal %esi, %rsi
        inc     %rsi
        unbox_pointer_internal %rdi
        mov     %rdx, (%rax,%rsi,POINTER_SIZE)
        mov     %rdx, %rax
        ret

make_string:                    # k
        prologue
        inc     %rdi
        mov     %rdi, %rbx
        call_fn malloc, %rdi
        movb    $0, (%rax,%rbx,1)
        tag     TAG_STRING, %rax
        return

string_length:                  # vector
        unbox_pointer_internal %rdi
        call_fn strlen, %rax
        box_int_internal %eax
        ret

string_ref:                     # string, k
        unbox_int_internal %esi, %rsi
        unbox_pointer_internal %rdi
        movsxb  (%rax,%rsi,1), %eax
        box_int_internal %eax
        ret

string_set:                     # string, k, char
        unbox_int_internal %esi, %rsi
        unbox_int_internal %edx, %rdx
        unbox_pointer_internal %rdi
        mov     %dl, (%rax,%rsi,1)
        box_int_internal %edx
        ret

identity:                       # x
unbox_double:                   # double
        mov     %rdi, %rax
        ret

unbox_int:                      # int
        unbox_int_internal %edi, %rax
        ret

unbox_pointer:                  # ptr
        unbox_pointer_internal %rdi
        ret

unbox:                          # value
        prologue
        tagged_jump unbox_jump_table
        return

int_to_s:                       # int
        prologue str
        unbox_int_internal %edi, %rdx
        xor     %al, %al
        lea     str(%rsp), %rdi
        call_fn asprintf, %rdi, $int_format, %rdx
        return str(%rsp)

double_to_s:                    # double
        prologue str
        movq    %rdi, %xmm0
        mov     $1, %al         # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        lea     str(%rsp), %rdi
        mov     $double_format, %rsi
        call    asprintf
        return str(%rsp)

symbol_to_string:               # symbol
        imul    $symbol_table_entry_size, %edi
        mov     (symbol_table + symbol_table_entry_symbol)(%rdi), %rax
        mov     symbol_name(%rax), %rax
        ret

string_to_symbol:               # string
        prologue string
        mov     %rdi, string(%rsp)
        call_fn malloc, $symbol_size
        mov     obj1(%rsp), %rdi
        mov     %rdi, symbol_name(%rax)

        movq    (symbol_next_id), %rdi
        mov     %rdi, symbol_id(%rax)
        incq    (symbol_next_id)

        imul    $symbol_table_entry_size, %rdi
        mov     %rax, (symbol_table + symbol_table_entry_symbol)(%rdi)

        tag     TAG_SYMBOL, %rax
        return

number_to_string:               # z
to_s:                           # value
        prologue
        tagged_jump to_s_jump_table
        return

display:                        # obj
        prologue
        call_fn to_s, %rdi
        mov     %rax, %rdi
        xor     %al, %al
        call_fn printf, %rdi
        return $NIL

newline:
        call_fn putchar, $'\n
        mov     $NIL, %rax
        ret

is_eq:                          # obj1, obj2
        eq_internal %rdi, %rsi
        box_boolean_internal %rax
        ret

not:                            # obj
        mov     $FALSE, %rax
        eq_internal %rdi, %rax
        ret

box_boolean:                    # c-boolean
        and     $C_TRUE, %rdi
        box_boolean_internal %rdi
        ret

box_int:                        # c-int
        box_int_internal %edi
        ret

box_string:                     # c-string
        mov     $PAYLOAD_MASK, %rax
        and     %rdi, %rax
        tag     TAG_STRING, %rax
        ret

is_integer:                     # obj
is_exact:                       # z
        has_tag TAG_INT, %rdi
        box_boolean_internal %rax
        ret

is_inexact:                     # z
        is_double_internal %rdi
        box_boolean_internal %rax
        ret

is_boolean:                     # obj
        mov     $TRUE, %rax
        eq_internal %rax, %rdi
        mov     %rax, %r11
        mov     $FALSE, %rax
        eq_internal %rax, %rdi
        or      %r11, %rax
        box_boolean_internal %rax
        ret

is_number:                      # obj
        is_double_internal %rdi
        mov     %rax, %r11
        has_tag TAG_INT, %rdi
        or      %r11, %rax
        box_boolean_internal %rax
        ret

is_null:                        # obj
        mov     $NIL, %rax
        eq_internal %rax, %rdi
        box_boolean_internal %rax
        ret

is_pair:                        # obj
        has_tag TAG_PAIR, %rdi
        mov     %rax, %r11
        mov     $NIL, %rax
        eq_internal %rax, %rdi
        btc     $0, %rax
        and     %r11, %rax
        box_boolean_internal %rax
        ret

is_vector:                      # obj
        has_tag TAG_VECTOR, %rdi
        box_boolean_internal %rax
        ret

is_string:                      # obj
        has_tag TAG_STRING, %rdi
        box_boolean_internal %rax
        ret

is_symbol:                      # obj
        has_tag TAG_SYMBOL, %rdi
        xor     %r11, %r11
        cmp     $C_TRUE, %edi
        setg    %r11b
        and     %r11, %rax
        box_boolean_internal %rax
        ret

neg:                            # z1
        has_tag TAG_INT, %rdi
        jnz     neg_int
neg_double:
        mov     %rdi, %rax
        btc     $SIGN_BIT, %rax
        ret
neg_int:
        neg     %edi
        box_int_internal %edi
        ret

plus:                           # z1, z2
        has_tag TAG_INT, %rdi
        mov     %rax, %rdx
        has_tag TAG_INT, %rsi
        shl     %rax
        or      %rdx, %rax
        shl     $4, %rax
        lea     plus_double_double(%rax), %rax
        jmp     *%rax
1:      addsd   %xmm1, %xmm0
        movq    %xmm0, %rax
        ret
        .align 16
plus_double_double:
        movq    %rdi, %xmm0
        movq    %rsi, %xmm1
        jmp     1b
        .align 16
plus_int_double:
        cvtsi2sd %edi, %xmm0
        movq    %rsi, %xmm1
        jmp     1b
        .align 16
plus_double_int:
        movq    %rdi, %xmm0
        cvtsi2sd %esi, %xmm1
        jmp     1b
        .align 16
plus_int_int:
        mov     %edi, %eax
        add     %esi, %eax
        box_int_internal %eax
        ret

        .globl allocate_code, cons, car, cdr, length, display, newline, box_int, box_string, unbox, number_to_s
        .globl is_eq, is_string, is_boolean, is_symbol, is_null, is_exact, is_inexact, is_integer, is_number, is_pair, is_vector
        .globl make_vector, vector_length, vector_ref, vector_set, make_string, string_length, string_ref, string_set
        .globl int_format, double_format, neg, plus, init_runtime, symbol_next_id
