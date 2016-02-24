        .include "macros.s"

        .data
char_format:
        .string "#\\%c"
int_format:
        .string "%d"
double_format:
        .string "%f"
backspace_char:
        .string "#\\backspace"
tab_char:
        .string "#\\tab"
newline_char:
        .string "#\\newline"
return_char:
        .string "#\\return"
space_char:
        .string "#\\space"
false_string:
        .string "#f"
true_string:
        .string "#t"

char_table:
        .zero   POINTER_SIZE * 8
        .quad   backspace_char
        .quad   tab_char
        .quad   newline_char
        .zero   POINTER_SIZE * 2
        .quad   return_char
        .zero   POINTER_SIZE * 18
        .quad   space_char

to_string_jump_table:
        .quad   double_to_string
        .quad   boolean_to_string
        .zero   POINTER_SIZE
        .quad   char_to_string
        .quad   int_to_string
        .zero   POINTER_SIZE * 3
        .quad   unbox_pointer
        .zero   POINTER_SIZE * 7
        .quad   symbol_to_string
        .zero   POINTER_SIZE * 7
        .quad   pair_to_string
        .zero   POINTER_SIZE * 7
        .quad   vector_to_string
        .zero   POINTER_SIZE * 7
        .quad   0

unbox_jump_table:
        .quad   unbox_double
        .quad   unbox_int
        .zero   POINTER_SIZE
        .quad   unbox_int
        .quad   unbox_int
        .zero   POINTER_SIZE * 3
        .quad   unbox_pointer
        .zero   POINTER_SIZE * 7
        .quad   unbox_pointer
        .zero   POINTER_SIZE * 7
        .quad   unbox_pointer
        .zero   POINTER_SIZE * 7
        .quad   unbox_vector
        .zero   POINTER_SIZE * 7
        .quad   0

symbol_table_values:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE
symbol_table_names:
        .zero   MAX_NUMBER_OF_SYMBOLS * POINTER_SIZE
symbol_next_id:
        .quad   TAG_MASK + 1

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

pair_to_string:                 # pair
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
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        call_fn fputs, %rdi, stream(%rsp)

        call_fn cdr, pair(%rsp)
        mov     %rax, pair(%rsp)
        cmp     %rbx, %rax
        je      2f

        call_fn fputc, $' , stream(%rsp)

        has_tag TAG_PAIR, pair(%rsp)
        jnz     1b

        call_fn fputc, $'., stream(%rsp)
        call_fn fputc, $' , stream(%rsp)
        call_fn to_string, pair(%rsp)
        unbox_pointer_internal %rax, %rdi
        call_fn fputs, %rdi, stream(%rsp)

2:      call_fn fputc, $'), stream(%rsp)
        call_fn fclose, stream(%rsp)
        tag     TAG_STRING, str(%rsp)
        return

length:                         # list
        prologue
        mov     %rdi, %rax
        xor     %ecx, %ecx
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
        mov     %edi, %ebx
        inc     %edi
        imul    $POINTER_SIZE, %rdi
        call_fn malloc, %rdi
        mov     %rbx, (%rax)
        tag     TAG_VECTOR, %rax
        return

vector_length:                  # vector
        unbox_pointer_internal %rdi
        mov     (%rax), %rax
        box_int_internal %eax
        ret

vector_ref:                     # vector, k
        inc     %esi
        unbox_pointer_internal %rdi
        mov     (%rax,%rsi,POINTER_SIZE), %rax
        ret

vector_set:                     # vector, k, obj
        inc     %esi
        unbox_pointer_internal %rdi
        mov     %rdx, (%rax,%rsi,POINTER_SIZE)
        mov     %rdx, %rax
        ret

vector_to_string:                 # vector
        prologue idx, str, size, stream
        unbox_pointer_internal %rdi, %rbx

        lea     str(%rsp), %rdi
        lea     size(%rsp), %rsi
        call_fn open_memstream, %rdi, %rsi
        mov     %rax, stream(%rsp)

        call_fn fputc, $'\#, stream(%rsp)
        call_fn fputc, $'(, stream(%rsp)

        movq    $0, idx(%rsp)
1:      mov     idx(%rsp), %rcx
        test    %rcx, %rcx
        jz      2f
        cmp     (%rbx), %rcx
        je      3f

        call_fn fputc, $' , stream(%rsp)

2:      incq    idx(%rsp)
        mov     idx(%rsp), %rcx

        mov     (%rbx,%rcx,POINTER_SIZE), %rax
        call_fn to_string, %rax
        unbox_pointer_internal %rax, %rdi
        call_fn fputs, %rdi, stream(%rsp)
        jmp     1b

3:      call_fn fputc, $'), stream(%rsp)
        call_fn fclose, stream(%rsp)
        tag     TAG_STRING, str(%rsp)
        return

make_string:                    # k
        prologue
        inc     %esi
        mov     %rdi, %rbx
        call_fn malloc, %rdi
        movb    $0, (%rax,%rbx)
        tag     TAG_STRING, %rax
        return

string_length:                  # vector
        unbox_pointer_internal %rdi
        call_fn strlen, %rax
        box_int_internal %eax
        ret

string_ref:                     # string, k
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        movsxb  (%rax,%rsi), %eax
        tag     TAG_CHAR, %rax
        ret

string_set:                     # string, k, char
        mov     %esi, %esi
        unbox_pointer_internal %rdi
        mov     %dl, (%rax,%rsi)
        box_int_internal %edx
        ret

string_to_number:               # string
        prologue tail
        unbox_pointer_internal %rdi, %rbx

        lea     tail(%rsp), %r11
        call_fn strtol, %rbx, %r11
        mov     tail(%rsp), %r11
        cmpb    $0, (%r11)
        jne     1f
        box_int_internal %eax
        return

1:      lea     tail(%rsp), %r11
        call_fn strtod, %rbx, %r11
        mov     tail(%rsp), %r11
        cmpb    $0, (%r11)
        jne     2f
        movq    %xmm0, %rax
        return

2:      return $FALSE

char_to_string:
        prologue str
        mov     %edi, %edx
        cmp     $SPACE_CHAR, %dx
        jg      1f
        mov     char_table(,%edx,POINTER_SIZE), %rax
        test    %rax, %rax
        jz      1f
        tag     TAG_STRING, %rax
        return
1:      xor     %al, %al
        lea     str(%rsp), %rdi
        call_fn asprintf, %rdi, $char_format, %rdx
        tag     TAG_STRING, str(%rsp)
        return

char_to_integer:
        movsx   %di, %eax
        box_int_internal %eax
        ret

integer_to_char:
        xor     %eax, %eax
        mov     %di, %ax
        tag     TAG_CHAR, %rax
        ret

identity:                       # x
unbox_double:                   # double
        mov     %rdi, %rax
        ret

unbox_int:                      # int
        movsx   %edi, %rax
        ret

unbox_pointer:                  # ptr
        unbox_pointer_internal %rdi
        ret

unbox_vector:                   # vector
        unbox_pointer_internal %rdi
        add     $POINTER_SIZE, %rax
        ret

unbox:                          # value
        prologue
        tagged_jump unbox_jump_table
        return

int_to_string:                  # int
        prologue str
        movsx   %edi, %rdx
        xor     %al, %al
        lea     str(%rsp), %rdi
        call_fn asprintf, %rdi, $int_format, %rdx
        tag     TAG_STRING, str(%rsp)
        return

double_to_string:               # double
        prologue str
        movq    %rdi, %xmm0
        mov     $1, %al         # number of vector var arguments http://www.x86-64.org/documentation/abi.pdf p21
        lea     str(%rsp), %rdi
        mov     $double_format, %rsi
        call    asprintf
        tag     TAG_STRING, str(%rsp)
        return

set:                            # variable, expression
        unbox_pointer_internal %rdi
        mov     %rsi, symbol_table_values(,%rax,POINTER_SIZE)
        mov     $NIL, %rax
        ret

symbol_to_string:               # symbol
        mov     symbol_table_names(,%edi,POINTER_SIZE), %rax
        tag     TAG_STRING, %rax
        ret

string_to_symbol:               # string
        prologue string
        unbox_pointer_internal %rdi
        mov     %rax, string(%rsp)
        mov     (symbol_next_id), %rbx

1:      test    %rbx, %rbx
        je      2f

        dec     %rbx
        mov     symbol_table_names(,%rbx,POINTER_SIZE), %rax

        test    %eax, %eax
        jz      1b
        call_fn strcmp, string(%rsp), %rax
        jnz     1b
        jmp     3f

2:      movq    (symbol_next_id), %rbx
        incq    (symbol_next_id)

        call_fn strdup, string(%rsp)
        mov     %rax, symbol_table_names(,%rbx,POINTER_SIZE)

3:      tag     TAG_SYMBOL, %rbx
        return

lookup_global_symbol:           # symbol
        lookup_global_symbol_internal %edi
        ret

number_to_string:               # z
to_string:                      # value
        prologue
        tagged_jump to_string_jump_table
        return

display:                        # obj
        prologue
        call_fn to_string, %rdi
        unbox_pointer_internal %rax, %rdi
        xor     %al, %al
        call_fn printf, %rdi
        return $NIL

newline:
        call_fn putchar, $'\n
        mov     $NIL, %rax
        ret

is_eq:                          # obj1, obj2
is_eqv:                         # obj1, obj2
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

is_char:                        # obj
        has_tag TAG_CHAR, %rdi
        box_boolean_internal %rax
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
        has_tag TAG_BOOLEAN, %rdi
        box_boolean_internal %rax
        ret

boolean_to_string:              # boolean
        mov     $true_string, %rax
        mov     $false_string, %r11
        test    $C_TRUE, %rdi
        cmovz   %r11, %rax
        tag     TAG_STRING, %rax
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
        box_boolean_internal %rax
        ret

is_procedure:                   # obj
        has_tag TAG_PROCEDURE, %rdi
        box_boolean_internal %rax
        ret

exact_to_inexact:               # z
        cvtsi2sd %edi, %xmm0
        movq    %xmm0, %rax
        ret

inexact_to_exact:               # z
        movq     %rdi, %xmm0
        cvtsd2si %xmm0, %rax
        box_int_internal %eax
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
        binary_op plus addsd add

minus:                          # z1, z2
        binary_op minus subsd sub

multiply:                       # z1, z2
        binary_op multiply mulsd imul

divide:                         # z1, z2
        binary_op divide divsd
divide_int_int:
        cvtsi2sd %edi, %xmm0
        cvtsi2sd %esi, %xmm1
        divsd   %xmm1, %xmm0
        roundsd $ROUNDING_MODE_TRUNCATE, %xmm0, %xmm1
        ucomisd %xmm0, %xmm1
        je      2f
        movq    %xmm0, %rax
        ret
2:      cvtsd2si %xmm1, %rax
        box_int_internal %eax
        ret

equal:                          # z1, z2
        binary_comparsion equals sete sete

less_than:                      # z1, z2
        binary_comparsion less_than setb setl

greater_than:                   # z1, z2
        binary_comparsion greater_than seta setg

less_than_or_equal:             # z1, z2
        binary_comparsion less_than_or_equals setbe setle

greater_than_or_equal:          # z1, z2
        binary_comparsion greater_than_or_equals setae setge

quotient:                       # n1, n2
        integer_division
        box_int_internal %eax
        ret

remainder:                      # n1, n2
        integer_division
        box_int_internal %edx
        ret

modulo:                         # n1, n2
        prologue
        math_library_binary_call fmod
        cvtsd2si %xmm0, %rax
        box_int_internal %eax
        return

floor_:                         # z
        math_library_unary_call_integer floor

ceiling:                        # z
        math_library_unary_call_integer ceil

truncate:                       # z
        math_library_unary_call_integer trunc

round_:                         # z
        math_library_unary_call_integer round

        .irp name, exp, log, sin, cos, tan, asin, acos, atan, sqrt
\name\()_:                      # z
        prologue
        math_library_unary_call \name
        return %xmm0
        .endr

expt:                           # z1, z2
        prologue
        math_library_binary_call pow
        return %xmm0

        .globl cons, car, cdr, length
        .globl display, newline
        .globl is_eq, is_eq_v, is_string, is_boolean, is_char, is_procedure, is_symbol, is_null,
        .globl is_exact, is_inexact, is_integer, is_number, is_pair, is_vector
        .globl make_vector, vector_length, vector_ref, vector_set
        .globl make_string, string_length, string_ref, string_set, string_to_number, string_to_symbol
        .globl char_to_integer, integer_to_char
        .globl neg, plus, minus, multiply, divide, equal, less_than, greater_than, less_than_or_equal, greater_than_or_equal
        .globl floor_, ceiling, truncate, round_, exp_, log_, sin_, cos_, tan_, asin_, acos_, atan_, sqrt_, expt
        .globl quotient, remainder, modulo
        .globl exact_to_inexact, inexact_to_exact
        .globl symbol_to_string, set, lookup_global_symbol
        .globl init_runtime, allocate_code
        .globl int_format, double_format, true_string, false_string, box_int, box_string, unbox
