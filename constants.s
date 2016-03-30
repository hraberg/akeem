        .equ NULL, 0
        .equ EOF, -1
        .equ PAGE_SIZE, 4096

        ## setjmp.h
        .equ JMP_BUF_SIZE, 200

        ## sys/mman.h
        .equ PROT_READ, 0x1
        .equ PROT_WRITE, 0x2
        .equ PROT_EXEC, 0x4
        .equ MAP_PRIVATE, 0x02
        .equ MAP_ANONYMOUS, 0x20

        ## stdio.h
        .equ SEEK_SET, 0

        ## time.h
        .equ CLOCKS_PER_SEC, 1000000

        ## unistd.h
        .equ F_OK, 0

        .equ ROUNDING_MODE_TRUNCATE, 0b11

        .equ CPUID_FEATURE_INFORMATION, 1
        .equ SSE4_1, 1 << 19
        .equ SSE4_2, 1 << 20

        .equ POINTER_SIZE, 8
        .equ INT_SIZE, POINTER_SIZE / 2
        .equ WORD_SIZE, INT_SIZE / 2

        .equ POINTER_SIZE_SHIFT, 3

        .equ NAN_MASK, 0x7FF8000000000000
        .equ TAG_SHIFT, 45
        .equ TAG_MASK, (1 << 6) - 1
        .equ POINTER_TAG_MASK, ~(TAG_SYMBOL - 1)

        .equ PAYLOAD_SHIFT, 64 - 48
        .equ SIGN_BIT, 63

        .equ TAG_DOUBLE, 0

        .equ TAG_BOOLEAN, 1
        .equ TAG_BYTE, 2
        .equ TAG_CHAR, 3
        .equ TAG_INT, 4

        .equ TAG_SYMBOL, 8
        .equ TAG_PROCEDURE, 16
        .equ TAG_PORT, 24

        .equ TAG_STRING, 32
        .equ TAG_PAIR, 40
        .equ TAG_VECTOR, 48
        .equ TAG_OBJECT, 56

        .equ TAG_BYTEVECTOR, 64

        .equ C_TRUE, 1
        .equ C_FALSE, 0
        .equ FALSE, (NAN_MASK | (TAG_BOOLEAN << TAG_SHIFT))
        .equ TRUE, (FALSE | C_TRUE)

        .equ NIL, (NAN_MASK | (TAG_PAIR << TAG_SHIFT) | NULL)
        .equ VOID, (NAN_MASK | (TAG_OBJECT << TAG_SHIFT) | NULL)

        .equ EOF_OBJECT, (NAN_MASK | (TAG_CHAR << TAG_SHIFT) | 0xffffffff)
        .equ NEWLINE_CHAR, (NAN_MASK | (TAG_CHAR << TAG_SHIFT) | 10)

        .equ ZERO_INT, (NAN_MASK | (TAG_INT << TAG_SHIFT) | 0)

        .equ BINARY_RADIX_INT, (NAN_MASK | (TAG_INT << TAG_SHIFT) | 2)
        .equ OCTAL_RADIX_INT, (NAN_MASK | (TAG_INT << TAG_SHIFT) | 8)
        .equ DECIMAL_RADIX_INT, (NAN_MASK | (TAG_INT << TAG_SHIFT) | 10)
        .equ HEX_RADIX_INT, (NAN_MASK | (TAG_INT << TAG_SHIFT) | 16)

        .equ MAX_NUMBER_OF_SYMBOLS, 1024
        .equ POINTER_STACK_INITIAL_SIZE, 8 * 1024

        .equ CODE_SPACE_SIZE, 512 * 1024 * 1024

        .equ CHAR_TABLE_SIZE, 256
        .equ CHAR_PREFIX_LENGTH, 2

        .equ BINARY_OP_SHIFT, 4
        .equ BINARY_OP_MASK, 4 - 1
        .equ BINARY_OP_INT_INT, (1 << 1 | 1)

        .equ APPLY_JUMP_ALIGNMENT, 2

        .equ GC_MARK_BIT, 0

        .equ NUMBER_OF_REGISTERS, 16
        .equ MAX_REGISTER_ARGS, 6

        .equ MAX_CLOSURE_ENVIRONMENT_SIZE, 64

        .equ RET_SIZE, 1

        .equ RAX, 0
        .equ RCX, 1
        .equ RDX, 2
        .equ RBX, 3
        .equ RSP, 4
        .equ RBP, 5
        .equ RSI, 6
        .equ RDI, 7
        .irp reg, 8, 9, 10, 11, 12, 13, 14, 15
        .equ R\reg, \reg
        .endr

        .equ MICROSECONDS_PER_SEC, 1000000

        .equ LOG_JIT, C_FALSE

        .struct 0
header_object_mark:
        .struct . + WORD_SIZE
header_object_type:
        .struct . + WORD_SIZE
header_object_size:
        .struct . + INT_SIZE
header_size:

        .struct header_size
pair_car:
        .struct . + POINTER_SIZE
pair_cdr:
        .struct . + POINTER_SIZE
pair_size:

        .struct 0
stack_bottom:
        .struct . + POINTER_SIZE
stack_top_offset:
        .struct . + POINTER_SIZE
stack_max_size:
        .struct . + POINTER_SIZE
stack_size:
