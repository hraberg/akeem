        .equ NULL, 0
        .equ EOF, -1
        .equ PAGE_SIZE, 4096

        ## dlfcn.h
        .equ RTLD_DEFAULT, 0

        ## sys/mman.h
        .equ PROT_READ, 0x1
        .equ PROT_WRITE, 0x2
        .equ PROT_EXEC, 0x4
        .equ MAP_PRIVATE, 0x02
        .equ MAP_ANONYMOUS, 0x20

        ## stdio.h
        .equ SEEK_SET, 0

        .equ ROUNDING_MODE_TRUNCATE, 0b11

        .equ POINTER_SIZE, 8
        .equ INT_SIZE, POINTER_SIZE / 2
        .equ WORD_SIZE, INT_SIZE / 2

        .equ POINTER_SIZE_SHIFT, 3

        .equ NAN_MASK, 0x7FF8000000000000
        .equ TAG_SHIFT, 45
        .equ TAG_MASK, 0x3f
        .equ POINTER_TAG_MASK, ~(TAG_SYMBOL - 1)

        .equ PAYLOAD_MASK, (1 << 47) - 1
        .equ INT_MASK, (1 << 32) - 1
        .equ SIGN_BIT, 63

        .equ TAG_DOUBLE, 0

        .equ TAG_BOOLEAN, 1
        .equ TAG_BYTE, 2
        .equ TAG_CHAR, 3
        .equ TAG_INT, 4

        .equ TAG_SYMBOL, 8
        .equ TAG_STRING, 16
        .equ TAG_PAIR, 24
        .equ TAG_VECTOR, 32
        .equ TAG_PROCEDURE, 40
        .equ TAG_PORT, 48
        .equ TAG_OBJECT, 56

        .equ C_TRUE, 1
        .equ C_FALSE, 0
        .equ FALSE, (NAN_MASK | (TAG_BOOLEAN << TAG_SHIFT))
        .equ TRUE, (FALSE | C_TRUE)

        .equ NIL, (NAN_MASK | (TAG_PAIR << TAG_SHIFT) | NULL)

        .equ NEWLINE_CHAR, (NAN_MASK | (TAG_CHAR << TAG_SHIFT) | 10)
        .equ SPACE_CHAR, (NAN_MASK | (TAG_CHAR << TAG_SHIFT) | 32)

        .equ MAX_NUMBER_OF_SYMBOLS, 1024
        .equ OBJECT_SPACE_INITIAL_SIZE, 8 * 1024

        .equ BINARY_OP_SHIFT, 4

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
