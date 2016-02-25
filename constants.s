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

        .equ ROUNDING_MODE_TRUNCATE, 0b11

        .equ POINTER_SIZE, 8

        .equ NAN_MASK, 0x7FF8000000000000
        .equ TAG_SHIFT, 45
        .equ TAG_MASK, 0x3f

        .equ PAYLOAD_MASK, (1 << 47) - 1
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
        .equ FALSE, (NAN_MASK | (TAG_BOOLEAN << TAG_SHIFT))
        .equ TRUE, (FALSE | C_TRUE)

        .equ NIL, (NAN_MASK | (TAG_PAIR << TAG_SHIFT) | NULL)

        .equ SPACE_CHAR, 32

        .equ MAX_NUMBER_OF_SYMBOLS, 1024

        .equ BINARY_OP_SHIFT, 4

        .struct 0
pair_car:
        .struct . + POINTER_SIZE
pair_cdr:
        .struct . + POINTER_SIZE
pair_size:
