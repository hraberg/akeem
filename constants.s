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

        .equ POINTER_SIZE, 8

        .equ NAN_MASK, 0x7FF8000000000000
        .equ TAG_SHIFT, 47
        .equ TAG_MASK, 0x7

        .equ PAYLOAD_MASK, (1 << TAG_SHIFT) - 1
        .equ SIGN_BIT, 63

        .equ TAG_INT, 1
        .equ TAG_STRING, 2
        .equ TAG_SYMBOL, 3
        .equ TAG_PAIR, 4
        .equ TAG_VECTOR, 5

        .equ C_TRUE, 1
        .equ FALSE, (NAN_MASK | (TAG_SYMBOL << TAG_SHIFT))
        .equ TRUE, (FALSE | C_TRUE)

        .equ NIL, (NAN_MASK | (TAG_PAIR << TAG_SHIFT) | NULL)
