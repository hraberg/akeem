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
        .equ TAG_MASK, 0xf << TAG_SHIFT

        .equ PAYLOAD_MASK, (1 << TAG_SHIFT) - 1
        .equ SIGN_BIT, 1 << 63

        .equ TAG_INT, 1 << TAG_SHIFT
        .equ TAG_POINTER, 2 << TAG_SHIFT
        .equ TAG_BOOLEAN, 3 << TAG_SHIFT
        .equ TAG_NIL, 4 << TAG_SHIFT
        .equ TAG_PAIR, 5 << TAG_SHIFT
        .equ TAG_NAN, 8 << TAG_SHIFT

        .equ C_TRUE, 1
        .equ TRUE, (NAN_MASK | TAG_BOOLEAN | C_TRUE)
        .equ FALSE, (NAN_MASK | TAG_BOOLEAN)
        .equ NIL, (NAN_MASK | TAG_NIL)
