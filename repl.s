        .include "macros.s"

        .text

        .globl main

main:
        prologue

        call_fn init_runtime, %rsp, $C_TRUE

        intern_string welcome_message, "Welcome to Akeem Scheme."
        intern_string prompt, "> "

        call_fn display, welcome_message
        call_fn newline

1:      call_fn display, prompt

        call_fn read
        call_fn eval, %rax
        mov     %rax, %rbx

        call_fn write, %rax
        call_fn newline

        call_fn class_of, %rbx
        call_fn display, %rax
        call_fn newline

        call_fn gc

        jmp     1b

2:      return  $0
