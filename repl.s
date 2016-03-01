        .include "macros.s"

        .text

        .globl main

main:
        prologue

        call_fn init_runtime, %rsp
        intern_string welcome_message, "Welcome to Akeem Scheme."
        intern_string prompt, "> "

        call_fn display, welcome_message
        call_fn newline

1:      call_fn display, prompt

        call_fn read
        call_fn write, %rax
        call_fn newline

        jmp     1b

2:      return  $0
