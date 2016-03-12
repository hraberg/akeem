        .include "macros.s"

        .text

        .globl main

main:
        prologue

        call_fn init_runtime, %rsp, %rdi, %rsi, $C_TRUE

        call_fn current_command_line_arguments
        call_fn vector_length, %rax
        cmp     $1, %eax
        jg      2f

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

        .if REPL_DISPLAY_CLASS
        call_fn class_of, %rbx
        call_fn display, %rax
        call_fn newline
        .endif

        call_fn gc

        jmp     1b

2:      call_fn current_command_line_arguments
        mov     %rax, %rbx
        call_fn box_int, $1
        call_fn vector_ref, %rbx, %rax
        call_fn load, %rax
        return  $0
