        .include "macros.s"

        .text

        .globl main

main:
        prologue

        call_fn init_runtime, %rsp, %rdi, %rsi, $REPL_LOG_JIT
        call_fn parse_command_line_arguments

        intern_string welcome_message, "Welcome to Akeem Scheme."
        intern_string prompt, "> "

        call_fn display, welcome_message
        call_fn display, $NEWLINE_CHAR

1:      call_fn display, prompt

        call_fn read
        call_fn eval, %rax
        mov     %rax, %rbx

        call_fn write, %rax
        call_fn display, $NEWLINE_CHAR

        .if REPL_DISPLAY_CLASS
        call_fn class_of, %rbx
        call_fn display, %rax
        call_fn display, $NEWLINE_CHAR
        .endif

        call_fn gc

        jmp     1b

        return  $0
