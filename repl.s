        .include "macros.s"

        .text

        .globl main

main:
        prologue
        call_fn init_runtime

        call_fn box_string, $welcome_message
        call_fn display, %rax
        call_fn newline

        call_fn box_string, $prompt
        mov     %rax, %rbx
1:      call_fn display, %rbx

        call_fn read
        call_fn write, %rax
        call_fn newline
        jmp     1b

2:      return  $0

        .data
welcome_message:
        .string "Welcome to Akeem Scheme."
prompt:
        .string "> "
