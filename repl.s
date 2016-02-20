        .include "constants.s"
        .include "macros.s"

        .data
welcome_message:
        .string "Welcome to ASM Lisp!"
prompt:
        .string "> "
line_format:
        .string "%as"

        .text
readline:
        .equ line, 0
        prologue 1
        lea     line(%rsp), %rax
        call_fn scanf, $line_format, %rax
        call_fn box_pointer, line(%rsp)
        epilogue %rax

main:
        prologue
        call_fn box_pointer, $welcome_message
        call_fn println, %rax

1:      call_fn box_pointer, $prompt
        call_fn print, %rax

        call_fn readline
        call_fn println, %rax

        jmp     1b

2:      epilogue $0
        .globl main
