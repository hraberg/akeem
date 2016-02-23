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
        prologue line
        lea     line(%rsp), %rax
        call_fn scanf, $line_format, %rax
        call_fn box_string, line(%rsp)
        return

main:
        prologue
        call_fn init_runtime

        call_fn box_string, $welcome_message
        call_fn display, %rax
        call_fn newline

1:      call_fn box_string, $prompt
        call_fn display, %rax

        call_fn readline
        call_fn display, %rax
        call_fn newline

        jmp     1b

2:      return  $0

        .globl main
