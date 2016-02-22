# ASM-LISP

A small JIT-ed Lisp written in x86-64 assembler experiment.

Name to change. Only tested on Linux. Written in GNU Assembler.


## Implementation Notes / Ideas

Implement closures by splitting prolog (with closed over vars) from
the body?

Can you implement TCO with a setjmp/longjmp style technique? A
recursive function checks if its on stack, at a tail call, and unwinds
itself instead of creating a new frame.

Generated functions need to use the stack. Either by calculating the
max size, or by using pushq %rbp, movq %rsp, %rbp so we have a stable
reference to the frame.

Potentially use %rax for number of arguments, similar to the x86-64
ABI. Have different functions with different arities with common
wrapper. Varargs potentially marked as negative?

Symbol table where each symbol gets an unique id (and offset)
symbol_table_entry has pointer to both the value and the symbol, and
potentially also unboxed version of the value pointer.

Use callee saved registers %rbx (potentially %rbp) and %r12-%r15 for
local variables. Save used registers in prologue for functions. Save
used registers at start of let block if they are used afterwards.

Number spilled local symbols negatively, so access is -(local_id *
8)%rbp. Potentially implement a variant of Linear Scan Register
Allocation:
http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf

For calls, push each argument, skipping constants and local variables,
then load the argument registers in order, either from the stack or by
local variable reference.

Closures modifying a local variable requires the variable to be moved
to the heap.

For disassembling generated raw code:

```bash
objdump -b binary -D -mi386:x86-64 code.bin

```

## References

### Assembler

* http://ref.x86asm.net/coder64-abc.html
* http://www.x86-64.org/documentation/abi.pdf
* https://sourceware.org/binutils/docs/as/
* http://rayseyfarth.com/asm/
* http://bob.cs.sonoma.edu/IntroCompOrg/book.html
* http://www.agner.org/optimize/
* http://www.avabodh.com/cin/cin.html
* http://github.com/nineties/amber

### Lisp

* http://piumarta.com/software/maru/
* http://piumarta.com/papers/S3-2010.pdf
* http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
* http://library.readscheme.org/page1.html
* http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf
* https://github.com/kanaka/mal
* http://shenlanguage.org/
