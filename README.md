# Akeem

*When you think of garbage, think of Akeem.*
-- [Prince Akeem of Zamunda](http://www.imdb.com/title/tt0094898/)

Akeem is a small JIT-ed subset of R5RS Scheme written in x86-64
assembler as an experiment.

Only tested on Linux. Written in GNU Assembler using AT&T syntax.


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

If symbol 0 is #f and 1 #t booleans can be symbols while still handle
logical operations.

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

## Road Map:

We aim to implement a subset of R5RS.

### Milestone 1

[milestone1.txt]

* Parser
** integers and doubles.
** No define-syntax
* JIT Compiler
** Based on piecing together snippets.
** Mainly stack based.
** Single arity.
** Lambdas.
* Runtime
** NaN boxing.
** [SRFI-6](http://srfi.schemers.org/srfi-6/srfi-6.html)
* R5RS syntax / procedures
** 4.1, 5.1, 6.1, 6.2.5, 6.2.6,
   6.3.2, 6.3.3, 6.3.5, 6.3.6, 6.6.2, 6.6.3
** library procedures initially in Assembler to bootstrap.

### Milestone 2

* GC
** Simple Mark & Sweep.
* JIT Compiler
** Multiple arities, varargs
** Register allocation.
** Self-call TCO.
** Closures.
* Runtime
* R5RS syntax / procedures
** 5.2, 6.3.1, 6.3.4, 6.5, 6.6.1, 6.6.4
* R5RS library syntax / procedures
** 4.2, 6.2.5, 6.2.6, 6.3.5, 6.3.6, 6.6.2, 6.6.3
** [SRFI 1](http://srfi.schemers.org/srfi-1/srfi-1.html)
** rewrite most library procedures in Scheme.

### Milestone 3

* Parser
** define-syntax
* GC
** Generational.
* JIT Compiler
** Macros
** call/cc
** Sibling call optimization.
* Runtime
* R5RS
** 4.3, 5.3, 6.4
** [SRFI 4](http://srfi.schemers.org/srfi-4/srfi-4.html)
** [SRFI 9](http://srfi.schemers.org/srfi-9/srfi-9.html)
** [SRFI 23](http://srfi.schemers.org/srfi-23/srfi-23.html)
** [SRFI 55](http://srfi.schemers.org/srfi-55/srfi-55.html)
** [SRFI 69](http://srfi.schemers.org/srfi-69/srfi-69.html)

### Milestone 4

R7RS "small" language.


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
* https://dspace.mit.edu/handle/1721.1/5600
* http://www.schemers.org/Documents/Standards/R5RS/r5rs.pdf
* http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf
* https://github.com/kanaka/mal
* http://shenlanguage.org/

## License

Copyright © 2016 Håkan Råberg

Distributed under the MIT License.
