# ASM-LISP

A small JIT-ed Lisp written in x86-64 assembler experiment.

Name to change. Only tested on Linux. Written in GNU Assembler.


## Implementation Notes / Ideas

Implement closures by splitting prolog (with closed over vars) from
the body?

Can you implement TCO with a setjmp/longjmp style technique? A
recursive function checks if its on stack, at a tail call, and unwinds
itself instead of creating a new frame.


## References

### Assembler

* http://ref.x86asm.net/coder64-abc.html
* http://www.x86-64.org/documentation/abi.pdf
* https://sourceware.org/binutils/docs/as/
* http://rayseyfarth.com/asm/
* http://bob.cs.sonoma.edu/IntroCompOrg/book.html
* http://www.agner.org/optimize/
* http://github.com/nineties/amber

### Lisp

* http://piumarta.com/software/maru/
* http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf
* http://library.readscheme.org/page1.html
* http://trac.sacrideo.us/wg/raw-attachment/wiki/WikiStart/r7rs.pdf
* https://github.com/kanaka/mal
* http://shenlanguage.org/
