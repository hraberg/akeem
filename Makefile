ASFLAGS += -g --64 -march=generic64+sse4.1
LDLIBS = -lm

default: akeem

%.o: %.s constants.s macros.s r5rs.scm extensions.scm
	$(AS) $< $(ASFLAGS) -o $@

tests: tests.o lisp.o
	$(CC) $^ $(CFLAGS) $(LDLIBS) -o $@

akeem: repl.o lisp.o
	$(CC) $^ $(CFLAGS) $(LDLIBS) -o $@

# based on http://unix.stackexchange.com/a/79137
run-tests: tests
	./$< | diff -y -W250 test_output.txt - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)' \
		&& echo Tests FAILED \
		|| echo `cat test_output.txt | grep -v ';;;' | wc -l` Tests PASSED

run-tests-catchsegv: tests
	catchsegv ./$<

/usr/bin/rlwrap:
	sudo apt-get install -y rlwrap

run-repl: akeem /usr/bin/rlwrap
	@rlwrap -nm -q "\"" ./$<

/usr/bin/entr:
	sudo apt-get install -y entr

retest: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile -o -name test_output.txt | \
		$< -r $(MAKE) -s run-tests ; done

jit-dissassmble:
	objdump -b binary -D -mi386:x86-64 jit_code/jit_code_*.bin

jit-clean:
	rm -rf jit_code

release: CFLAGS += -s
release: clean akeem

clean:	jit-clean
	rm -f tests akeem *.o

check: run-tests

.PHONY: run-tests run-tests-catchsegv run-repl retest jit_clean jit_dissassmble clean check release
