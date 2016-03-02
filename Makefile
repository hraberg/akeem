ASFLAGS += -g --64 -march=generic64+sse4.2
CFLAGS += -rdynamic

LDLIBS = -ldl -lm

default: repl

%.o: %.s constants.s macros.s
	$(AS) $< $(ASFLAGS) -o $@

tests: tests.o lisp.o
	$(CC) $^ $(CFLAGS) $(LDLIBS) -o $@

repl: repl.o lisp.o
	$(CC) $^ $(CFLAGS) $(LDLIBS) -o $@

run-tests: tests
	./$< | diff -y -W250 test_output.txt - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)' \
		&& echo Tests FAILED \
		|| echo `cat test_output.txt | grep -v ';;;' | wc -l` Tests PASSED

run-tests-catchsegv: tests
	catchsegv ./$<

/usr/bin/rlwrap:
	sudo apt-get install -y rlwrap

run-repl: repl /usr/bin/rlwrap
	@rlwrap -nm -q "\"" ./$<

/usr/bin/entr:
	sudo apt-get install -y entr

# based on http://unix.stackexchange.com/a/79137
retest: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile -o -name test_output.txt | \
		$< -r $(MAKE) -s run-tests ; done

release: CFLAGS += -s
release: clean repl

clean:
	rm -f tests repl *.o

check: run-tests

.PHONY: run-tests run-tests-catchsegv run-repl retest clean check release
