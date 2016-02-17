CFLAGS += -rdynamic -ldl

default: repl

.s.o:
	as --64 $< -o $@

tests: tests.o lisp.o
	gcc $^ $(CFLAGS) -o $@

repl: repl.o lisp.o
	gcc $^ $(CFLAGS) -o $@

run-tests: tests
	./$< | diff -y -W250 test_output.txt - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)' \
		&& echo Tests FAILED || echo `wc -l < test_output.txt` Tests PASSED

/usr/bin/rlwrap:
	sudo apt-get install -y rlwrap

run-repl: repl /usr/bin/rlwrap
	@rlwrap -n -pBlue -m ./$<

/usr/bin/entr:
	sudo apt-get install -y entr

# based on http://unix.stackexchange.com/a/79137
retest: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile -o -name test_output.txt | \
		$< -r make -s run-tests ; done

clean:
	rm -f tests *.o

check: run-tests

.PHONY: run-tests run-repl retest clean check
