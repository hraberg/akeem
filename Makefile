DEBUG = true

ifdef DEBUG
DEBUGFLAGS = -g
endif
CFLAGS += -rdynamic -ldl -lm $(DEBUGFLAGS)

default: repl

%.o: %.s constants.s macros.s
	as --64 $(DEBUGFLAGS) $< -o $@

tests: tests.o lisp.o
	gcc $^ $(CFLAGS) $(DEBUGFLAGS) -o $@

repl: repl.o lisp.o
	gcc $^ $(CFLAGS) -o $@

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

release: clean
	$(MAKE) DEBUG= repl

clean:
	rm -f tests repl *.o

check: run-tests

.PHONY: run-tests run-tests-catchsegv run-repl retest clean check release
