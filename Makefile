ASFLAGS += -g --64 -march=generic64+sse4.2
LDLIBS = -lm

AKEEM_HOME = $(PWD)
AKEEM = $(AKEEM_HOME)/akeem

RACKET = `which racket`
RACKET_HOME = ../racket
RACKET_BENCHMARKS_HOME = $(RACKET_HOME)/pkgs/racket-benchmarks/tests/racket/benchmarks/common
RACKET_BENCHMARKS = ctak nboyer nfa nothing nqueens puzzle scheme-c2 scheme-c takr2 tak takr
RUN_RACKET_BENCHMARKS = true

default: akeem

%.o: %.s constants.s macros.s boot.scm r7rs.scm init.scm Makefile
	$(AS) $< $(ASFLAGS) -o $@

akeem: lisp.o
	$(CC) $^ $(LDLIBS) -o $@

# based on http://unix.stackexchange.com/a/79137
run-tests: akeem
	./$<  tests.scm 2>&1 | diff -y -W250 tests.out - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)'; \
	if [ $$? -eq 0 ] ; then \
		echo Tests FAILED ; false ; \
	else \
		echo `cat tests.out | grep -v ';;;' | wc -l` Tests PASSED ; \
	fi

run-tests-catchsegv: akeem
	catchsegv ./$< tests.scm

/usr/bin/rlwrap:
	sudo apt-get install -y rlwrap

run-repl: akeem /usr/bin/rlwrap
	@rlwrap -nm -q "\"" ./$<

/usr/bin/entr:
	sudo apt-get install -y entr

retest: /usr/bin/entr
	while true; do find . -name '*.s' -o -name '*.scm' -o -name Makefile -o -name tests.out | \
		$< -r $(MAKE) -s run-tests ; done

valgrind: clean akeem
	if [ -n "`which valgrind`" ] ; then \
		echo "(exit 0)" | valgrind --suppressions=akeem.supp --error-exitcode=1 -q $(AKEEM) > /dev/null ; \
	else \
		echo "valgrind not found, skipping." ; \
	fi

benchmark: clean akeem
	cd $(RACKET_BENCHMARKS_HOME) ; \
	for test in $(RACKET_BENCHMARKS) ; do \
		test -n '$(RUN_RACKET_BENCHMARKS)' && (echo $$test.rkt ; $(RACKET) $$test.rkt) ; \
		echo $$test.sch ; $(AKEEM) $(AKEEM_HOME)/benchmarks-prelude.scm $$test.sch ; \
	done

profile: RACKET_BENCHMARKS = nqueens
profile: RUN_RACKET_BENCHMARKS =
profile: CFLAGS += -pg
profile: benchmark
	gprof -b $(AKEEM) $(RACKET_BENCHMARKS_HOME)/gmon.out

profile-clean:
	rm -f $(RACKET_BENCHMARKS_HOME)/gmon.out

jit-dissassmble:
	objdump -b binary -D -mi386:x86-64 jit_code/jit_code_*.bin

jit-clean:
	rm -rf jit_code

release: clean akeem run-tests valgrind
	strip $(AKEEM)

clean:	jit-clean profile-clean
	rm -f $(AKEEM) *.o

check: run-tests

docker:
	docker build -t akeem .

run-docker: docker
	docker run --rm -i -t akeem

run-docker-shell: docker
	docker run --rm -i -t akeem bash

.PHONY: run-tests run-tests-catchsegv run-repl retest benchmark profile jit-clean jit-dissassmble clean check release docker run-docker run-docker-shell
.SILENT: run-tests retest benchmark profile valgrind
