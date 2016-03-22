ASFLAGS += -g --64 -march=generic64+sse4.1
LDLIBS = -lm

AKEEM_HOME = $(PWD)
AKEEM = $(AKEEM_HOME)/akeem

RACKET = `which racket`
RACKET_HOME = ../racket
RACKET_BENCHMARKS_HOME = $(RACKET_HOME)/pkgs/racket-benchmarks/tests/racket/benchmarks/common
RACKET_BENCHMARKS = ctak nothing nqueens puzzle tak takr
RUN_RACKET_BENCHMARKS = true

default: akeem

%.o: %.s constants.s macros.s boot.scm r5rs.scm r7rs.scm init.scm
	$(AS) $< $(ASFLAGS) -o $@

akeem: lisp.o
	$(CC) $^ $(CFLAGS) $(LDLIBS) -o $@

# based on http://unix.stackexchange.com/a/79137
run-tests: akeem
	./$<  tests.scm | diff -y -W250 tests.out - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)' \
		&& echo Tests FAILED \
		|| echo `cat tests.out | grep -v ';;;' | wc -l` Tests PASSED

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

benchmark: akeem
	cd $(RACKET_BENCHMARKS_HOME) ; \
	for test in $(RACKET_BENCHMARKS) ; do \
		test -n '$(RUN_RACKET_BENCHMARKS)' && (echo $$test.rkt ; $(RACKET) $$test.rkt) ; \
		echo $$test.sch ; $(AKEEM) $(AKEEM_HOME)/benchmarks-prelude.scm $$test.sch ; \
	done

profile: RACKET_BENCHMARKS = nqueens
profile: RUN_RACKET_BENCHMARKS =
profile: CFLAGS += -pg
profile: clean akeem benchmark
	gprof -b $(AKEEM) $(RACKET_BENCHMARKS_HOME)/gmon.out

jit-dissassmble:
	objdump -b binary -D -mi386:x86-64 jit_code/jit_code_*.bin

jit-clean:
	rm -rf jit_code

release: CFLAGS += -s
release: clean akeem

clean:	jit-clean
	rm -f akeem *.o $(RACKET_BENCHMARKS_HOME)/gmon.out

check: run-tests

.PHONY: run-tests run-tests-catchsegv run-repl retest benchmark profile jit-clean jit-dissassmble clean check release
.SILENT: benchmark profile
