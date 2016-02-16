CFLAGS += -m64 -rdynamic -ldl

lisp:  lisp.s macros.s
	gcc $< $(CFLAGS) -o $@

run: lisp
	./$<

/usr/bin/entr:
	sudo apt-get install -y entr

watch: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile | $< -r make run; done

# based on http://unix.stackexchange.com/a/79137
retest: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile -o -name test_output.txt | \
		$< -r sh -c "make -s run | diff -y -W250 test_output.txt - | expand | grep --color=always -nEC1 '^.{123} [|<>]( |$$)' && echo Tests FAILED || echo `wc -l < test_output.txt` Tests PASSED" ; done

clean:
	rm lisp

.PHONY: run watch clean
