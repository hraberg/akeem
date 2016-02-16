CFLAGS += -m64 -rdynamic -ldl

lisp:  lisp.s macros.s
	gcc $< $(CFLAGS) -o $@

run: lisp
	./$<

/usr/bin/entr:
	sudo apt-get install -y entr

watch: /usr/bin/entr
	while true; do find . -name '*.s' -o -name Makefile | $< -r make run; done

clean:
	rm lisp

.PHONY: run watch clean
