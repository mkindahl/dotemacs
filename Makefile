%.elc: %.el
	emacs -batch -f batch-byte-compile-if-not-done $<

compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.emacs.d/

all: $(compiled)

install: $(compiled)
	cp $(compiled) $(prefix)/lisp
