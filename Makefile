%.elc: %.el
	emacs -batch -f batch-byte-compile-if-not-done $<

compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local

all: $(compiled)

install: $(compiled)
	cp $(compiled) $(prefix)/share/emacs/site-lisp
