%.elc: %.el
	emacs -batch -f batch-byte-compile-if-not-done $<

compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local

all: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	cp $(compiled) $(prefix)/share/emacs/site-lisp

install-dotemacs:
	cp dotemacs.el $(HOME)/.emacs
