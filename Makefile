compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: emacs -batch -f batch-byte-compile-if-not-done $<

all: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	cp $(compiled) $(prefix)/share/emacs/site-lisp

install-dotemacs:
	cp dotemacs.el $(HOME)/.emacs
