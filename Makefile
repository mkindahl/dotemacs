compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local
COMPILE.el = emacs -batch -f batch-byte-compile-if-not-done

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: $(COMPILE.el) $<

all: compile-lisp

compile-lisp: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	install -t $(prefix)/share/emacs/site-lisp $(compiled) 

install-dotemacs:
	cp dotemacs.el $(HOME)/.emacs
