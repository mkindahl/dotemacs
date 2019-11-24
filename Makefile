compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local
COMPILE.el = emacs -batch -f batch-byte-compile-if-not-done

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: $(COMPILE.el) $<

all: compile-lisp compile-dotemacs

compile-dotemacs:
	$(COMPILE.el) dotemacs.el

compile-lisp: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	mkdir -p $(prefix)/share/emacs/site-lisp
	cp $(compiled) $(prefix)/share/emacs/site-lisp

install-dotemacs:
	cp dotemacs.el $(HOME)/.emacs
