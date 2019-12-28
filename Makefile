compiled = lisp/cc-hacks.elc lisp/org-hacks.elc
prefix = $(HOME)/.local
COMPILE.el = emacs -batch -f batch-byte-compile-if-not-done
DIFF = diff -u

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: $(COMPILE.el) $<

all: compile-lisp compile-dotemacs

diff: diff-lisp diff-dotemacs

diff-dotemacs:
	$(DIFF) dotemacs.el $(HOME)/.emacs

diff-lisp:
	$(DIFF) lisp $(prefix)/share/emacs/site-lisp

compile-dotemacs:
	$(COMPILE.el) dotemacs.el

compile-lisp: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	cp $(compiled) $(prefix)/share/emacs/site-lisp

install-dotemacs:
	cp dotemacs.el $(HOME)/.emacs
