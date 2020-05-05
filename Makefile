compiled = lisp/cc-hacks.elc lisp/org-hacks.elc lisp/rust-hacks.elc \
	lisp/python-hacks.elc
prefix = $(HOME)/.local
COMPILE.el = emacs -batch -f package-initialize -f batch-byte-compile-if-not-done
DIFF = diff -u

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: $(COMPILE.el) $<

all: compile-lisp compile-dotemacs

diff: diff-lisp diff-dotemacs

diff-dotemacs:
	$(DIFF) dotemacs.el $(HOME)/.emacs

diff-lisp:
	$(DIFF) -x '*~' -x '*.el' lisp $(prefix)/share/emacs/site-lisp

compile-dotemacs:
	$(COMPILE.el) dotemacs.el

compile-lisp: $(compiled)

install: install-lisp install-dotemacs

install-lisp: $(compiled)
	mkdir -p $(prefix)/share/emacs/site-lisp
	install -t $(prefix)/share/emacs/site-lisp $(compiled) 

install-dotemacs:
	install -T -C dotemacs.el $(HOME)/.emacs
