compiled = lisp/cc-hacks.elc lisp/org-hacks.elc lisp/rust-hacks.elc \
	lisp/python-hacks.elc
prefix = $(HOME)/.local
EMACS-BATCH = emacs -batch -f package-initialize 
COMPILE.el = $(EMACS-BATCH) -f batch-byte-compile-if-not-done
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

install: install-dependencies install-lisp install-dotemacs

install-dependencies:
	sudo apt install clang-tools elpa-spinner
	$(EMACS-BATCH) --eval "(package-install 'lsp-mode)"

install-lisp: $(compiled)
	mkdir -p $(prefix)/share/emacs/site-lisp
	install -t $(prefix)/share/emacs/site-lisp $(compiled) 

install-dotemacs:
	install -T -C dotemacs.el $(HOME)/.emacs
