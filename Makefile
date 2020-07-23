compiled = lisp/cc-hacks.elc lisp/org-hacks.elc lisp/rust-hacks.elc \
	lisp/python-hacks.elc lisp/sql-hacks.elc
prefix = $(HOME)/.local
packages = clang-tools elpa-spinner
missing = $(shell dpkg-query --show --showformat='${binary:Package}:${db:Status-Status}\n' $(packages) | grep installed | cut -d: -f1)
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

install-missing-packages:
ifdef $(missing)
	sudo apt install $(missing)
endif

install-dependencies: install-missing-packages
	$(EMACS-BATCH) --eval "(package-install 'lsp-mode)"

install-lisp: $(compiled)
	mkdir -p $(prefix)/share/emacs/site-lisp
	install -t $(prefix)/share/emacs/site-lisp $(compiled) 

install-dotemacs:
	install -T -C dotemacs.el $(HOME)/.emacs
