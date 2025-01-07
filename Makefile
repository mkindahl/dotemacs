sources = $(wildcard lisp/*.el)
compiled = $(addsuffix .elc, $(basename $(sources)))
prefix = $(HOME)/.local
packages = clang-tools elpa-spinner perltidy
missing = $(shell dpkg-query --show --showformat='${binary:Package}:${db:Status-Status}\n' $(packages) | grep installed | cut -d: -f1)
EMACS = emacs
COMPILE.el = $(EMACS) -batch -f package-initialize -f batch-byte-compile-if-not-done
DIFF = diff -u

%.elc: %.el
	EMACSLOADPATH=$(prefix)/share/emacs/site-lisp: $(COMPILE.el) $<

all: compile-lisp compile-dotemacs

clean: clean-lisp clean-dotemacs

clean-dotemacs:
	rm dotemacs.elc

clean-lisp:
	rm $(compiled)

diff: diff-lisp diff-dotemacs

diff-dotemacs:
	$(DIFF) $(HOME)/.emacs dotemacs.el

diff-lisp:
	$(DIFF) -x '*~' -x '*.elc' $(prefix)/share/emacs/site-lisp lisp 

compile-dotemacs:
	$(COMPILE.el) dotemacs.el

compile-lisp: $(compiled)

install: install-dependencies install-lisp install-dotemacs

install-missing-packages:
ifdef $(missing)
	sudo apt install $(missing)
endif

install-dependencies: install-missing-packages
	$(EMACS) -batch -f package-initialize --eval "(package-install 'lsp-mode)"

install-lisp: $(compiled)
	mkdir -p $(prefix)/share/emacs/site-lisp
	install -t $(prefix)/share/emacs/site-lisp $(compiled) $(sources)

install-dotemacs:
	install -T -C dotemacs.el $(HOME)/.emacs
