# Emacs Configuration File

This is my configuration file setup, including both a `.emacs` and a
set of other files that I compile to make it work a little speedier.

# Requirements

For Ubuntu 18.04 you can install the dependencies using:

```
sudo apt install clang-tools clang-format perltidy elpa-spinner elpa-lsp-mode elpa-rust-mode
```

In addition, the `lsp-mode` package need to be installed into Emacs.

The `Makefile` installs missing dependencies and packages
automatically on Ubuntu-like systems. If you want to see what is
executed to install these, try

```bash
make -n install-missing-packages install-dependencies
```

## ClangFormat

You need to have `clang-format` installed. Instructions for how to set
it up can be found on the [LLVM clang-format page][CLangFormat], and
it is available under Ubuntu 18.04 in the package `clang-tools`.

## Spinner

Required by `lsp-mode`.

# Installation

To compile and install the files, run:

```bash
make
make install
```

Compiled Emacs-Lisp files will be installed under
`$HOME/.local/share/emacs/site-lisp`, but you can change the prefix to
install in a different location. For example, the following will
install the lisp files under `/usr/local/share/emacs/site-lisp`.

```bash
make install prefix=/usr/local
```

# Checking what is being installed

If you want to see what the differences are between the current
version and the to-be-installed version, you can use the targets
`diff-lisp` (for comparing the compiled Lisp files) and
`diff-dotemacs` (for comparing the `.emacs` file).

The target `diff` will compare both, so the following will show the
differences between the `.emacs` file and the lisp files.

```bash
make diff
```

[CLangFormat]: https://clang.llvm.org/docs/ClangFormat.html
