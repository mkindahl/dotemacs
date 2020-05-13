# Emacs Configuration File

This is my configuration file setup, including both a `.emacs` and a
set of other files that I compile to make it work a little speedier.

# Requirements

For Ubuntu 18.04 you can install the dependencies using:

```
sudo apt install clang-tools elpa-spinner
```

## ClangFormat

You need to have `clang-format` installed. Instructions for how to set
it up can be found on the [LLVM clang-format page][CLangFormat], and
it is available under Ubuntu 18.04 in the package `clang-tools`.

## Spinner

Required by `lsp-mode`.

[CLangFormat]: https://clang.llvm.org/docs/ClangFormat.html
