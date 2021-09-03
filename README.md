# Emacs Configuration File

This is my configuration file setup, including both a `.emacs` and a
set of other files that I compile to make it work a little speedier.

## Dependencies

Emacs packages:
  - `clang-format` (found in `clang-format` package)
  - `yasnippets` (found in `elpa-yasnippets` package)

Other binaries:
- `clangd` (found in `clang-tools`)
- `rust-analyzer` (downloaded separately)
- Rust source code (for `rust-analyzer` to work. Package `rust-src`)

### Ubuntu 18.04

For Ubuntu 18.04 you can install the dependencies using:

```
apt install clang-format clang-tools elpa-yasnippets rust-src elpa-spinner elpa-rust-mode
```

### Ubuntu 20.04

For Ubuntu 20.04 you can install the dependencies using:

```
apt install clang-format clang-tools elpa-yasnippets elpa-spinner elpa-rust-mode elpa-lsp-mode
```

### Installing Rust Analyzer

Install `curl` to download `rust-analyzer`:
```
apt install curl
```

Download and install `rust-analyzer`:
```
mkdir -p ~/.local/bin
curl -L https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux -o ~/.local/bin/rust-analyzer
chmod +x ~/.local/bin/rust-analyzer

```

## ClangFormat

You need to have `clang-format` installed. Instructions for how to set
it up can be found on the [LLVM clang-format page][CLangFormat], and
it is available under Ubuntu 18.04 in the package `clang-tools`.

## Spinner

Required by `lsp-mode`.

[CLangFormat]: https://clang.llvm.org/docs/ClangFormat.html
