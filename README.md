# Emacs Configuration

## Dependencies

Emacs packages:
  - `clang-format` (found in `clang-format` package)
  - `yasnippets` (found in `elpa-yasnippets` package)

Other binaries:
- `clangd` (found in `clang-tools`)
- `rust-analyzer` (downloaded separately)
- Rust source code (for `rust-analyzer` to work. Package `rust-src`)

### Ubuntu 18.04

```
apt install clang-format clang-tools elpa-yasnippets rust-src
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
