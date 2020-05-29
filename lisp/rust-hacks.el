(require 'lsp-mode)
(require 'rust-mode)
(require 'lsp-clients)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'lsp)
  (rust-enable-format-on-save))
