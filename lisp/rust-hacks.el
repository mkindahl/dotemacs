(require 'lsp-mode)
(require 'rust-mode)


(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook #'lsp-deferred)
  (setq rust-format-on-save t))
