(require lsp-mode)

;;; Rust settings
(add-hook 'rust-mode-hook #'lsp)

(setq 'rust-format-on-save t)

