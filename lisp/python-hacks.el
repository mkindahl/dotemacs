(require 'lsp-mode)

;; Python mode settings
(add-hook 'python-mode-hook #'lsp-deferred)
