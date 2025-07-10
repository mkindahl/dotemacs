;;;; From https://github.com/golang/tools/blob/master/gopls/doc/editor/emacs.md
(require 'lsp-mode)

(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'lsp-deferred)

  ;; Set up before-save hooks to format buffer and add/delete imports.
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

