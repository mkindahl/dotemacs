(require 'clang-format)

;; Show trailing whitespace in red
(defun turn-on-show-trailing-whitespace ()
  (setq show-trailing-whitespace t))

(defun mk/clang-format-buffer (style)
  "Call clang-format-buffer and print a debug message"
  (message "Formatting %s using %s with style '%s'"
	   buffer-file-name
	   clang-format-executable
	   style)
  (clang-format-buffer style))

(defun mk/format-buffer ()
  "Format buffer by removing trailing whitespace and formatting
the buffer using clang-format.

Use the .clang-format file, if one exists in some parent
directory. Otherwise, use the value of 'clang-format-style."
  (save-excursion
    (delete-trailing-whitespace (point-min) (point-max))
    (mk/clang-format-buffer (if (locate-dominating-file "." ".clang-format") "file" clang-format-style))
    nil))

(defun turn-on-format-buffer ()
  "Format buffer before saving."
  (add-hook 'write-contents-functions 'mk/format-buffer))

(with-eval-after-load 'lsp-mode
  (add-hook 'c-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp))

(with-eval-after-load 'c++-mode
  (add-hook 'c++-mode-hook #'turn-on-format-buffer)
  (add-hook 'c++-mode-hook #'turn-on-show-trailing-whitespace))

(with-eval-after-load 'c-mode
  (add-hook 'c-mode-hook #'turn-on-format-buffer)
  (add-hook 'c-mode-hook #'turn-on-show-trailing-whitespace))
