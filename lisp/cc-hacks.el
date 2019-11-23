(require 'clang-format)
(require 'cquery)
		
(defun mk/symbol-from-filename (full-name)
  (let* ((root-dir (expand-file-name (locate-dominating-file full-name ".git")))
	 (upcase-name (upcase (string-remove-prefix root-dir (expand-file-name full-name)))))
    (message (format "upcase-name: %s" upcase-name))
    (dotimes (i (length upcase-name))
      (let ((c (aref upcase-name i)))
        (unless (or (and (>= c ?A) (<= c ?Z))
                    (and (>= c ?0) (<= c ?9)))
          (aset upcase-name i ?_))))
    (concat upcase-name "_")))

(defun mk/add-header-include-guard ()
  (interactive)
  (let ((pp-sym (cc-hacks-symbol-from-filename (buffer-file-name))))
    (save-excursion
      (goto-char (point-min))
      (c-forward-comments)
      (if (not (looking-at "#ifndef[[:blank:]]+\\([[:alnum:]_]+\\)\n#define[[:blank:]]+\\1"))
          (progn
            (goto-char (point-min))
            (c-forward-comments)
            (insert (concat "#ifndef " pp-sym "\n" "#define " pp-sym "\n\n"))
            (goto-char (point-max))
            (insert (concat "\n#endif  /* " pp-sym " */\n")))))))

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

(with-eval-after-load 'cquery
  (setq cquery-executable
	(expand-file-name "~/.local/bin/cquery")))

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
