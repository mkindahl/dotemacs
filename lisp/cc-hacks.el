;;; Hacks to configure C/C++ mode

(require 'clang-format)
(require 'cc-mode)
(require 'lsp-mode)
;(require 'lsp-clients)

(defun mk/symbol-from-filename (full-name)
  "Generate a preprocessor symbol from the file name."
  (let* ((root-dir (expand-file-name (locate-dominating-file full-name ".git")))
	 (upcase-name (upcase (string-remove-prefix root-dir (expand-file-name full-name)))))
    (dotimes (i (length upcase-name))
      (let ((c (aref upcase-name i)))
        (unless (or (and (>= c ?A) (<= c ?Z))
                    (and (>= c ?0) (<= c ?9)))
          (aset upcase-name i ?_))))
    (concat upcase-name "_")))

(defun mk/add-header-include-guard ()
  "Add header include guard to file."
  (interactive)
  (let ((pp-sym (mk/symbol-from-filename (buffer-file-name))))
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

(defun mk/format-cc-buffer ()
  "Format buffer by removing trailing whitespace and formatting
the buffer using clang-format.

Use the .clang-format file, if one exists in some parent
directory. Otherwise, use the value of clang-format-style."
  (save-excursion
    (delete-trailing-whitespace (point-min) (point-max))
    (mk/clang-format-buffer
     (if (locate-dominating-file "." ".clang-format")
	 "file"
       clang-format-style))
    nil))

(defun mk/wrap-code (tag start finish)
  (goto-char start)
  (let ((symbol-point (insert (concat "#" tag " "))))
    (goto-char finish)
    (insert (concat "#endif"))
    (goto-char symbol-point)))

(defun mk/comment-out (start finish)
  (save-excursion
    (goto-char start)
    (insert (concat "#if 0"))
    (goto-char finish)
    (insert (concat "#endif"))))

(defun mk/ifdef-code (start finish)
  (interactive "r")
  (mk/wrap-code "ifdef" start finish))

(defun mk/ifndef-code (start finish)
  (interactive "r")
  (mk/wrap-code "ifndef" start finish))

(defun mk/add-custom-cc-keys ()
  "Return a function that when executed adds custom keys to the
provided key-map."
  (define-key c-mode-base-map "\C-chw" 'mk/add-header-include-guard)
  (define-key c-mode-base-map "\C-c#0" 'mk/comment-out)
  (define-key c-mode-base-map "\C-c#d" 'mk/ifdef-code)
  (define-key c-mode-base-map "\C-c#n" 'mk/ifndef-code)
  (define-key c-mode-base-map "\M-." 'lsp-find-definition)
  (define-key c-mode-base-map "\M-?" 'lsp-find-references))

(defun mk/enable-lsp ()
  "Enable LSP for the buffer.
We hard-code to not enable it for bpftrace-mode since it has no good hooks."
  (unless (string-equal major-mode "bpftrace-mode") (lsp-deferred)))

(defun enable-global-format-cc-buffer ()
  "Enable formatting buffer before saving.
We hard-code to not enable it for bpftrace-mode since it has no good hooks."
  (unless (string-equal major-mode "bpftrace-mode")
    (message "Enabling format-cc-buffer")
    (add-hook 'write-contents-functions 'mk/format-cc-buffer)))

(defun disable-local-format-cc-buffer ()
  "Disable formatting buffer before saving locally."
  (message "Disabling format-cc-buffer")
  (remove-hook 'write-contents-functions 'mk/format-cc-buffer t))

(with-eval-after-load 'lsp-mode
  (message "Running lsp-mode evals: %S" c-mode-common-hook)
  (add-hook 'c-mode-common-hook #'mk/enable-lsp))

(with-eval-after-load 'cc-mode
  (message "Running c-common-mode evals: %S" c-mode-common-hook)
  (add-hook 'c-mode-common-hook #'enable-global-format-cc-buffer)
  (add-hook 'c-mode-common-hook #'mk/add-custom-cc-keys))
