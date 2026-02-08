;;; Hacks to configure Perl mode

(defcustom perltidy-executable
  (executable-find "perltidy")
  "Location of the perltidy executable.

A string containing the name or the full path of the executable."
  :group 'perl-format
  :type '(file :must-match t)
  :risky t)

(defun perltidy-format-region (start end)
  "Use perltidy to format the region between START and END."
  (call-process-region start end perltidy-executable t t t "-st"))

(defun mk/format-perl-buffer ()
  "Format buffer by removing trailing whitespace and format the
 buffer using perltidy."
  (interactive)
  (message "Formatting %s using %s" buffer-file-name perltidy-executable)
  (let ((start (point-min))
        (finish (point-max)))
    (save-excursion
      (delete-trailing-whitespace start finish)
      (perltidy-format-region start finish))))

(defun disable-format-perl-buffer ()
  "Disable formatting buffer before saving locally."
  (interactive)
  (message "Disabling format-cc-buffer")
  (remove-hook 'write-contents-functions #'mk/format-perl-buffer t))

(with-eval-after-load 'perl-mode
  (add-hook 'write-contents-functions #'mk/format-perl-buffer))
