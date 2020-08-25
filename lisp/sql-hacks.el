;;; Does not do anything currently. Just a placeholder.
(defun mk/pg-format-buffer ()
  "Call pg_format"
  ())

(defun turn-on-pg-format-buffer ()
  "Format buffer before saving."
  (add-hook 'write-contents-functions 'mk/pg-format-buffer))
