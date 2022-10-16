(require 'package)

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; Load custom file settings.
(setq custom-file "~/.gnu-emacs-custom")
(load "~/.gnu-emacs-custom" t t)

;; System Setting
(setq system-time-locale "C") ; Make sure that the weekdays in the
			      ; time stamps of your Org mode files and
			      ; in the agenda appear in English.

;; Set the load path to use
(add-to-list 'load-path "~/.local/share/emacs/site-lisp")

(load-library "org-hacks")              ;Org Mode settings
(load-library "cc-hacks")               ;C/C++ Settings
(load-library "rust-hacks")             ;Rust Settings
(load-library "python-hacks")           ;Python Settings
(load-library "sql-hacks")		;SQL Settings
(load-library "perl-hacks")		;Perl settings

(global-set-key "\C-x\C-e" 'compile)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key "\C-x\C-p" 'previous-error)

;; Promela mode settings
(autoload 'promela-mode "promela-mode" "PROMELA mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(spin\\|pml\\)\\'" . promela-mode))

;; Markdown settings
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;;; TeX mode setting
(setq tex-dvi-view-command "okular")
