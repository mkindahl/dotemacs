(require 'package)
(require 'lsp)

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

(if (string-match "XEmacs\\|Lucid" emacs-version)
  ;;; XEmacs
  (progn
     (if (file-readable-p "~/.xemacs/init.el")
        (load "~/.xemacs/init.el" nil t))
  )
  ;;; GNU-Emacs
  (if (file-readable-p "~/.gnu-emacs")
      (load "~/.gnu-emacs" nil t)
    (if (file-readable-p "/etc/skel/.gnu-emacs")
	(load "/etc/skel/.gnu-emacs" nil t)))

  ;; Custom Settings
  (setq custom-file "~/.gnu-emacs-custom")
  (load "~/.gnu-emacs-custom" t t)
)

;; System Setting
(setq system-time-locale "C") ; Make sure that the weekdays in the
			      ; time stamps of your Org mode files and
			      ; in the agenda appear in English.

;; Set the load path to use
(add-to-list 'load-path "~/.local/share/emacs/site-lisp")

(load-library "org-hacks")              ;Org Mode settings
(load-library "cc-hacks")               ;C/C++ Settings

(global-set-key "\C-x\C-e" 'compile)
(global-set-key "\C-x\C-n" 'next-error)
(global-set-key "\C-x\C-p" 'previous-error)

;; Set the default values of some variables
;; RST mode settings
(autoload 'rst-mode "rst-mode"
  "Major mode for editing reStructuredText documents" t)
(add-to-list 'auto-mode-alist '("\\.rst\\'" . rst-mode))

;; JavaScript mode settings
(autoload 'javascript-mode "javascript" 
  "Major mode for editing JavaScript" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; Promela mode settings
(autoload 'promela-mode "promela-mode" "PROMELA mode" t)
(add-to-list 'auto-mode-alist '("\\.\\(spin\\|pml\\)\\'" . promela-mode))

;; Python mode settings
(add-hook 'python-mode-hook #'lsp)

;; KerboScript mode settings
(autoload 'ks-mode "ks"
  "Major mode for editing KerboScript" t)
(add-to-list 'auto-mode-alist '("\\.ks\\'" . ks-mode))

;; Markdown settings
(add-to-list 'auto-mode-alist '("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; Protobuf settings
(autoload 'protobuf-mode "protobuf-mode"
  "Major mode for editing Protocol Buffers description language." t)
(add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))

;;; Rust settings
(add-hook 'rust-mode-hook #'lsp)

;;; TeX mode setting
(setq tex-dvi-view-command "okular")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(clang-format-style "Google")
 '(indent-tabs-mode t)
 '(org-agenda-files
   (quote
    ("~/org/bugs.org" "~/org/someday.org" "~/org/admin.org" "~/org/research.org" "~/org/meetings.org" "~/org/refactoring.org" "~/org/reviews.org" "~/org/projects.org" "~/org/refile.org")))
 '(org-src-preserve-indentation t)
 '(rust-format-on-save t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
