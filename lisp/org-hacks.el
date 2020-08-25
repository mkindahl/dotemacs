;; Org-mode settings

;(add-to-list 'load-path "/usr/share/emacs/site-lisp/org")

(require 'ob-plantuml)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-clock)
(require 'ox-beamer)
(require 'ox-latex)
(require 'ox-md)

(when (version< "9.2" (org-version))
  (require 'org-tempo))

;; We want the week summary to be displayed in hours, not days.
(when (version<= "9.1.6" (org-version))
  (require 'org-duration)
  (setq org-duration-format 'h:mm))

(setq org-latex-listings 'minted)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-pdf-process
      '("%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"%latex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"%latex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Beamer settings
(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass\[presentation\]\{beamer\}"
               ("\\section\{%s\}" . "\\section*\{%s\}")
               ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
               ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

;; LaTeX settings
(add-to-list 'org-latex-packages-alist '("" "listing"))
(add-to-list 'org-latex-packages-alist '("" "color"))


;; ISpell settings
(add-to-list 'ispell-skip-region-alist
             '("^#\\+begin_src" . "^#\\+end_src"))

;; Basic paths and settings
(setq org-global-properties '(("Effort_ALL" . "0:30 1:00 2:00 4:00 6:00 8:00"))
      org-columns-default-format "%80ITEM(Task) %10Effort{:} %10CLOCKSUM"
      org-list-allow-alphabetical t)

;; Logging configuration
(setq org-log-done 'time
      org-log-into-drawer t)

;; Agenda configuration
(setq org-agenda-diary-file (concat org-directory "/diary.org")
      org-agenda-include-diary t
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-columns-add-appointments-to-effort-sum t)

;;; Refile configuraton
(setq org-refile-use-outline-path 'file
      org-refile-targets '(("projects.org" :maxlevel . 3)
                           ("company.org" :maxlevel . 2)
                           ("someday.org" :maxlevel . 2)))

(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n!)" "STARTED(s!)"
                  "|" "DONE(f!)")
        (sequence "DEFERRED(h@/!)" "DELEGATED(d@)" "WAITING(w@/!)"
                  "|" "REASSIGNED(o@)" "CANCELED(x@)")
	(sequence "TASK(1)" "RESEARCH(2)" "DESIGN(3!)" "ONGOING(4!)" "REVIEW(5!)" "DELIVERED(8!)"
		  "|" "COMPLETE(9!)")))

(setq org-capture-templates
       '(("t" "Todo" entry (file "refile.org")
          "* TODO %?\n  CREATED: %u\n\n"
          :clock-in t :clock-resume t :empty-lines 1
          :clock-keep nil)
         ("j" "Journal entry"
	  entry (file+datetree "journal.org")
          "* %^{topic} %^{CATEGORY}p\n%i%?\n"
          :empty-lines 1)
         ;; Notes sub-menu
         ("n" "Templates for notes of different kinds")
         ("nn" "General note"
	  entry (file "notes.org")
          "* %?\n\n" :empty-lines 1)
	 ("nm" "Notes for meetings")
	 ("nmc" "Notes for cluster team meeting"
	  entry (file+olp "notes.org" "Meetings" "Cluster Team Meeting")
	  "* %?\n\n" :empty-lines 1)))

(setq org-tags-exclude-from-inheritance '("project")
      org-stuck-projects '("/-MAYBE-DONE-COMPLETE-REASSIGNED"
                           ("NEXT")
                           ()))
;;; Some helper functions

;; Resume clocking tasks when emacs is restarted
(org-clock-persistence-insinuate)
(setq org-clock-history-length 28
      ;; Resume clocking task on clock-in if the clock is open
      org-clock-in-resume t
      ;; Change task state to NEXT when clocking in
;      org-clock-in-switch-to-state 'mk/clock-in-to-next
      ;; Separate drawers for clocking and logs
;      org-drawers '("PROPERTIES" "LOGBOOK" "CLOCK")
      ;; Save clock data in the CLOCK drawer and state changes and
      ;; notes in the LOGBOOK drawer
      org-clock-into-drawer "CLOCK"
      ;; Removes clocked tasks with 0:00 duration
      org-clock-out-remove-zero-time-clocks t
      ;; Clock out when moving task to a done state
      org-clock-out-when-done t
      ;; Save the running clock and all clock history when exiting
      ;; Emacs, load it on startup
      org-clock-persist 'history
      ;; Enable auto clock resolution for finding open clocks
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      ;; Include current clocking task in clock reports
      org-clock-report-include-clocking-task t)

(defvar mk/keep-clock-running nil)

(defun mk/clock-in-to-next (kw)
  "Switch task from TODO to STARTED when clocking in.
Skips capture tasks and tasks with subtasks"
  (if (and (member kw '("TODO" "WAITING" "NEXT"))
           (not (and (boundp 'org-capture-mode) org-capture-mode)))
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-subtask nil))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-not-done-keywords)
              (setq has-subtask t))))
        (when (not has-subtask)
          "STARTED"))))

(defun mk/clock-in ()
  (interactive)
  (setq mk/keep-clock-running t)
  (if (marker-buffer org-clock-default-task)
      (unless (org-clock-is-active)
        (mk/clock-in-default-task))
    (unless (marker-buffer org-clock-default-task)
      (org-agenda nil "c"))))

(defun mk/clock-out ()
  (interactive)
  (setq mk/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out)))

(defun mk/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun mk/clock-out-maybe ()
  (when (and mk/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task))
    (mk/clock-in-default-task)))

;(add-hook 'org-clock-out-hook 'mk/clock-out-maybe 'append)

(setq org-agenda-clockreport-parameter-plist '(:maxlevel 4))

;;; Babel settings
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (C . t)
   (plantuml . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (latex . t)
   (perl . t)
   (python . t)
   (scheme . t)))

;;; Defining keys

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cr" 'org-capture)
(global-set-key (kbd "<f12>") 'org-agenda)
