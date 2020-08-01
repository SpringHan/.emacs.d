;;;; This file is about org-mode settings
(setq org-log-mode 'note) ; Set the log mode type
(setq org-todo-keywords
      '((sequence "TODO(t)" "STUDY(s)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCEL(c@/!)")
	(sequence "BUG(b)" "KNOWN(k)" "|" "FINISHED(f@/!)"))) ; Set the todo's keywords
(setq org-tag-alist '(("@code" . ?c) ("@life" . ?l))) ; Set the org tags keyword
(setq org-M-RET-may-split-line '((headline . nil))) ; Don't split text
(setq org-hide-leading-stars t) ; Hide the leading stars
(setq org-odd-levels-only t) ; Only odd the levels

;; GTD
(setq org-agenda-files '("~/.emacs.d/gtd"))
(defvar org-agenda-dir "" "gtd org files location")
(setq-default org-agenda-dir "~/.emacs.d/gtd")
;; org-agenda-dir files
(setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
(setq org-agenda-file-insp (expand-file-name "insps.org" org-agenda-dir))
(setq org-agenda-file-task (expand-file-name "tasks.org" org-agenda-dir))
(setq org-capture-templates
      '(
	("t" "Task")
	("tw" "Work Task" entry (file+headline org-agenda-file-task "Work")
	 "* TODO %U - %^{Work Mainly Content} %^g\n  %?" :clock-in t :clock-keep t)
	("ts" "Study Task" entry (file+headline org-agenda-file-task "Study")
	 "* STUDY %U - %^{Study Mainly Content} %^g\n  %?" :clock-in t :clock-keep t)
	("i" "inspiration" entry (file+headline org-agenda-file-insp "Inspiration")
	 "* %^{Inspiration Mainly Content} \n  %?")
	("n" "Note" entry (file+headline org-agenda-file-note "Note")
	 "* %^{Note Mainly Content} \n  %?")))

(provide 'init-org)
