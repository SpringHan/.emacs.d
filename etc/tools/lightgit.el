;;;; This is the lightgit, a light git plugin for Emacs.

(defgroup lightgit nil
	"The group of `lightgit-mode'."
	:group 'applications)

(defcustom lightgit-commit-message-line nil
	"The line of commit message."
	:type 'number
	:group 'lightgit)

(defconst lightgit-buffer-name "*LightGit*"
	"The buffer name of `lightgit' mode.")

(defconst lightgit-push-output-buffer "*LightGit-Push-Output*"
	"The buffer name of lightgit output for push.")

(defconst lightgit-quick-commit-buffer "*LightGit-Quick-Commit*"
	"The buffer name of lightgit quick commit.")

(defconst lightgit-shell-output " *LightGit-Shell-Output*"
	"The buffer for shell output.")

(defcustom lightgit-buffer-list (list lightgit-buffer-name lightgit-push-output-buffer lightgit-shell-output)
	"The buffer list of the lightgit."
	:type 'list
	:group 'lightgit)

(defvar lightgit-mode-map
	(let ((map (make-sparse-keymap)))
		(define-key map "q" 'lightgit-quit)
		(define-key map "x" 'lightgit-back-buffer)
		(define-key map "c" 'lightgit-commit)
		(define-key map (kbd "RET") 'lightgit-return)
		(define-key map "P" 'lightgit-push)
		(define-key map "s" '(lambda () (lightgit-stage-unstage :type 'stage)))
		(define-key map "a" '(lambda () (lightgit-stage-unstage :type 'stage t)))
		(define-key map "u" '(lambda () (lightgit-stage-unstage :type 'unstage)))
		(define-key map "A" '(lambda () (lightgit-stage-unstage :type 'unstage t)))
		(define-key map "n" 'next-line)
		(define-key map "p" 'previous-line)
		(define-key map "d" 'lightgit-discard)
		(define-key map "D" '(lambda () (lightgit-discard t)))
		(define-key map ":" 'lightgit-execute-command)
		(define-key map "w" 'lightgit-next-item)
		(define-key map "b" 'lightgit-previous-item)
		(define-key map "g" 'lightgit-refresh-buffer)
		map)
	"The map for the `lightgit-mode'.")

(define-derived-mode lightgit-mode nil "LightGit"
	"A light Git plugin for Emacs."
	:syntax-table nil
	:abbrev-table nil
	:group 'lightgit
	(setq buffer-read-only t))

(defun lightgit-change-evil-state (state)
	"Change evil state."
	(when (featurep 'evil)
		(evil-change-state state)))

(defun lightgit ()
	"The function to open lightgit buffer."
	(interactive)
	(switch-to-buffer lightgit-buffer-name)
	(lightgit-mode)
	(lightgit-change-evil-state 'emacs))

(defun lightgit-quit ()
	"Close the lightgit buffer."
	(interactive)
	(dolist (buffer lightgit-buffer-list)
		(when (get-buffer buffer)
			(kill-buffer buffer))))

(defun lightgit-run-git (arg)
	"The function to run git."
	(shell-command (concat "git " arg) lightgit-shell-output))

(defun lightgit-get-unstaged-files ()
	"The function to get all of the unstaged files."
	(let (file-contents)
		(lightgit-run-git "status --short")
		(with-current-buffer lightgit-shell-output
			(setq file-contents (buffer-string)))))

(defun lightgit-stage-unstage (&key type &optional all)
	"Stage or unstage the changes."
	(interactive)
	(unless (null type)
		(let* ((action
						(if (eq type 'stage)
											"add"
										"reset HEAD"))
					(files
					 (if (string= action "add")
							 (if all
									 "-a")
						 (if all
								 "")))))))

(defun lightgit-refresh-buffer ()
	"The function to refresh the buffer.")

(provide 'lightgit)
