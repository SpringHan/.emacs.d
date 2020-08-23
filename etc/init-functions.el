;;;; This file is used for the useful functions
(defun open-config-file ()
	(interactive)
	(find-file "~/.emacs.d/init.el"))

(defun open-etc-config (file)
	(interactive "sEnter the filename: ")
	(pcase file
		("ui" (find-file "~/.emacs.d/etc/init-ui.el"))
		("org" (find-file "~/.emacs.d/etc/init-org.el"))
		("keymap" (find-file "~/.emacs.d/etc/init-keymaps.el"))
		("mode" (find-file "~/.emacs.d/etc/init-modes.el"))
		("package" (find-file "~/.emacs.d/etc/init-require-package.el"))
		("packages" (find-file "~/.emacs.d/etc/init-package.el"))
		("functions" (find-file "~/.emacs.d/etc/init-functions.el"))))

(defun open-vterm (&optional dir)
	(interactive "DInput the directory: ")
	(find-file dir)
	(let ((current-buffer-name (buffer-name)))
		(vterm)
		(linum-mode -1)
		(kill-buffer current-buffer-name)))

(defun open-the-dir (dir-name)
	(interactive "sThe directory's name: ")
	(pcase dir-name
		("gtd" (find-file "~/.emacs.d/gtd"))
		("git" (find-file "~/Github"))
		("emacs" (find-file "~/.emacs.d"))
		("C" (find-file "~/Code/C/src/Study"))))

(defun set-alpha (var)
	(interactive "sAlpha or not(y-or-n): ")
	(pcase var
		("y" (set-frame-parameter nil 'alpha '(90 . 100)))
		("n" (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun window-move (way)
	(interactive "sEnter the way(n-e-u-i): ")
	(let ((current-window-buffer (window-buffer))
				(current-window (get-buffer-window)))
		(pcase way
			("n" (windmove-left))
			("e" (windmove-down))
			("u" (windmove-up))
			("i" (windmove-right)))
		(setq another-window-buffer (get-buffer-window))
		(if (not (eql current-window-buffer another-window-buffer))
				(progn
					(set-window-buffer current-window (window-buffer))
					(set-window-buffer (get-buffer-window) current-window-buffer))))) ; Move the window

(defun sudo-save ()
	(interactive)
	(if (not buffer-file-name)
			(write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
		(write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun write-scratch ()
	(interactive)
	(switch-to-buffer "*Write-Scratch*")
	(markdown-mode))

(defun markdown-table-keymap ()
	(interactive)
	(define-key markdown-mode-map (kbd "C-c C-c TAB") 'markdown-table-align))

(defun day-or-night ()
	"Return t/nil.
If it's daytime now,return t.Otherwise return nil."
	(let ((now-time
				 (string-to-number (cl-subseq (current-time-string) 11 13))))
		(if (and (>= now-time 6) (< now-time 19))
				t
			nil)))

(defun load-the-theme ()
	(interactive)
	(if (day-or-night)
			(progn
				(package-require
				 'atom-one-light
				 :outside
				 :before-load-eval '(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
				 :load-theme 'atom-one-light)
				(when (string= spring/time-block "night")
					(eaf-browser-set))
				(setq spring/time-block "daytime"))
		(package-require
		 'atom-one-dark
		 :outside
		 :before-load-eval '(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
		 :load-theme 'atom-one-dark)
		(when (string= spring/time-block "daytime")
			(eaf-browser-set))
		(setq spring/time-block "night")))

(defun kill-unwanted-buffer ()
	"Kill the unwanted buffers."
	(interactive)
	(kill-buffer "*dashboard*")
	(kill-buffer "notes.org")
	(kill-buffer "tasks.org")
	(kill-buffer "user-init.el"))

(defun tab-bar-new-with-buffer (buffer-name)
	"Create a new tab then select a buffer."
	(interactive "bBuffer Name: ")
	(tab-bar-new-tab)
	(switch-to-buffer buffer-name))

(provide 'init-functions)
