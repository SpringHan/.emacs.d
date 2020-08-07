;;;; This file is used for the useful functions
(defun open-config-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-etc-config(file)
  (interactive "sEnter the filename: ")
  (pcase file
    ("ui" (find-file "~/.emacs.d/etc/init-ui.el"))
    ("org" (find-file "~/.emacs.d/etc/init-org.el"))
    ("keymap" (find-file "~/.emacs.d/etc/init-keymaps.el"))
    ("mode" (find-file "~/.emacs.d/etc/init-modes.el"))
    ("package" (find-file "~/.emacs.d/etc/init-require-package.el"))
    ("packages" (find-file "~/.emacs.d/etc/init-package.el"))
    ("functions" (find-file "~/.emacs.d/etc/init-functions.el"))))

(defun open-vterm(&optional dir)
  (interactive "sInput the directory: ")
  (if dir
      (progn
	(find-file dir)
	(let ((current-buffer-name (buffer-name)))
	  (vterm)
		(linum-mode -1)
	  (kill-buffer current-buffer-name)))))

(defun open-the-dir(dir-name)
  (interactive "sThe directory's name: ")
  (pcase dir-name
    ("gtd" (find-file "~/.emacs.d/gtd"))
    ("github" (find-file "~/Github"))
    ("emacs" (find-file "~/.emacs.d"))))

(defun set-alpha(var)
  (interactive "sAlpha or not(y-or-n): ")
  (pcase var
    ("y" (set-frame-parameter nil 'alpha '(90 . 100)))
    ("n" (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun window-move(way)
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

(provide 'init-functions)