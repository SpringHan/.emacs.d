;;;; This file is used to define functions about the disabled commands

(defconst spring/enable-commands
	'(narrow-to-region downcase-region upcase-region)
	"The disabled commands that I want to enable.")

;;;###autoload
(defun enable-commands (commands)
	"Enable the disbaled command like `enable-command', but it can enable more than one command at once."
	(dolist (command commands)
		(when command
			(en/disable-command command nil))))

;;;###autoload
(defun disable-commands (commands)
	"Disable command like `disable-command', but it can disbale more than one command at once."
	(dolist (command commands)
		(when command
			(en/disable-command command t))))

;;;###autoload
(defun enable-commands-init ()
	"Enable some commands."
	(if (and (not (file-exists-p "~/.emacs.d/enable-commands")) (not (file-exists-p "~/.emacs.d/disable-commands")))
			(let ((y-or-n (read-minibuffer "Do you want to enable some disabled commands?It's a bit Dangerous!(y-n)" "n")))
				(if (string= y-or-n "y")
						(progn
							(enable-commands spring/enable-commands)
							(make-empty-file "~/.emacs.d/enable-commands"))
					(make-empty-file "~/.emacs.d/disable-commands")))
		(when (file-exists-p "~/.emacs.d/enable-commands")
			(enable-commands spring/enable-commands))))

(provide 'init-enable-disabled-commands)
