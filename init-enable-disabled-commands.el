;;;; This file is used to define functions about the disabled commands

(defconst spring/enable-commands
	'(narrow-to-region downcase-region upcase-region)
	"The disabled commands that I want to enable.")

;;;###autoload
(defun enable-commands(commands)
	"Enable the disbaled command like `enable-command', but it can enable more than one command at once."
	(dolist (command commands)
		(when command
			(en/disable-command command nil))))

;;;###autoload
(defun disable-commands(commands)
	"Disable command like `disable-command', but it can disbale more than one command at once."
	(dolist (command commands)
		(when command
			(en/disable-command command t))))

;;;###autoload
(defun enable-commands-init()
	"Enable some commands."
	(when (not (file-exists-p "~/.emacs.d/enable-commands"))
		(let ((y-or-n (read-minibuffer "Do you want to enable some disabled commands?It's a bit Dangerous!(y-n)" "n")))
			(make-empty-file "~/.emacs.d/enable-commands")
			(when (string= y-or-n "y")
				(enable-commands spring/enable-commands)))))

(provide 'init-enable-disabled-commands)
