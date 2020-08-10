;;;; This file is used for the `package-require` function
(defun package-download(package)
  (interactive)
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database")
    (package-refresh-contents)
    (package-install package))) ; Install the packages haven't installed

(defun package-setting(keymaps hooks)
  (if (listp keymaps)
      (dolist (keymap keymaps)
	(global-set-key (kbd (nth 0 keymap)) (nth 1 keymap))))
  (if (listp hooks)
      (progn
	(setq other-hooks (nth 0 hooks)
	      only-hook (nth 1 hooks))
	(if (listp other-hooks)
	    (dolist (other-hook other-hooks)
	      (add-hook other-hook only-hook))
	  (add-hook other-hooks only-hook)))))

(defun package-others(others)
  (when (listp others)
    (dolist (other others)
			(when (and (symbolp other) (eq other :hook))
				(let ((hook
							 (nth (+ 1 (cl-position other others))
										others)))
					(package-setting :keymaps hook))))))

(defun package-require(package-name &optional keymaps hooks s-ex &rest others)
  (if
      (not
       (require package-name nil 't))
			(if (or (and (listp others) (eq (nth 0 others) :outside))
							(and (symbolp others) (eq others :outside)))
					(ignore (message
									 (format "The %s package from third-party is not installed." 'package-name)))
				(ignore (message
								 (format "The %s package is not exists.And now it'll be installed." 'package-name))
								(package-download package-name))))
  (package-setting keymaps hooks)
  (if others
      (package-others others)))

(provide 'init-require-package)
