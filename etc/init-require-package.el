;;;; This file is used for the `package-require` function
(defun my/packages-installed-p()
  (loop for pkg in my/packages
	when (not (package-installed-p pkg)) do (return nil)
	(finally (return t)))) ; Define function to return whether package installed

(defun auto-download-plugins()
  (interactive)
  (unless (my/packages-installed-p)
    (message "%s" "Refreshing package database")
    (package-refresh-contents)
    (dolist (pkg my/packages)
      (when (not (package-installed-p pkg))
	(package-install pkg))))) ; Install the packages haven't installed

(defun package-require(package-name &optional keymaps others)
  (if
      (not
       (require package-name nil 't))
      (ignore
       (message
	(format "The %s package is not exists." 'package-name)))
    (if (listp keymaps)
	(dolist (keymap keymaps)
	  (global-set-key (kbd (nth 0 keymap)) (nth 1 keymap))))
    (if (listp others)
	(dolist (other others)
	  (other)))))

(provide 'init-require-package)
