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
	      (add-hook other-hook only-hook)
	      (print other-hook) (print only-hook))
	  (add-hook other-hooks only-hook)))))

(defun package-require(package-name &optional keymaps hooks others)
  (if
      (not
       (require package-name nil 't))
      (ignore
       (message
	(format "The %s package is not exists.And now it'll be installed." 'package-name))
       (package-download package-name)))
  (package-setting keymaps hooks))

(provide 'init-require-package)
