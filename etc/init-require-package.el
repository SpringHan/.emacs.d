;;;; This file is used for the `package-require` function
(defun package-download(package)
  (interactive)
  (unless (package-installed-p package)
    (message "%s" "Refreshing package database")
    (package-refresh-contents)
    (package-install package))) ; Install the packages haven't installed

(defun package-setting(&optional keymaps)
  (if (listp keymaps)
      (dolist (keymap keymaps)
	(global-set-key (kbd (nth 0 keymap)) (nth 1 keymap)))))

(defun package-require(package-name &optional keymaps others)
  (if
      (not
       (require package-name nil 't))
      (ignore
       (message
	(format "The %s package is not exists.And now it'll be installed." 'package-name))
       (package-download package-name)))
    (package-setting keymaps))

(provide 'init-require-package)
