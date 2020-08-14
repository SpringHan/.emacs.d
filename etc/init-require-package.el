;;;; This file is used for the `package-require` function
;;;###autoload
(defun package-download(package)
	"Check whether the PACKAGE downloaded.
If not,download it."
	(interactive)
	(unless (package-installed-p package)
		(message "%s" "Refreshing package database")
		(package-refresh-contents)
		(package-install package))) ; Install the packages haven't installed

;;;###autoload
(defun package-setting(keymaps hooks)
	"Setting the KEYMAPS and HOOKS."
	(when (listp keymaps)
		(dolist (keymap keymaps)
			(global-set-key (kbd (nth 0 keymap)) (nth 1 keymap))))
	(when (listp hooks)
		(let ((other-hooks (nth 0 hooks))
					(only-hook (nth 1 hooks)))
			(if (listp other-hooks)
					(dolist (other-hook other-hooks)
						(add-hook other-hook only-hook))
				(add-hook other-hooks only-hook)))))

;;;###autoload
(defun package-others(others &optional avg)
	"Check the OTHERS and do the corresponding actions."
  (when (listp others)
    (dolist (other others)
			(when (symbolp other)
				(if avg
						(pcase avg
							(:before-load-eval (when (eq other :before-load-eval)
																	 (eval (nth (+ 1
																								 (cl-position other others))
																							others))))
							(:load-theme (when (eq other :load-theme)
														 (load-theme (nth (+ 1
																								 (cl-position other others))
																							others)
																				 t))))
					(pcase other
						(:hook (let ((hook (nth (+ 1
																			 (cl-position other others))
																		others)))
										 (package-setting :keymaps hook)))
						(:delay-eval (let ((eval-s (nth (+ 1
																							 (cl-position other others))
																						others)))
													 (eval eval-s)))
						(:keymap (let ((keymaps (nth (+ 1
																						(cl-position other others))
																				 others)))
											 (package-setting keymaps :hook)))))))))

;;;###autoload
(defun package-themep(others)
	"Check if the package is a theme.
If it's, return t. Otherwise return nil."
	(cond ((listp others)
				 (dolist (other others)
					 (when (eq other :load-theme)
						 (return t))))
				((and (symbolp others))
				 (if (eq others :load-theme)
						 t
					 nil))))

(defun package-require(package-name &optional &rest others)
	"Require the PACKAGE-NAME and its configurations."
	(when others
		(package-others others :before-load-eval)
		(package-others others :load-theme))
  (if (not (require package-name nil 't))
			(if (and others (or (and (listp others) (eq (car others) :outside))
													(and (symbolp others) (eq others :outside))))
					(ignore
					 (when (not (package-themep others))
						 (message
							(format "The %s package from third-party is not installed." package-name))))
				(ignore (message
								 (format "The %s package is not exists.And now it'll be installed." package-name))
								(package-download package-name))))
	(when others
		(package-others others)))

(provide 'init-require-package)
