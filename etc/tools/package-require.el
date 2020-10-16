;;;; This file is used for the `package-require` function

;;;###autoload
(defun package-download (package)
	"Check whether the PACKAGE downloaded.
If not,download it."
	(interactive)
	(unless (package-installed-p package)
		(message "%s" "Refreshing package database")
		(package-refresh-contents)
		(package-install package))) ; Install the packages haven't installed

;;;###autoload
(cl-defun package-setting (&key (keymaps nil) (hooks nil))
	"Setting the KEYMAPS and HOOKS."
	(when (and keymaps (listp keymaps))
		(dolist (keymap keymaps)
			(global-set-key (kbd (car keymap)) (cdr keymap))))
	(when (and hooks (listp hooks))
		(let ((other-hooks (car hooks))
					(only-hook (cdr hooks)))
			(if (listp other-hooks)
					(dolist (other-hook other-hooks)
						(add-hook other-hook only-hook))
				(add-hook other-hooks only-hook)))))

;;;###autoload
(cl-defun package-others (others &optional avg)
	"Check the OTHERS and do the corresponding actions."
  (when (listp others)
    (dolist (other others)
			(when (symbolp other)
				(if avg
						(pcase avg
							(:before-load-eval
							 (when (eq other :before-load-eval)
								 (eval (nth
												(+ 1 (cl-position other others))
												others))))
							(:load-theme
							 (when (eq other :load-theme)
								 (load-theme (nth
															(+ 1 (cl-position other others))
															others) t)
								 (cl-return-from package-others t)))
							(:require-name
							 (when (eq other :require-name)
								 (require (nth
													 (+ 1 (cl-position other others))
													 others))
								 (cl-return-from package-others t)))
							(:child-package
							 (when (eq other :child-package)
								 (let ((packages (nth
																	(+ 1 (cl-position other others))
																	others)))
									 (cond ((symbolp packages)
													(package-require packages))
												 ((listp packages)
													(dolist (package packages)
														(package-require package))))))))
					(pcase other
						(:hook
						 (let ((hook (nth
													(+ 1 (cl-position other others))
													others)))
							 (package-setting :hooks hook)))
						(:config
						 (let ((eval-s (nth
														(+ 1 (cl-position other others))
														others)))
							 (eval eval-s)))
						(:keymap
						 (let ((keymaps (nth
														 (+ 1 (cl-position other others))
														 others)))
							 (package-setting :keymaps keymaps)))
						(:child-config
						 (let ((configs (nth
														 (+ 1 (cl-position other others))
														 others)))
							 (dolist (config configs)
								 (unless (keywordp config)
									 (eval config)))))))))))

;;;###autoload
(cl-defun package-themep (others)
	"Check if the package is a theme.
If it's, return t. Otherwise return nil."
	(cond ((listp others)
				 (dolist (other others)
					 (when (eq other :load-theme)
						 (cl-return-from package-themep t))))
				((and (symbolp others))
				 (if (eq others :load-theme)
						 t
					 nil))))

;;;###autoload
(defun package-require (package-name &optional &rest others)
	"Require the PACKAGE-NAME and its configurations."
	(unless (memq :disable others)
		(when others
			(package-others others :before-load-eval)
			(package-others others :load-theme)
			(package-others others :child-package))
		(if (not (require package-name nil 't))
				(if (and others (or (and (listp others) (eq (car others) :outside))
														(and (symbolp others) (eq others :outside))))
						(unless (package-others others :require-name)
							(ignore
							 (when (not (package-themep others))
								 (message
									(format "The %s package from third-party is not installed." package-name)))))
					(ignore (message
									 (format "The %s package is not exists.And now it'll be installed." package-name))
									(package-download package-name)
									(require package-name))))
		(when others
			(package-others others))))

;; (defmacro package-require-bind-key (key-infos)
;; 	"Eval `define-key' in `package-require'."
;; 	(let (keymap key function)
;; 		(dolist (key-info key-infos)
;; 			(setq keymap (car key-info))
;; 			`(define-key ,keymap (kbd ,key) ,function))))

;; (defmacro package-require (package-name &rest arg)
;; 	"Require the PACKAGE-NAME and its configurations."
;; 	(declare (indent 1))
;; 	(unless (memq :disable arg)
;; 		;; The configs which should be loaded before requiring the package
;; 		(when arg
;; 			`(package-others ,@arg :before-load-eval)
;; 			`(package-others ,@arg :load-theme)
;; 			`(package-others ,@arg :child-package)
;; 			`(when (memq :bind-key ,arg)
;; 				 (package-require-bind-key (nth (+ 1 (cl-position :bind-key ,arg)) ,arg))))

;; 		;; Require package by its name
;; 		(if (not (require package-name nil 't))
;; 				(if (and arg (or (and (listp arg) (memq :outside arg))
;; 												 (and (symbolp arg) (memq :outside arg))))
;; 						`(unless (package-others ,@arg :require-name)
;; 							 (ignore
;; 								(when (not (package-themep ,@arg))
;; 									(message
;; 									 (format "The %s package from third-party is not installed." ,package-name)))))
;; 					`(ignore (message
;; 										(format "The %s package is not exists.And now it'll be installed." ,package-name))
;; 									 (package-download ,package-name)
;; 									 (require ,package-name))))

;; 		;; The configs which should be loaded after requiring the package
;; 		(when arg
;; 			`(package-others ,@arg))))

(provide 'package-require)
