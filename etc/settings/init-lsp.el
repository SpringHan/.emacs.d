;;;; This is the lsp-mode settings file

;;; Packages
;;; Lsp-mode
(package-require
 'lsp-mode
 :hook '((c-mode-hook c++-mode-hook lisp-mode-hook js-mode-hook web-mode-hook) . lsp)
 :keymap '(("C-' F" . lsp-format-buffer))
 :config '(progn
								(setq lsp-idle-delay 1200
											lsp-auto-guess-root nil
											lsp-file-watch-threshold 2000
											lsp-eldoc-hook nil
											lsp-log-io nil
											lsp-enable-folding nil	 
											lsp-enable-snippet nil	 
											lsp-prefer-flymake :none)))
;;; lsp-ui
(package-require
 'lsp-ui
 :hook '(lsp-mode-hook . lsp-ui-mode)
 :delay-eval '(progn
								;; lsp-ui-sideline
								(setq lsp-ui-sideline-enable t
											lsp-ui-sideline-delay 5)
								;; lsp-ui-peek
								(setq lsp-ui-peek-enable t)
								(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
								(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
								;; lsp-ui-doc
								(setq lsp-ui-doc-enable t)))

;;;FlyMake
(package-require
 'flymake
 :keymap '(("C-' C-f" . flymake-mode)))

(defvar lsp-on-touch-time 0
	"The lsp-on-change's time.")

;;; Dap-mode
(package-require
 'dap-mode
 :keymap '(("C-' d" . dap-debug)
					 ("<F5>" . dap-breakpoint-toggle)
					 ("<F6>" . dap-continue)))

;;; Config
(eval-after-load 'lsp-mode
	'(progn
		 (defvar lsp-on-touch-time 0)
     (defadvice lsp-on-change (around lsp-on-change-hack activate)
       ;; don't run `lsp-on-change' too frequently
       (when (> (- (float-time (current-time))
									 lsp-on-touch-time) 30)
				 (setq lsp-on-touch-time (float-time (current-time)))
				 ad-do-it))))

(provide 'init-lsp)
