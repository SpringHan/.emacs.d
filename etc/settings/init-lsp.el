;;;; This is the lsp-mode settings file

(defvar lsp-on-touch-time 0
	"The lsp-on-change's time.")

(eval-after-load 'lsp-mode
	'(progn
		 (defvar lsp-on-touch-time 0)
     (defadvice lsp-on-change (around lsp-on-change-hack activate)
       ;; don't run `lsp-on-change' too frequently
       (when (> (- (float-time (current-time))
									 lsp-on-touch-time) 30)
				 (setq lsp-on-touch-time (float-time (current-time)))
				 ad-do-it))))


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
