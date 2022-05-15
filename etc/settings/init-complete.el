;;;; This is the auto complete file for my configuration.

;;; Packages
;;; Company (Complete Anything)
;; (gpack company-c-headers)

(gpack orderless
  :config
  (setq completion-styles '(orderless pratial-completion)))
(gpack corfu
  :hook (after-init-hook . global-corfu-mode)
  :config
  (setq corfu-auto-delay 0
        corfu-auto-prefix 0
        corfu-auto t
        tab-always-indent 'complete))
(gpack corfu-doc
  :hook (corfu-mode-hook . corfu-doc-mode)
  :config
  (progn
    (define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
    (define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)))

;; (gpack company
;;   :hook ((after-init-hook . global-company-mode)
;;          (emacs-lisp-mode-hook . (lambda ()
;;                                    (setq-local company-backends
;;                                                (append '(company-elisp)
;;                                                        company-backends)))))
;;   :var ((company-show-numbers . t)
;;         (company-idle-delay . 0)
;;         (company-echo-delay . 0))
;;   :config (progn
;;             (setq company-idle-delay 0
;;                   company-minimum-prefix-length 1)
;;             (with-eval-after-load
;;                 'company
;;               (define-key company-active-map (kbd "M-p") nil)
;;               (define-key company-active-map (kbd "M-n") nil)
;;               (define-key company-active-map (kbd "C-n") #'company-select-next)
;;               (define-key company-active-map (kbd "C-p") #'company-select-previous)
;;               (setq company-backends
;;                     '((company-dabbrev-code company-dabbrev company-keywords company-files company-capf))))))

;; (gpack company-box
;;   :hook (company-mode-hook . company-box-mode))

;; (gpack company-web
;;   :hook (web-mode-hook . (lambda ()
;;                            (setq-local company-backends
;;                                        (append '(company-web-html company-web-jade company-web-slim)
;;                                                company-backends)))))

;; (gpack company-tabnine
;;   :hook ((python-mode-hook go-mode-hook) . (lambda ()
;;                                              (setq-local company-backends
;;                                                          (append '(company-tabnine) company-backends)))))

;;; Redefinition

(defun corfu--update ()
  "Refresh Corfu UI."
  (pcase-let* ((`(,beg ,end ,table ,pred) completion-in-region--data)
               (pt (- (point) beg))
               (str (buffer-substring-no-properties beg end))
               (initializing (not corfu--input)))
    (corfu--echo-refresh)
    (cond
     ;; XXX Guard against errors during candidate generation.
     ;; Turn off completion immediately if there are errors
     ;; For example dabbrev throws error "No dynamic expansion ... found".
     ;; TODO Report this as a bug? Are completion tables supposed to throw errors?
     ((condition-case err
          ;; Only recompute when input changed
          (unless (equal corfu--input (cons str pt))
            (corfu--update-candidates str pt table pred)
            nil)
        (error (corfu-quit))))
     ;; 1) Initializing, no candidates => Quit. Happens during auto completion.
     ((and initializing (not corfu--candidates))
      (corfu-quit))
     ;; 2) Single exactly matching candidate and no further completion is possible.
     ((and (not (equal str ""))
           (equal (car corfu--candidates) str) (not (cdr corfu--candidates))
           (not (consp (completion-try-completion str table pred pt corfu--metadata)))
           (or initializing corfu-on-exact-match))
      ;; Quit directly when initializing. This happens during auto completion.
      (if (or initializing (eq corfu-on-exact-match 'quit))
          (corfu-quit)
        (corfu--done str 'finished)))
     ;; 3) There exist candidates => Show candidates popup.
     (corfu--candidates
      (corfu--candidates-popup beg)
      (corfu--preview-current beg end)
      (corfu--echo-documentation)
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     ;; 4) There are no candidates & corfu-quit-no-match => Confirmation popup.
     ((and (not corfu--candidates)
           (pcase-exhaustive corfu-quit-no-match
             ('t nil)
             ('nil t)
             ('separator (seq-contains-p (car corfu--input) corfu-separator))))
      (corfu--popup-show beg 0 8 '(#("No match" 0 8 (face italic))))
      (redisplay 'force)) ;; XXX HACK Ensure that popup is redisplayed
     (t (corfu-quit)))))

(provide 'init-complete)
