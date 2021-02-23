;;; This is the clojure file.

(defun spring/cider-eval-last-sexp-in-repl ()
  "Eval last sexp in repl."
  (interactive)
  (let ((sexp (substring-no-properties (cider-last-sexp)))
        buffer)
    (with-current-buffer (current-buffer)
      (when (derived-mode-p 'clojure-mode)
        (setq sexp (concat (substring sexp 0 1)
                           (cider-current-ns)
                           "/"
                           (substring sexp 1)))))
    (catch 'stop
      (dolist (buf (buffer-list))
        (when (string-match-p "^\\*cider-repl \\(.*\\)\\*" (buffer-name buf))
          (setq buffer buf)
          (throw 'stop t))))
    (when buffer
      (with-current-buffer buffer
        (end-of-buffer)
        (insert sexp)
        (cider-repl-return))
      (while (not (eq (current-buffer) buffer))
        (other-window 1)))))

(gpack clojure-mode
  :var (clojure-toplevel-inside-comment-form . t)
  :key (clojure-mode-map . ("C-c M-l" . spring/cider-eval-last-sexp-in-repl)))

(gpack cider
  :hook (clojure-mode-hook . cider-mode))

(gpack clj-refactor
  :hook (clojure-mode-hook . clj-refactor-mode)
  :config (cljr-add-keybindings-with-prefix "C-c C-r"))

(provide 'spring-clojure)
