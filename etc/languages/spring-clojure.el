;;; This is the clojure file.

(use-package clojure-mode
  :init
  (setq clojure-toplevel-inside-comment-form t)

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

  (defun spring/clojure-comment ()
    "Comment clojure."
    (interactive)
    (let ((comment-check
           (lambda ()
             (string= "#_" (buffer-substring-no-properties (point) (+ 2 (point))))))
          (comment-check2
           (lambda ()
             (string-suffix-p "#_" (buffer-substring-no-properties
                                    (line-beginning-position) (point))))))
      (save-mark-and-excursion
        (cond ((null current-prefix-arg)
               (if (funcall comment-check)
                   (while (funcall comment-check)
                     (delete-char 2))
                 (unless (or (= (char-after) 40)
                             (= (char-after) 91)
                             (= (char-after) 123))
                   (backward-up-list))
                 (if (funcall comment-check2)
                     (while (funcall comment-check2)
                       (delete-char -2))
                   (insert "#_"))))

              ((numberp current-prefix-arg)
               (let ((current-symbol (symbol-name (symbol-at-point))))
                 (unless (string-prefix-p
                          current-symbol
                          (buffer-substring-no-properties (point) (line-end-position)))
                   (backward-sexp))
                 (dotimes (_ current-prefix-arg)
                   (insert "#_"))))

              ((equal '(4) current-prefix-arg)
               (unless (and (= (char-after) 40)
                            (= (point) (line-beginning-position)))
                 (beginning-of-defun))
               (if (funcall comment-check)
                   (delete-char 2)
                 (insert "#_")))))))

  :bind (:map clojure-mode-map
              ("C-c M-l" . spring/cider-eval-last-sexp-in-repl)
              ("C-#" . spring/clojure-comment)))

(use-package cider
  :hook (clojure-mode . cider-mode))

(provide 'spring-clojure)
