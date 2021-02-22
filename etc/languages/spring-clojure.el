;;; This is the clojure file.

(gpack clojure-mode
  :var (clojure-toplevel-inside-comment-form . t))

(gpack cider
  :hook (clojure-mode-hook . cider-mode))

(gpack clj-refactor
  :hook (clojure-mode-hook . clj-refactor-mode)
  :config (cljr-add-keybindings-with-prefix "C-c C-r"))

(provide 'spring-clojure)
