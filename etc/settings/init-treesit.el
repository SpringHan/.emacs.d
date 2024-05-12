;;; Treesit settings,

(require 'treesit)

(setq treesit-extra-load-path (list (locate-user-emacs-file "treesit/"))
      treesit-language-source-alist
      '((json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (dart . ("https://github.com/UserNobody14/tree-sitter-dart"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp")))
      major-mode-remap-alist
      (append major-mode-remap-alist
              '((js-json-mode . json-ts-mode))))

(defun spring/initialize-treesit-languages ()
  "Initialize treesit languages."
  (dolist (lang treesit-language-source-alist)
    (unless (treesit-language-available-p (car lang))
      (treesit-install-language-grammar (car lang)))))

(add-hook 'emacs-startup-hook #'spring/initialize-treesit-languages)
(add-hook 'emacs-lisp-mode-hook (lambda () (treesit-parser-create 'elisp)))

(provide 'init-treesit)
