;;; Markdown  -*- lexical-binding: t; -*-

(use-package markdown-ts-mode
  :mode "\\.md\\'")

(use-package markdown-ts-appear
  :hook (markdown-ts-mode . markdown-ts-appear-mode))

(provide 'spring-markdown)
