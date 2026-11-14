;;;; This file is the tools of my configuration.d  -*- lexical-binding: t; -*-

;;; Third-party
;;; Move-text
(use-package move-text
  :defer nil
  :init (git-download-ensure "move-text" "manateelazycat/move-text" 1)
  :load-path "~/.emacs.d/third-party/move-text")

;;; Screenshot
(use-package screenshot
  ;; This extension needs the require with `imagemagick'
  :init (git-download-ensure "screenshot" "tecosaur/screenshot" 1)
  :load-path "~/.emacs.d/third-party/screenshot"
  :config
  (spring/extra-add-to-list "~/.emacs.d/third-party/screenshot/screenshot"))

;;; Fuz.el
(use-package fuz
  :defer nil
  :init (git-download-ensure "fuz.el" "rustify-emacs/fuz.el" 1)
  :load-path "~/.emacs.d/third-party/fuz.el"
  :config (unless (require 'fuz-core nil t)
            (fuz-build-and-load-dymod)))


;;; Counsel
(use-package counsel)

;;; Counsel-etags
(use-package counsel-etags
  :bind ("C-]" . counsel-etags-find-tag-at-point))

;;; Icons
(use-package all-the-icons
  :bind ("C-' C-i" . all-the-icons-insert))

;;; Posframe
(use-package posframe
  :defer nil
  :config (setq posframe-mouse-banish nil))

(use-package ivy
  :hook (after-init-hook . ivy-mode)
  :bind (("C-s" . swiper)
         ("C-r" . swiper-backward)

         :map ivy-minibuffer-map
         ("C-<return>" . ivy-immediate-done))
  :config
  (use-package ivy-posframe
    :hook (ivy-mode . ivy-posframe-mode)
    :config
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center))
          ivy-posframe-parameters '((left-fringe . 8) (right-fringe . 8)))))

;;; Which Key
(use-package which-key
  :hook (after-init-hook . which-key-mode))

;;; ace window
(use-package ace-window
  :bind ("C-' C-c" . ace-window))

;;; Calendar-China
(use-package cal-china-x
  :config (setq calendar-chinese-all-holidays-flag t))

;;; Iedit
(use-package iedit
  :bind ("C-' C-e" . iedit-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode text-mode) . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character
        highlight-indent-guides-delay 0.1
        highlight-indent-guides-character 9478)
  :bind ("C-' r" . (lambda ()
                     (interactive)
                     (highlight-indent-guides-mode -1)
                     (highlight-indent-guides-mode t))))

;;; Window Resize
(use-package windresize
  :bind (("C-' C-r" . windresize)
         :map windresize-map
         ("C-n" . windresize-down)
         ("C-p" . windresize-up)
         ("C-b" . windresize-left)
         ("C-f" . windresize-right)))

;;; youdao translate
(use-package youdao-dictionary
  :bind ("C-' t" . youdao-dictionary-search-at-point))

;;; Treemacs : File explore
(use-package treemacs)

;;; Input Method
(use-package pyim
  :disabled
  ;; :defer nil
  :config
  (use-package pyim-basedict)

  (pyim-basedict-enable)
  (pyim-isearch-mode t)
  (setq default-input-method "pyim"
        pyim-default-scheme 'quanpin
        pyim-page-tooltip 'posframe
        pyim-page-length 8
        pyim-page-style 'one-line
        pyim-punctuation-translate '(auto yes no)
        pyim-indicator-cursor-color '("green")))


;;; Command Shower
(use-package command-log-mode
  :config (defun spring/open-or-close-command-log-mode ()
            "Open the command-log-mode."
            (interactive)
            (global-command-log-mode)
            (clm/toggle-command-log-buffer))
  :bind (("C-' k" . spring/open-or-close-command-log-mode)
         ("C-' K" . clm/command-log-clear)))

;;; Transient
(use-package transient)

;;; Package-Lint
(use-package package-lint)

;;; Subtree
(use-package dired-subtree
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("," . dired-subtree-up)
              ("/" . dired-subtree-down)))

;;; Expand region
(use-package expand-region)

;;; Projectile
(use-package projectile
  :autoload projectile-project-root)

;;; Interactive Align
(use-package ialign
  :defer nil)

(use-package rime
  :config
  (setq rime-show-candidate 'posframe))

;;; Etaf
(use-package etaf-install
  :commands (etaf-install))

(provide 'init-tools)
