;;;; This file is the tools of my configuration.d

;;; Third-party
;;; Emacs Application Framework
(gpack eaf
	:load-path "~/.emacs.d/third-party/emacs-application-framework"
  :key (("C-q C-w l" . eaf-open-browser)
        ("C-q C-w h" . eaf-open-browser-with-history)
        ("C-q C-m t" . eaf-open-pyqterminal))
  :var ((eaf-webengine-default-zoom . 2.15)
        (eaf-browser-remember-history . t)
        (eaf-pyqterminal-font-size . 40)
        (eaf-pyqterminal-font-family . "Monego"))
  :hook (eaf-mode-hook . (lambda ()
                           (setq-local awesome-tray-active-modules
                                       '("netease-current-song" "input-method" "mode-name" "date"))))
  :config
  (progn
    (require 'eaf-browser)
    (require 'eaf-markdown-previewer)
    (require 'eaf-org-previewer)
    (require 'eaf-image-viewer)
    (require 'eaf-pdf-viewer)
    (require 'eaf-js-video-player)
    (require 'eaf-pyqterminal)
    (require 'eaf-mindmap)
    (require 'eaf-rss-reader)))

;;; Netease Cloud Music
;; (gpack request)
;; (gpack netease-cloud-music
;;   :repo "SpringHan/netease-cloud-music.el"
;;   :key (("C-' n q" . netease-cloud-music-quit)
;;         ("C-' n t" . (lambda ()
;;                        (interactive)
;;                        (if (get-buffer netease-cloud-music-buffer-name)
;;                            (netease-cloud-music)
;;                          (eaf-open-netease-cloud-music)))))
;;   :var ((netease-cloud-music-search-limit . 15)
;;         (netease-cloud-music-show-lyric . 'all)
;;         (netease-cloud-music-line-number-relative . t))
;;   :config (progn
;;             (require 'netease-cloud-music-ui)
;;             (require 'netease-cloud-music-comment)))

;; (spring/extra-add-to-list
;;  "~/.emacs.d/third-party/netease-cloud-music.el/netease-cloud-music")

;;; Shengci.el
;; (gpack f)
;; (gpack shengci
;;   :repo "EvanMeek/shengci.el")

;;; Popweb
(gpack popweb
  :repo "manateelazycat/popweb"
  :var (popweb-zoom-factor . 2.13)
  :key (("C-q C-m h" . popweb-dict-bing-pointer)
        ("C-q C-m i" . popweb-dict-bing-input))
  :config
  (progn
    (add-to-list 'load-path "~/.emacs.d/third-party/popweb/extension/dict")
    (require 'popweb-dict)))

;;; Move-text
(gpack move-text
  :repo "manateelazycat/move-text")

;;; Dired-File-Preview
(gpack dired-file-preview
  :repo "SpringHan/dired-file-preview")

(spring/extra-add-to-list "~/.emacs.d/third-party/dired-file-preview/dired-file-preview")

;;; Screenshot
(gpack screenshot
  ;; This extension needs the require with `imagemagick'
  :repo "tecosaur/screenshot")

(spring/extra-add-to-list "~/.emacs.d/third-party/screenshot/screenshot")

;;; Fuz.el
(gpack fuz
  :repo "rustify-emacs/fuz.el"
  :config (unless (require 'fuz-core nil t)
            (fuz-build-and-load-dymod)))

;;; Copilot
(gpack copilot-chat
  :config (defun spring/copilot-chat-display ()
            (interactive)
            (copilot-chat-display)
            (delete-other-windows)
            (switch-to-buffer (car (copilot-chat--prepare-buffers)))))


;;; Counsel
(gpack counsel)

;;; Counsel-etags
(gpack counsel-etags
  :key ("C-]" . counsel-etags-find-tag-at-point))

;;; Icons
(gpack all-the-icons
  :key ("C-' C-i" . all-the-icons-insert))

;;; Posframe
(gpack posframe
  :var (posframe-mouse-banish . nil))

(gpack ivy
  :hook (after-init-hook . ivy-mode)
  :key (("C-s" . swiper)
        ("C-r" . swiper-backward)
        (ivy-minibuffer-map . ("C-<return>" . ivy-immediate-done))))

(gpack ivy-posframe
  :hook (ivy-mode-hook . ivy-posframe-mode)
  :var ((ivy-posframe-display-functions-alist . '((t . ivy-posframe-display-at-frame-center)))
        (ivy-posframe-parameters . '((left-fringe . 8) (right-fringe . 8)))))

;;; Which Key
(gpack which-key
  :hook (after-init-hook . which-key-mode))

;;; ace window
(gpack ace-window
  :key ("C-' C-c" . ace-window))

;;; Calendar-China
(gpack cal-china-x
  :var (calendar-chinese-all-holidays-flag . t))

;;; Iedit
(gpack iedit
  :key ("C-' C-e" . iedit-mode))

(gpack highlight-indent-guides
  :hook ((prog-mode-hook text-mode-hook) . highlight-indent-guides-mode)
  :var ((highlight-indent-guides-method . 'character)
        (highlight-indent-guides-delay . 0)
        (highlight-indent-guides-character . 9478))
  :key ("C-' r" . (lambda ()
                    (interactive)
                    (highlight-indent-guides-mode -1)
                    (highlight-indent-guides-mode t))))

;;; Window Resize
(gpack windresize
  :key (("C-' C-r" . windresize)
        (windresize-map . (("C-n" . windresize-down)
                           ("C-p" . windresize-up)
                           ("C-b" . windresize-left)
                           ("C-f" . windresize-right)))))

;;; youdao translate
(gpack youdao-dictionary
  :key ("C-' t" . youdao-dictionary-search-at-point))

;;; Treemacs : File explore
(gpack treemacs
  :key ("C-' e" . treemacs))

;;; GitHub Explorer
(gpack github-explorer
  :key ("C-' G" . github-explorer))

;;; Input Method
(gpack pyim
  :config (progn
            (pyim-basedict-enable)
            (pyim-isearch-mode t))
  :var ((default-input-method . "pyim")
        (pyim-default-scheme . 'quanpin)
        (pyim-page-tooltip . 'posframe)
        (pyim-page-length  . 8)
        (pyim-page-style . 'one-line)
        (pyim-punctuation-translate . '(auto yes no)))
  :key ("M-j" . pyim-convert-string-at-point))
(gpack pyim-basedict)

;;; Arch Package Manager

;;; Command Shower
(gpack command-log-mode
  :config (defun spring/open-or-close-command-log-mode ()
            "Open the command-log-mode."
            (interactive)
            (global-command-log-mode)
            (clm/toggle-command-log-buffer))
  :key (("C-' k" . spring/open-or-close-command-log-mode)
        ("C-' K" . clm/command-log-clear)))

;;; Transient
(gpack transient)

;;; License
(gpack license-templates)

;;; Restart
(gpack restart-emacs)

;;; Package-Lint
(gpack package-lint)

;;; Subtree
(gpack dired-subtree
  :key (dired-mode-map . (("TAB" . dired-subtree-toggle)
                          ("," . dired-subtree-up)
                          ("/" . dired-subtree-down))))

;;; Expand region
(gpack expand-region)

;;; MUtil-Term
(gpack multi-term
  :repo "manateelazycat/multi-term"
  :var (multi-term-program . "/bin/zsh"))

;;; Projectile
(gpack projectile)

;;; Interactive Align
(gpack ialign)

(provide 'init-tools)
