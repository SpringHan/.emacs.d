;;;; This file is the tools of my configuration.

;;; Third-party
;;; Emacs Application Framework
(gpack eaf
  :load-path "~/.emacs.d/third-party/emacs-application-framework"
  :key (("C-q C-w l" . eaf-open-browser)
        ("C-q C-w h" . eaf-open-browser-with-history)
        ("C-q C-m t" . eaf-open-terminal))
  :var ((eaf-browser-remember-history . t)
        (eaf-browser-default-zoom . 1.1)
        (eaf-terminal-font-family . "Source Code Pro")
        (eaf-terminal-font-size . 25)
        (eaf-kill-process-after-last-buffer-closed . t))
  :hook (eaf-mode-hook . (lambda ()
                           (setq-local awesome-tray-active-modules
                                       '("netease-current-song" "input-method" "mode-name" "date"))))
  :config
  (progn
    (require 'eaf-terminal)
    (require 'eaf-image-viewer)
    (require 'eaf-js-video-player)
    (require 'eaf-video-player)
    (require 'eaf-markdown-previewer)
    (require 'eaf-pdf-viewer)
    (require 'eaf-browser)
    (require 'eaf-file-sender)
    (require 'eaf-org-previewer)
    (require 'eaf-airshare)
    (require 'eaf-file-browser)
    (require 'eaf-netease-cloud-music)))

;;; English Teacher
(gpack english-teacher
  :disable
  :repo "loyalpartner/english-teacher.el"
  :key (("C-' C-l" . english-teacher-smart-translation)
        ("C-' T" . english-teacher-follow-mode))
  :var ((english-teacher-backend . 'baidu)
        (english-teacher-show-result-function . 'english-teacher-eldoc-show-result-function))
  :hook ((Info-mode-hook eww-mode-hook help-mode-hook helpful-mode-hook) . english-teacher-follow-mode))

;;; Netease Cloud Music
(gpack request)
(gpack netease-cloud-music
  :repo "SpringHan/netease-cloud-music.el"
  :key (("C-' n q" . netease-cloud-music-quit)
        ("C-' n t" . (lambda ()
                       (interactive)
                       (if (get-buffer netease-cloud-music-buffer-name)
                           (netease-cloud-music)
                         (eaf-open-netease-cloud-music)))))
  :var ((netease-cloud-music-search-limit . 15)
        (netease-cloud-music-show-lyric . 'all)
        (netease-cloud-music-line-number-relative . t))
  :config (progn
            (require 'netease-cloud-music-ui)
            (require 'netease-cloud-music-comment)))

;;; Shengci.el
(gpack shengci
  :repo "EvanMeek/shengci.el")

;;; Snails
(gpack snails
  :disable
  :repo "manateelazycat/snails")

;;; Move-text
(gpack move-text
  :repo "manateelazycat/move-text")

;;; Dired-File-Preview
(gpack dired-file-preview
  :repo "SpringHan/dired-file-preview")

;;; Screenshot
(gpack screenshot
  ;; This extension needs the require with `imagemagick'
  :repo "tecosaur/screenshot")


;;; Dashboard
(gpack dashboard
  :disable
  :var ((dashboard-banner-logo-title . "Live in Emacs!")
        (dashboard-startup-banner . 'logo)
        (dashboard-center-content . t)
        (dashboard-set-heading-icons . t)
        (dashboard-set-navigator . t)
        (dashboard-init-info . "SpringHan Emacs"))
  :config (dashboard-setup-startup-hook))

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

;;; hungry-delete
(gpack hungry-delete
  :key (("C-' C-h" . hungry-delete-mode)
        ("C-' DEL" . hungry-delete-backward))
  :hook ((emacs-lisp-mode-hook lisp-mode-hook) . hungry-delete-mode))

(gpack highlight-indent-guides
  :hook ((prog-mode-hook text-mode-hook) . highlight-indent-guides-mode)
  :var ((highlight-indent-guides-method . 'character)
        (highlight-indent-guides-delay . 0))
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
(gpack pyim-basedict)
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

;;; Arch Package Manager
(gpack arch-packer)

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

;;; Helpful - For more helpful help
(gpack helpful)

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

(provide 'init-tools)
