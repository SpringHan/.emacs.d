;;;; This file is the tools of my configuration.

;;; Third-party
;;; Emacs Application Framework
(gpack ctable)
(gpack deferred)
(gpack epc)
(gpack eaf
  :load-path "~/.emacs.d/third-party/emacs-application-framework"
  :key (("C-q C-w l" . eaf-open-browser)
        ("C-q C-w h" . eaf-open-browser-with-history)
        ("C-q C-m b" . eaf-open-bookmark))
  :config (progn
            (setq eaf-browser-remember-history t)
            (setq eaf-browser-default-zoom 1.1)
            ;; (defun eaf-browser-set (&optional day)
            ;;   (interactive)
            ;;   (if (null day)
            ;;       (if (day-or-night)
            ;;           (setq eaf-browser-dark-mode nil)
            ;;         (setq eaf-browser-dark-mode "follow"))
            ;;     (pcase day
            ;;       ("day" (setq eaf-browser-dark-mode nil))
            ;;       ("night" (setq eaf-browser-dark-mode t)))))
            ;; (eaf-browser-set)
            )
  :hook (eaf-mode-hook . (lambda ()
                           (setq-local awesome-tray-active-modules
                                       '("netease-current-song" "input-method" "mode-name" "date")))))

;;; English Teacher
(gpack english-teacher
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
        ("C-' n t" . eaf-open-netease-cloud-music))
  :var ((netease-cloud-music-search-limit . 15)
        (netease-cloud-music-show-lyric . 'all))
  :config (require 'netease-cloud-music-ui))

;;; Shengci.el
(gpack shengci
  :repo "EvanMeek/shengci.el")

;;; Snails
(gpack snails
  :disable
  :repo "manateelazycat/snails")

;;; Safe
(gpack safe
  :disable
  :repo "SpringHan/safe"
  :key ("M-z" . safe))

;;; Move-text
(gpack move-text
  :repo "manateelazycat/move-text")

;;; Dired-File-Preview
(gpack dired-file-preview
  :repo "SpringHan/dired-file-preview")


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
(gpack counsel
  :key (("M-x" . counsel-M-x)
        ("C-x C-f" . counsel-find-file)
        ("C-q a" . counsel-linux-app)
        ("C-' A" . counsel-ag)))

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
        ("C-- s" . swiper-all)
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

;;; Caps_Lock
(gpack caps-lock
  :key ("C-' g" . caps-lock-mode))

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
(gpack helpful
  :key (("C-h f" . helpful-callable)
        ("C-h F" . helpful-function)
        ("C-h w" . helpful-command)
        ("C-h k" . helpful-key)
        ("C-h v" . helpful-variable)))

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
