;;;; This file is the tools of my configuration.

;;; Third-party
;;; Emacs Application Framework
(package-require 'eaf
	:outside
	:before-load-eval '(add-to-list 'load-path "~/.emacs.d/third-party/emacs-application-framework")
	:keymap '(("C-q C-w l" . eaf-open-browser)
						("C-q C-w h" . eaf-open-browser-with-history)
						("C-q C-m b" . eaf-open-bookmark))
	:config '(progn
						 (eaf-setq eaf-browser-remember-history "true")
						 (eaf-setq eaf-browser-default-zoom "1.05")
						 (defun eaf-browser-set (&optional day)
							 (interactive)
							 (if (null day)
									 (if (day-or-night)
											 (eaf-setq eaf-browser-dark-mode "false")
										 (eaf-setq eaf-browser-dark-mode "true"))
								 (pcase day
									 ("day" (eaf-setq eaf-browser-dark-mode "false"))
									 ("night" (eaf-setq eaf-browser-dark-mode "true")))))
						 (eaf-browser-set))
	:hook '(eaf-mode-hook . (lambda () (evil-change-state 'emacs)
														(setq-local awesome-tray-active-modules '("mode-name" "date")))))

;;; English Teacher
(package-require 'english-teacher
	:outside
	:before-load-eval '(add-to-list 'load-path "~/.emacs.d/third-party/english-teacher.el")
	:keymap '(("C-' C-l" . english-teacher-smart-translation)
						("C-' T" . english-teacher-follow-mode))
	:config '(setq english-teacher-backend 'baidu
								 english-teacher-show-result-function 'english-teacher-eldoc-show-result-function)
	:hook '((Info-mode-hook eww-mode-hook help-mode-hook) . english-teacher-follow-mode))

;;; Netease Cloud Music
(package-require 'netease-cloud-music
	:outside
	:before-load-eval '(add-to-list 'load-path "~/.emacs.d/third-party/netease-cloud-music.el")
	:keymap '(("C-' C-m t" . netease-cloud-music)
						("C-' C-m r" . netease-cloud-music-change-repeat-mode))
	:child-package '(async request))


;;; Dashboard
(package-require 'dashboard
	:config '(progn
						 (dashboard-setup-startup-hook)
						 (setq dashboard-banner-logo-title "Live in Emacs!")
						 (setq dashboard-startup-banner 'logo)
						 (setq dashboard-center-content t
									 dashboard-set-heading-icons t
									 dashboard-set-navigator t
									 dashboard-init-info "SpringHan Emacs")))

;;; Counsel
(package-require 'counsel
	:keymap '(("M-x" . counsel-M-x)
						("C-x C-f" . counsel-find-file)
						("C-q a" . counsel-linux-app)))

;;; Counsel-etags
(package-require 'counsel-etags
	:keymap '(("C-]" . counsel-etags-find-tag-at-point)))

;;; Icons
(package-require 'all-the-icons
	:keymap '(("C-' C-i" . all-the-icons-insert)))

;;; ivy
(package-require 'ivy
	:hook '(after-init-hook . ivy-mode)
	:child-package '(posframe ivy-posframe)
	:child-config '(:posframe
									(setq posframe-mouse-banish nil)
									:ivy-posframe
									(progn
										(setq ivy-posframe-display-functions-alist
													'((t . ivy-posframe-display-at-frame-center)))
										(setq ivy-posframe-parameters '((left-fringe . 8)
																										(right-fringe . 8)))
										(add-hook 'ivy-mode-hook #'ivy-posframe-mode)))
	:keymap '(("C-s" . swiper)
						("C-r" . swiper-backward)
						("C-- s" . swiper-all)))

;;; Bongo
(package-require 'bongo)

;;; Which Key
(package-require 'which-key
	:hook '(after-init-hook . which-key-mode))

;;; ace window
(package-require 'ace-window
	:keymap '(("C-' C-c" . ace-window)))

;;; Calendar-China
(package-require 'cal-china-x)

;;; Iedit
(package-require 'iedit
	:keymap '(("C-' C-e" . iedit-mode)))

;;; hungry-delete
(package-require 'hungry-delete
	:keymap '(("C-' C-h" . hungry-delete-mode)
						("C-' DEL" . hungry-delete-backward))
	:hook '((emacs-lisp-mode-hook lisp-mode-hook) . hungry-delete-mode))

;;; rainbow-delimiters
(package-require 'rainbow-delimiters
	:hook '((lisp-mode-hook emacs-lisp-mode-hook org-mode-hooke eshell-mode-hook) . rainbow-delimiters-mode))

;;; indent guide
(package-require 'indent-guide
	:hook '(after-init-hook . indent-guide-global-mode))

;;; doom-modeline
(package-require 'doom-modeline
	:disable
	:hook '(after-init-hook . doom-modeline-mode)
	:config '(progn
						 (setq-default doom-modeline-height 13)
						 (setq-default doom-modeline-bar-width 3)))

;;; Window Resize
(package-require 'windresize
	:keymap '(("C-' C-r" . windresize)))

;;; youdao translate
(package-require 'youdao-dictionary
	:keymap '(("C-' t" . youdao-dictionary-search-at-point)))

;;; Treemacs : File explore
(package-require 'treemacs
	:keymap '(("C-' e" . treemacs))
	:hook '(treemacs-select-hook . (lambda () (evil-change-state 'emacs))))

;;; Caps_Lock
(package-require 'caps-lock
	:keymap '(("C-' g" . caps-lock-mode)))

;;; QuickRun
(package-require 'quickrun
	:keymap '(("C-' r" . quickrun-shell)))

;;; GitHub Explorer
(package-require 'github-explorer
	:keymap '(("C-' G" . github-explorer))
	:hook '(github-explorer-mode-hook . (lambda () (evil-change-state 'emacs))))

;;; Input Method
(package-require 'pyim
	:child-package 'pyim-basedict
	:child-config '(:pyim-basedict
									(progn
										(pyim-basedict-enable)))
	:config '(progn
						 (setq default-input-method "pyim"
									 pyim-default-scheme 'quanpin
									 pyim-page-tooltip 'posframe
									 pyim-page-length 8
									 pyim-page-style 'one-line
									 pyim-punctuation-translate '(auto yes no))
						 (pyim-isearch-mode t))
	:keymap '(("M-j" . pyim-convert-string-at-point)))

;;; Arch Package Manager
(package-require
		'arch-packer)

;;; Command Shower
(package-require 'command-log-mode
	:config '(defun spring/open-or-close-command-log-mode ()
						 "Open the command-log-mode."
						 (interactive)
						 (global-command-log-mode)
						 (clm/toggle-command-log-buffer))
	:keymap '(("C-' k" . spring/open-or-close-command-log-mode)
						("C-' K" . clm/command-log-clear)))

;;; Ranger
(package-require 'ranger
	:config '(progn
						 (ranger-override-dired-mode t)
						 (define-more-keys ranger-mode-map
							 (("n" ranger-up-directory)
								("k" ranger-search-next)
								("i" ranger-find-file)
								("uv" nil)
								("um" nil)
								("uv" nil)
								("u" ranger-prev-file)
								("e" ranger-next-file)))))

(provide 'init-tools)
