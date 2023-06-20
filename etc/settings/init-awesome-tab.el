;;;; This is the awesome tab config.

;;; Awesome-Tab
(gpack awesome-tab
  :repo "manateelazycat/awesome-tab"
  :hook (after-init-hook . awesome-tab-mode)
  :var (awesome-tab-height . 150))

(spring/extra-add-to-list "~/.emacs.d/third-party/awesome-tab/awesome-tab")

(defhydra awesome-fast-switch (:hint nil)
  "
 ^^^^Fast Move             ^^^^Tab                    ^^Search            ^^Misc
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
   ^_k_^   prev group    | _C-a_^^     select first | _b_ search buffer | _C-k_   kill buffer
 _h_   _l_  switch tab   | _C-e_^^     select last  | _g_ search group  | _C-S-k_ kill others in group
   ^_j_^   next group    | _C-j_^^     ace jump     | ^^                | ^^
 ^^0 ~ 9^^ select window | _C-h_/_C-l_ move current | ^^                | ^^
-^^^^--------------------+-^^^^---------------------+-^^----------------+-^^---------------------------
"
  ("h" awesome-tab-backward-tab)
  ("j" awesome-tab-forward-group)
  ("k" awesome-tab-backward-group)
  ("l" awesome-tab-forward-tab)
  ("0" my-select-window)
  ("1" my-select-window)
  ("2" my-select-window)
  ("3" my-select-window)
  ("4" my-select-window)
  ("5" my-select-window)
  ("6" my-select-window)
  ("7" my-select-window)
  ("8" my-select-window)
  ("9" my-select-window)
  ("C-a" awesome-tab-select-beg-tab)
  ("C-e" awesome-tab-select-end-tab)
  ("C-j" awesome-tab-ace-jump)
  ("C-h" awesome-tab-move-current-tab-to-left)
  ("C-l" awesome-tab-move-current-tab-to-right)
  ("b" ivy-switch-buffer)
  ("g" awesome-tab-counsel-switch-group)
  ("C-k" kill-current-buffer)
  ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
  ("q" nil "quit"))

(defun spring/awesome-tab-adjust-color-with-theme (orig)
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (select-tab-background (awesome-tab-get-select-background-color))
         (unselect-tab-background (awesome-tab-get-unslect-background-color)))
    (when (display-graphic-p)
      (setq awesome-tab-active-bar (awesome-tab-make-xpm awesome-tab-active-bar-width awesome-tab-active-bar-height)))

    (set-face-attribute awesome-tab-display-line nil :height awesome-tab-height)

    (set-face-attribute 'awesome-tab-selected-face nil
                        :background select-tab-background)
    (set-face-attribute 'awesome-tab-unselected-face nil
                        :background unselect-tab-background)

    (set-face-attribute 'awesome-tab-selected-ace-face nil
                        :background select-tab-background)
    (set-face-attribute 'awesome-tab-unselected-ace-face nil
                        :background unselect-tab-background)

    (set-face-attribute 'awesome-tab-selected-index-face nil
                        :background select-tab-background)
    (set-face-attribute 'awesome-tab-unselected-index-face nil
                        :background unselect-tab-background)))

(advice-add #'awesome-tab-adjust-color-with-theme :around
            #'spring/awesome-tab-adjust-color-with-theme)

(provide 'init-awesome-tab)
