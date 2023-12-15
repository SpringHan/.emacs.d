;;;; This file is used for emacs UI
(menu-bar-mode -1) ; Close the menu bar
(tool-bar-mode -1) ; Close the tool bar
(scroll-bar-mode -1) ; Close Scroll bar
(tab-bar-mode -1) ; Set tab bar not display
(blink-cursor-mode -1) ; Close cursor blink
(setq tab-bar-show nil) ; Always not display tab bar
;;; Set the line number as the relative style
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(global-hl-line-mode -1) ; Highlight the current line
(toggle-frame-fullscreen) ; Set fullscreen
(setq cursor-type 'box) ; Cursor Shape
(setq inhibit-splash-screen t) ; Close the start flash
(set-face-attribute
 'default nil
 :height 140
 :family "Monego"
 :weight 'normal
 :width 'normal) ; Set the font size
;; Set backgroup alpha
(unless (file-exists-p
         (expand-file-name (locate-user-emacs-file "not-alpha")))
  (set-frame-parameter nil 'alpha-background 75))

(defcustom spring/truncate-check-timer nil
  "Timer for checking whether to truncate lines."
  :type 'timer)

(defcustom spring/truncate-operated-buffers nil
  "Buffers that have been operated, namely be set to truncate line."
  :type 'list)

(defun spring/truncate-line ()
  "Determines whether to truncate lines of current buffer.
It is determined by spliting window."
  (if (> (length (window-list)) 1)
      (let (buffer)
        (dolist (window (window-list))
          (setq buffer (window-buffer window))
          (unless (memq buffer spring/truncate-operated-buffers)
            (with-current-buffer buffer
              (toggle-truncate-lines t))
            (add-to-list 'spring/truncate-operated-buffers buffer))))
    (dolist (buffer spring/truncate-operated-buffers)
      (toggle-truncate-lines -1))
    (cancel-timer spring/truncate-check-timer)
    (setq spring/truncate-operated-buffers nil
          spring/truncate-check-timer nil)))

(advice-add #'split-window-right :after
            (lambda (orig &optional window size side pixelwise)
              (setq spring/truncate-check-timer
                    (run-with-idle-timer 0.5 3 #'spring/truncate-line))))

(provide 'init-ui)
