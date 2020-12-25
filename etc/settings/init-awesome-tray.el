;;;; This is the awesome-tray settings for my emacs configuration.

;;; awesome-tray
(gpack awesome-tray
  :repo "manateelazycat/awesome-tray"
  :hook (after-init-hook . awesome-tray-mode))

(defun awesome-tray-module-input-method-info ()
  (pcase current-input-method
    ('nil "EN")
    ("pyim" "ZN")
    ("japanese" "JA")))

(defun awesome-tray-read-only ()
  (if (eq buffer-read-only t)
      "read-only"
    ""))

(defun awesome-tray-buffer-modified ()
  (if (buffer-modified-p)
      "*"
    ""))

(defun awesome-tray-netease-current-song ()
  (when (netease-cloud-music-process-live-p)
    (format "%s-%s"
            (car netease-cloud-music-current-song)
            (nth 1 netease-cloud-music-current-song))))

(defun spring/disable-modeline ()
  "The function to disable the modeline."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground awesome-tray-mode-line-active-color
                      :background awesome-tray-mode-line-active-color
                      :height 0.1
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground awesome-tray-mode-line-inactive-color
                      :background awesome-tray-mode-line-inactive-color
                      :height 0.1
                      :box nil
                      :inherit 'unspecified))

(add-to-list 'awesome-tray-module-alist '("buffer-read-only" . (awesome-tray-read-only awesome-tray-module-parent-dir-face)))
(add-to-list 'awesome-tray-module-alist '("buffer-modified-p" . (awesome-tray-buffer-modified awesome-tray-module-date-face)))
(add-to-list 'awesome-tray-module-alist '("netease-current-song" . (awesome-tray-netease-current-song awesome-tray-module-mode-name-face)))

(setq awesome-tray-active-modules '("evil" "netease-current-song" "input-method" "location" "buffer-read-only"
                                    "buffer-modified-p" "git" "mode-name" "date"))

(provide 'init-awesome-tray)
