;;;; This is the awesome-tray settings for my emacs configuration.

;;; awesome-tray
(gpack awesome-tray
  :repo "manateelazycat/awesome-tray"
  :hook (after-init-hook . awesome-tray-mode))

(spring/extra-add-to-list "~/.emacs.d/third-party/awesome-tray/awesome-tray")

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

(add-to-list 'awesome-tray-module-alist '("buffer-read-only" . (awesome-tray-read-only awesome-tray-module-parent-dir-face)))
(add-to-list 'awesome-tray-module-alist '("buffer-modified-p" . (awesome-tray-buffer-modified awesome-tray-module-date-face)))
(add-to-list 'awesome-tray-module-alist '("netease-current-song" . (awesome-tray-netease-current-song awesome-tray-module-mode-name-face)))
(add-to-list 'awesome-tray-module-alist '("buffer-name" . (buffer-name awesome-tray-module-location-face)))

(setq awesome-tray-active-modules '("sniem-state" "input-method" "buffer-read-only"
                                    "buffer-modified-p" "git" "buffer-name" "mode-name" "date"))

(provide 'init-awesome-tray)
