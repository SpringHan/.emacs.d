;;;; This is the awesome-tray settings for my emacs configuration.  -*- lexical-binding: t; -*-

;;; awesome-tray
(use-package awesome-tray
  :defer nil
  :init
  (git-download-ensure "awesome-tray" "manateelazycat/awesome-tray" 1 t)
  (spring/extra-add-to-list "~/.emacs.d/third-party/awesome-tray/awesome-tray" nil t)
  :load-path "~/.emacs.d/third-party/awesome-tray"
  :hook (after-init-hook . awesome-tray-mode)

  :config
  (defun awesome-tray-module-input-method-info ()
    (pcase current-input-method
      ('nil "")
      ("rime" "ZH")
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

  (defun awesome-tray-buffer-name ()
    "Return current buffer name.
Return the parent path of it when it's a file. Otherwise
only return the buffer name."
    (let ((file (buffer-file-name (current-buffer)))
          root)
      (if (null file)
          (buffer-name (current-buffer))
        (setq root (vc-root-dir))

        (if root
            (file-relative-name file root)
          (if (string-prefix-p (getenv "HOME") file)
              (file-relative-name file "~")
            (buffer-name (current-buffer)))))))

  (add-to-list 'awesome-tray-module-alist '("buffer-read-only" . (awesome-tray-read-only awesome-tray-module-parent-dir-face)))
  (add-to-list 'awesome-tray-module-alist '("buffer-modified-p" . (awesome-tray-buffer-modified awesome-tray-module-date-face)))
  (add-to-list 'awesome-tray-module-alist '("buffer-name" . (awesome-tray-buffer-name awesome-tray-module-location-face)))

  (setq awesome-tray-active-modules '("sniem-state" "input-method" "buffer-read-only"
                                      "buffer-modified-p" "git" "buffer-name" "mode-name")))

(provide 'init-awesome-tray)
