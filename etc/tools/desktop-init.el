;;; desktop-init.el --- Desktop initialization -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Code:

(defcustom desktop-init-files nil
  "The desktop files need to be saved."
  :type 'list)

(defun desktop-init-save-buffers ()
  "Save current opening files."
  (let ((cache-file (locate-user-emacs-file "desktop-cache"))
        files tmp)
    (if (yes-or-no-p "Save current buffers?")
        (progn
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (when (and (not (string-prefix-p " " (buffer-name buffer)))
                         (or (setq tmp (buffer-file-name buffer))
                             (and (eq major-mode 'dired-mode)
                                  (setq tmp default-directory))))
                (add-to-list 'files tmp))))
          (setq files (append files desktop-init-files))

          (unless (file-exists-p cache-file)
            (make-empty-file cache-file))
          (with-temp-file cache-file
            (goto-char (point-min))
            (when files
              (dolist (path files)
                (insert path "\n")))))
      (with-temp-file cache-file))))

(defun desktop-init-reopen-files (&optional use-var-p)
  "Reopen files opened in previous time.
When USE-VAR-P is non-nil, reopen files in `desktop-init-files'.
Otherwise reopen files in the file `desktop-cache'."
  (interactive)
  (let ((cache-file (locate-user-emacs-file "desktop-cache"))
        files-string)
    (if (and (not use-var-p)
             (file-exists-p cache-file))
        (progn
          (setq files-string (with-temp-buffer
                               (insert-file-contents cache-file)
                               (buffer-substring-no-properties
                                (point-min) (point-max))))
          
          (unless (string-empty-p files-string)
            (dolist (path (split-string files-string "\n"))
              (unless (string-empty-p path)
                (find-file path)))))
      (dolist (file desktop-init-files)
        (find-file file)))))

(defun desktop-init-kill-all-files ()
  "Kill all opened files."
  (interactive)
  (let (temp)
    (setq desktop-init-files nil)
    (dolist (buffer (buffer-list))
      (when (and (not (string-prefix-p " " (buffer-name buffer)))
                 (or (setq temp (buffer-file-name buffer))
                     (and (eq major-mode 'dired-mode)
                          (setq temp default-directory))))
        (kill-buffer buffer)
        (add-to-list 'desktop-init-files temp t 'string-equal)))))

(add-hook 'kill-emacs-hook #'desktop-init-save-buffers)
(add-hook 'emacs-startup-hook #'desktop-init-reopen-files)

(provide 'desktop-init)

;;; desktop-init.el ends here
