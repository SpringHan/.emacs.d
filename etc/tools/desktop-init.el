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

(defun desktop-init-save-directory ()
  "Save current project directory."
  (let ((cache-file (locate-user-emacs-file "desktop-cache"))
        root-dir)
    (if (yes-or-no-p "Save current directory?")
        (with-current-buffer (current-buffer)
          (setq root-dir (citre-project-root))
          (unless (file-exists-p cache-file)
            (make-empty-file cache-file))
          (with-temp-file cache-file
            (goto-char (point-min))
            (insert root-dir "\n")
            (when spring/projects-in-use
              (dolist (project spring/projects-in-use)
                (insert project "\n")))))
      (with-temp-file cache-file))))

(defun desktop-init-goto-directory ()
  "Goto the newest directory in the cache file."
  (let ((cache-file (locate-user-emacs-file "desktop-cache"))
        target-dirs)
    (when (file-exists-p cache-file)
      (setq target-dirs (with-temp-buffer
                         (insert-file-contents cache-file)
                         (buffer-substring-no-properties
                          (point-min) (point-max))))
      (unless (string-empty-p target-dirs)
        (dolist (dir (split-string target-dirs "\n"))
          (dired dir))))))

(add-hook 'kill-emacs-hook #'desktop-init-save-directory)
(add-hook 'after-init-hook #'desktop-init-goto-directory)

(provide 'desktop-init)

;;; desktop-init.el ends here
