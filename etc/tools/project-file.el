;;; project-file.el --- Save & restore project files. -*- lexical-binding: t -*-

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

(defconst project-file-path (locate-user-emacs-file "project-files.cache")
  "Path to save project file caches.")

(defun project-file-open-cache ()
  "Open cache file."
  (interactive)
  (find-file project-file-path))

(defun project-file-save ()
  "Save project files in current Buffer Menu buffer."
  (interactive)
  (let ((project-name (read-string "Enter project name:"))
        file-list current-line current-path)
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-min))

        (while (ignore-errors (progn (next-line) t))
          (setq current-line (buffer-substring-no-properties
                              (line-beginning-position)
                              (line-end-position)))

          (when (string-match "[[:space:]]+\\(~.*\\)$" current-line)
            (add-to-list 'file-list (match-string 1 current-line))))

        (project-file--save-cache project-name file-list)))))

(defun project-file-open-list ()
  "Open marked file list & close cache file."
  (interactive)
  (unless (region-active-p)
    (user-error "You have not marked files you want to open!"))

  (let ((path-list (split-string (buffer-substring (region-beginning) (region-end))
                                 "\n")))
    (dolist (path path-list)
      (unless (string-empty-p path)
        (find-file-noselect path))))
  (kill-current-buffer))

(defun project-file--save-cache (name files)
  "Save FILES with NAME."
  (unless (file-exists-p project-file-path)
    (make-empty-file project-file-path))

  (let ((file-content (with-temp-buffer
                        (insert-file-contents project-file-path)
                        (buffer-string))))
    (with-temp-file project-file-path
      (unless (string-empty-p file-content)
        (insert file-content "\n"))

      (insert name "\n")
      (dolist (path files)
        (insert path "\n")))))

(provide 'project-file)

;;; project-file.el ends here
