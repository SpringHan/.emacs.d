;;; git-download.el --- Git download packages -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: (emacs)


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

(defgroup git-download nil
  "Git download group."
  :group 'applications)

(defcustom git-download-store-path "~/.emacs.d/third-party/"
  "The path to store packages downloaded with git."
  :type 'string
  :group 'git-download)

(defcustom git-download-clone-process nil
  "The process for clone."
  :type 'process
  :group 'git-download)

(defconst git-download-clone-output-buffer "*GitDownload-Clone-Output*"
  "The buffer name of clone output.")

(defun git-download-ensure (name url &optional depth)
  "Check whether the package with its NAME is downloaded, if not download it."
  (let ((package-path (expand-file-name name git-download-store-path)))
    (unless (file-exists-p package-path)
      (git-download--clone (git-download--handle-url url)
                           package-path
                           depth))))

(defun git-download--handle-url (url)
  "Handle the URL for repository."
  (if (string-prefix-p "http" url)
      url
    (format "https://github.com/%s.git" url)))

(defun git-download--clone (url path depth)
  "Clone the repository from URL.
Argument PATH is the repository's PATH.
Argument DEPTH is the depth for cloning."
  (split-window nil nil 'above)
  (switch-to-buffer git-download-clone-output-buffer)
  (if depth
      (setq git-download-clone-process
            (start-process "Git-Download-Clone"
                           git-download-clone-output-buffer
                           "git"
                           "clone"
                           url
                           path
                           (format "--depth=%d" depth)))
    (setq git-download-clone-process
          (start-process "Git-Download-Clone"
                         git-download-clone-output-buffer
                         "git"
                         "clone"
                         url
                         path)))
  (set-process-sentinel git-download-clone-process
                        #'git-download--sentinel)
  (while (process-live-p git-download-clone-process)
    (read-char nil nil 0.1)))

(defun git-download--sentinel (process event)
  "Sentinel for clone process."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (when (string-match "^finished" event)
      (with-current-buffer git-download-clone-output-buffer
        (if (> (length (window-list)) 1)
            (kill-buffer-and-window)
          (kill-current-buffer)))

      (message "[Gpack]: Clone finished.Press any key to confirm.")
      (setq git-download-clone-process nil))))

;;; Check
(unless (file-exists-p git-download-store-path)
  (make-directory git-download-store-path))

(provide 'git-download)

;;; git-download.el ends here
