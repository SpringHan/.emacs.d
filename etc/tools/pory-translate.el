;;; pory-translate.el --- Pory translate -*- lexical-binding: t -*-

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

(defgroup pory-translate nil
  "Pory translate group."
  :group 'applications)

(defcustom pory-translate-target-lang "en"
  "The target language to be translated to."
  :type 'string
  :group 'pory-translate)

(defun pory-translate-insert (text target)
  "Translate TEXT to TARGET language and insert it."
  (interactive (let ((text (read-string "Enter the text: "))
                     (target-lang (completing-read "Translate to: "
                                                   '("en" "ja" "ko"))))
                 (list text target-lang)))
  (insert (pory-translate--get-result text target)))

(defun pory-translate-range ()
  "Translate text from selected region with default target language."
  (interactive)
  (unless (region-active-p)
    (user-error "[Pory-Translate]: You haven't selected any text!"))

  (let ((text (buffer-substring-no-properties (region-beginning)
                                              (region-end))))
    (message "[Pory-Result]: %s" (pory-translate--get-result
                                  text
                                  (if (pory-translate--contain-chinese-p text)
                                      pory-translate-target-lang
                                    "zh-cn")))))

(defun pory-translate-change-default-target (target)
  "Change default TARGET language."
  (interactive (list (completing-read "Enter target language: "
                                      '("en" "ja" "ko"))))
  (setq pory-translate-target-lang target))

(defun pory-translate--get-result (text language)
  "The function to get translated result."
  (string-trim
   (shell-command-to-string
    (format "pory -b traditional -t %s --plain '%s'"
            language
            text))))

(defun pory-translate--contain-chinese-p (str)
  "Whether STR contains chinese."
  (string-match-p "[\u2e80-\u9fff]" str))

(provide 'pory-translate)

;;; pory-translate.el ends here
