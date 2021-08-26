;;; emulting.el --- A easy, multi-functional completing tool -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/SpringHan/.emacs.d.git


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; 

;;; Core

(defgroup emulting nil
  "Emulting group."
  :group 'application)

(defcustom emulting-input-buffer "*Emulting-Input*"
  "Input buffer."
  :type 'string
  :group 'emulting)

(defcustom emulting-result-buffer "*Emulting-Result*"
  "Result buffer."
  :type 'string
  :group 'emulting)

(defcustom emulting-extension-alist nil
  "Extension alist."
  :type 'list
  :group 'emulting)

(defcustom emulting-current-tab nil
  "Tab for emulting."
  :type 'number
  :group 'emulting)

(defcustom emulting-input-text-scale 1.25
  "Font increase scale."
  :type 'number
  :group 'emulting)

(defcustom emulting-last-buffer nil
  "The buffer where started emulting."
  :type 'buffer
  :group 'emulting)

(defcustom emulting-last-directory nil
  "The last buffer's directory."
  :type 'string
  :group 'emulting)

(defcustom emulting-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'sniem-minibuffer-keypad-start-or-stop)
    (define-key map (kbd "C-g") #'emulting-exit)
    map)
  "keymap for emulting-mode."
  :type 'keymap
  :group 'emulting)

;;;###autoload
(define-derived-mode emulting-mode nil "Emulting-Mode"
  "The major mode for Emulting buffer."
  :abbrev-table nil
  :syntax-table nil
  (kill-all-local-variables)
  (setq major-mode 'emulting-mode)
  (setq mode-name "Emulting-Mode")
  (company-mode -1)
  (use-local-map emulting-mode-map))

;;;###autoload
(defun emulting ()
  "Start emulting."
  (interactive)
  (if (or (get-buffer emulting-input-buffer)
          (get-buffer emulting-result-buffer))
      
      (emulting-exit)
    (setq emulting-last-buffer (current-buffer)
          emulting-last-directory default-directory)

    (tab-bar-new-tab)
    ;; (setq emulting-current-tab (tab-bar--current-tab-index))
    ;; TODO: There's some bug.
    ;; (tab-bar-select-tab emulting-current-tab)

    (emulting-input-buffer-init)
    (emulting-result-buffer-init)

    (switch-to-buffer-other-window emulting-input-buffer)))

(defun emulting-exit ()
  "Exit emulting."
  (interactive)
  (let (emulting-started)
    (when (get-buffer emulting-input-buffer)
      (setq emulting-started t)
      (kill-buffer emulting-input-buffer))
    (when (get-buffer emulting-result-buffer)
      (setq emulting-started t)
      (kill-buffer emulting-result-buffer))
    (when emulting-started
      (tab-bar-close-tab)))
  ;; (setq emulting-current-tab nil)
  )

(defun emulting-input-buffer-init ()
  "Initialize input buffer."
  (switch-to-buffer emulting-input-buffer)
  (with-current-buffer (get-buffer-create emulting-input-buffer)
    (erase-buffer)
    (emulting-mode)
    (run-hooks 'emulting-mode-hook)

    (text-scale-increase emulting-input-text-scale)
    (emulting-disable-settings)))

(defun emulting-result-buffer-init ()
  "initialize result buffer."
  (unless (get-buffer emulting-result-buffer)
    (let ((width (window-height)))
      (split-window nil (truncate (* 0.95 width)) 'above)
      (switch-to-buffer emulting-result-buffer)))
  (with-current-buffer (get-buffer-create emulting-result-buffer)
    (erase-buffer)
    (insert "test")
    (emulting-disable-settings t)
    (setq-local truncate-lines t)))

(defun emulting-disable-settings (&optional disable-cursor)
  "Disable normal settings.
When disable-cursor is non-nil, set `cursor-type' to nil."
  (when display-line-numbers
    (setq-local display-line-numbers nil))
  (setq-local header-line-format nil
              mode-line-format nil)
  (when (version< "27.0" emacs-version)
    (setq-local tab-line-format nil))
  (when disable-cursor
    (setq-local cursor-type nil)))

(defun emulting-extension-buffer-icon (buffer)
  "Icon function for BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p buffer 'eaf-mode)
        (all-the-icons-faicon "html5" :v-adjust 0.01)
      (all-the-icons-icon-for-buffer))))

;;; Functional functions for extension

(defmacro emulting-define-extension (name icon-function fliter-function execute-function)
  "The macro to define emulting extension.
NAME is the head-title which show in the result buffer.
ICON-FUNCTION is used for providing the icon for the result.
FLITER-FUNCTION is used to fliter result.
EXECUTE-FUNCTION is used to handle the result."
  (declare (indent defun))
  (let* ((async (eq (car fliter-function) 'async))
         (extension-symbol-name (replace-regexp-in-string " " "-" (downcase name)))
         (function-name (intern (concat "emulting-extension-"
                                        extension-symbol-name)))
         (variable-name (intern (concat "emulting-extension-var-"
                                        extension-symbol-name))))
    `(progn
       (defun ,function-name (content)
         (funcall ,fliter-function content))

       (defvar ,variable-name)

       (setq ,variable-name
             '((name . ,name)
               (fliter . ,function-name)
               (icon . ,icon-function)
               (execute . ,execute-function)))

       (add-to-list 'emulting-extension-alist '(,variable-name . nil)))))

(defun emulting-input-match (input content)
  "Check if INPUT is matched with CONTENT."
  (string-match-p (regexp-quote input) content))

(defmacro emulting-fliter-append (candidate val)
  "Add VAL to CANDIDATE."
  (declare ((debug t)))
  `(setq ,candidate (append ,candidate (list ,val))))

;;; Extensions
;;; Buffer
(defvar emulting-extension-buffer-blacklist
  (list
   emulting-input-buffer
   emulting-result-buffer
   " *code-conversion-work*"
   " *Echo Area "
   " *Minibuf-"
   " *Custom-Work*"
   " *pyim-page-tooltip-posframe-buffer*"
   " *load"
   " *server"
   ))

(defun emulting-extension-buffer-not-blacklist-buffer (buf)
  (catch 'failed
    (dolist (backlist-buf emulting-extension-buffer-blacklist)
      (when (string-prefix-p backlist-buf (buffer-name buf))
        (throw 'failed nil)))
    t))

(emulting-define-extension "BUFFER"
  (lambda (content)
    (emulting-extension-buffer-icon content))

  (lambda (input)
    (let (result)
      (dolist (buf (buffer-list))
        (when (and (emulting-extension-buffer-not-blacklist-buffer buf)
                   (emulting-input-match input (buffer-name buf)))
          (emulting-fliter-append result (buffer-name buf))))
      result))

  (lambda (content)
    (emulting-exit)
    (switch-to-buffer content)))

(emulting-define-extension "KILL BUFFER"
  (lambda (content)
    (emulting-extension-buffer-icon content))

  (lambda (input)
    (let (result)
      (dolist (buf (buffer-list))
        (when (and (emulting-extension-buffer-not-blacklist-buffer buf)
                   (emulting-input-match input (buffer-name buf)))
          (emulting-fliter-append result (buffer-name buf))))
      result))

  (lambda (content)
    (emulting-exit)
    (kill-buffer content)))

;;; Global keymap init
(global-set-key (kbd "M-z") #'emulting)

(provide 'emulting)

;;; emulting.el ends here
