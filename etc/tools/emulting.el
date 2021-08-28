;;; emulting.el --- A easy, multi-functional completing tool -*- lexical-binding: t -*-

;; Author: SpringHan
;; Maintainer: SpringHan
;; Version: 1.0
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

;;; Code

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

(defcustom emulting-extension-result nil
  "Result list for extension."
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

(defcustom emulting-input-match-timer nil
  "The timer for checking input match."
  :type 'timer
  :group 'emulting)

(defcustom emulting-current-extension nil
  "Current selected extension."
  :type 'number
  :group 'emulting)

(defcustom emulting-selected-overlay nil
  "Selected overlay."
  :type 'overlay
  :group 'emulting)

(defcustom emulting-selected-candidate nil
  "The candidate which is selected."
  :type 'string
  :group 'emulting)

(defcustom emulting-adjusting-overlay nil
  "If now the overlay is adjusting."
  :type 'boolean
  :group 'emulting)

(defface emulting-header-title-face
  '((t :height 1.5 :inherit awesome-tray-module-location-face))
  "Face for header title."
  :group 'emulting)

(defcustom emulting-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map " " #'sniem-minibuffer-keypad-start-or-stop)
    (define-key map (kbd "C-g") #'emulting-exit)
    (define-key map (kbd "C-k") #'emulting-extension-kill-buffer)
    (define-key map (kbd "C-n") #'emulting-next-item)
    (define-key map (kbd "C-p") #'emulting-prev-item)
    (define-key map (kbd "M-n") #'emulting-next-extension)
    (define-key map (kbd "M-p") #'emulting-prev-extension)
    (define-key map (kbd "RET") #'emulting-candidate-do)
    (define-key map (kbd "C-m") #'emulting-candidate-do)
    (define-key map (kbd "<C-return>") #'emulting-immediate-do)
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
    ;; (tab-bar-select-tab emulting-current-tab)

    (emulting-input-buffer-init)
    (emulting-result-buffer-init)

    (switch-to-buffer-other-window emulting-input-buffer)

    (setq emulting-input-match-timer (run-with-timer
                                      0 0.4 #'emulting-match-input))))

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
  (when (timerp emulting-input-match-timer)
    (cancel-timer emulting-input-match-timer)
    (setq emulting-input-match-timer nil))
  (setq emulting-current-extension nil
        emulting-adjusting-overlay nil
        emulting-selected-candidate nil
        emulting-selected-overlay nil)
  (emulting-clear-result))

(defun emulting-next-item ()
  "Select the next item."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (emulting-adjust-selected-overlay 'next)))

(defun emulting-prev-item ()
  (interactive)
  (with-current-buffer emulting-result-buffer
    (emulting-adjust-selected-overlay 'prev)))

(defun emulting-next-extension ()
  "Goto the next extension."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (let ((extension (1+ emulting-current-extension)))
      (when (nth extension (emulting-get-extension-has-result))
        (delete-overlay emulting-selected-overlay)
        (setq emulting-current-extension extension)
        (emulting-goto-extension extension)
        (forward-line)
        (emulting-select-current-candidate)))))

(defun emulting-prev-extension ()
  "Goto the prev extension."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (let ((extension (1- emulting-current-extension)))
      (when (nth extension (emulting-get-extension-has-result))
        (delete-overlay emulting-selected-overlay)
        (setq emulting-current-extension extension)
        (emulting-goto-extension extension)
        (forward-line)
        (emulting-select-current-candidate)))))

(defun emulting-candidate-do ()
  "Execute the function for current candidate."
  (interactive)
  (if (string-empty-p (with-current-buffer emulting-result-buffer
                        (buffer-string)))
      (user-error "[Emulting]: There's no candidate.")
    (when (and emulting-current-extension
               emulting-selected-candidate)
      (funcall (alist-get 'execute
                          (symbol-value
                           (nth emulting-current-extension
                                (emulting-get-extension-has-result))))
               (emulting-get-main-candidate emulting-selected-candidate)))))

(defun emulting-immediate-do ()
  "Like `emulting-candidate-do', but do not follow the candidate."
  (interactive)
  (if (string-empty-p (with-current-buffer emulting-result-buffer
                        (buffer-string)))
      (user-error "[Emulting]: There's no candidate.")
    (when emulting-current-extension
      (funcall (alist-get 'execute
                          (symbol-value
                           (nth emulting-current-extension
                                (emulting-get-extension-has-result))))
               (with-current-buffer emulting-input-buffer
                 (buffer-string))))))

(defun emulting-clear-result ()
  "Clear the match results."
  (let (extensions)
    (dolist (result emulting-extension-result)
      (setq extensions (append extensions (list (list (car result))))))
    (setq emulting-extension-result extensions)))

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

(defun emulting-update-result-buffer ()
  "Update result buffer."
  (with-current-buffer emulting-result-buffer
    (erase-buffer)
    (let (extension icon)
      (dolist (result (emulting-get-extension-result))
        (setq extension (symbol-value (car result))
              icon (alist-get 'icon extension))
        (insert (propertize (alist-get 'name extension)
                            'face 'emulting-header-title-face)
                "\n")
        (mapcar (lambda (r)
                  (insert (if icon
                              (funcall icon r)
                            "")
                          " " r "\n"))
                (cdr result))
        (insert "\n")))
    (emulting-adjust-selected-overlay)))

(defun emulting-adjust-selected-overlay (&optional moved)
  "Adjust the selected overlay.
If MOVED is non-nil, it'll not change the overlay to `emulting-selected-candidate'."
  (unless emulting-adjusting-overlay
    (if (and (null emulting-selected-overlay)
             (null emulting-selected-candidate))
        (progn
          (goto-char (point-min))
          (forward-line)
          (setq emulting-current-extension 0)
          (emulting-select-current-candidate))

      (setq emulting-adjusting-overlay t)

      (if moved
          (let ((line-arg (when (eq moved 'prev)
                            -1)))
            (goto-char (overlay-start emulting-selected-overlay))
            (delete-overlay emulting-selected-overlay)

            (forward-line line-arg)
            (cond ((or (= (line-beginning-position) (line-end-position))
                       (emulting-header-title-p))
                   (if line-arg
                       (emulting-prev-extension)
                     (emulting-next-extension)))
                  ((bobp)
                   (forward-line)
                   (setq emulting-current-extension 0)
                   (emulting-select-current-candidate))
                  (t (emulting-select-current-candidate))))

        (unless (string-empty-p (buffer-string))
          (delete-overlay emulting-selected-overlay)
          (emulting-goto-extension emulting-current-extension)
          (let (temp)
            (setq temp
                  (catch 'stop
                    (save-excursion
                      (while (or (not (emulting-header-title-p))
                                 (not (eobp)))
                        (when (string= (buffer-substring (line-beginning-position)
                                                         (line-end-position))
                                       emulting-selected-candidate)
                          (throw 'stop (cons (line-beginning-position)
                                             (line-end-position))))
                        (forward-line)))))
            (if temp
                (progn
                  (goto-char (car temp))
                  (setq emulting-selected-overlay (make-overlay (car temp)
                                                                (cdr temp))
                        emulting-selected-candidate (buffer-substring
                                                     (car temp) (cdr temp)))
                  (overlay-put emulting-selected-overlay 'face 'region)
                  (emulting-keep-cursor-visible))
              (forward-line)
              (emulting-select-current-candidate)))))
      (setq emulting-adjusting-overlay nil))))

(defun emulting-match-input ()
  "Match input for extensions."
  (if (not (or (get-buffer emulting-input-buffer)
               (get-buffer emulting-result-buffer)))
      (emulting-exit)
    (let ((input (with-current-buffer emulting-input-buffer
                   (buffer-string))))
      (dolist (extension emulting-extension-alist)
        (funcall (alist-get 'fliter (symbol-value extension)) input)))))

(defun emulting-keep-cursor-visible ()
  "Keep the selected overlay visible."
  (when (get-buffer-window emulting-result-buffer)
    (set-window-point (get-buffer-window emulting-result-buffer) (point))))

(defun emulting-header-title-p ()
  "If the cursor is on the header title."
  (eq (get-text-property (point) 'face) 'emulting-header-title-face))

(defun emulting-select-current-candidate ()
  "Select the candidate under cursor."
  (unless (= (line-beginning-position)
             (line-end-position))
    (setq emulting-selected-overlay (make-overlay (line-beginning-position)
                                                  (line-end-position))
          emulting-selected-candidate
          (buffer-substring (overlay-start emulting-selected-overlay)
                            (overlay-end emulting-selected-overlay)))
    (overlay-put emulting-selected-overlay 'face 'region)
    (emulting-keep-cursor-visible)))

(defun emulting-goto-extension (index)
  "Goto the extension by its INDEX."
  (let ((extension (nth index (emulting-get-extension-has-result)))
        name)
    (if extension
        (progn
          (setq name (alist-get 'name (symbol-value extension)))
          (goto-char (point-min))
          (while (not (and (string= (buffer-substring (line-beginning-position)
                                                      (line-end-position))
                                    name)
                           (emulting-header-title-p)))
            (forward-line)))
      (goto-char (point-min))
      (forward-line)
      (setq emulting-current-extension 0))))

(defun emulting-get-extension-has-result ()
  "Get all the extensions that has non-nil result."
  (let (extensions)
    (dolist (result emulting-extension-result)
      (when (cdr-safe result)
        (setq extensions (append extensions (list (car result))))))
    extensions))

(defun emulting-get-extension-result ()
  "Get the extension's result which is non-nil."
  (let (results)
    (dolist (result emulting-extension-result)
      (when (cdr-safe result)
        (setq results (append results (list result)))))
    results))

(defun emulting-get-main-candidate (candidate)
  "Get the main candiate from which one includes icon."
  (progn
    (string-match "^\\(.*\\) \\(.*\\)$" candidate)
    (match-string 2 candidate)))

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

       (add-to-list 'emulting-extension-alist ',variable-name t)
       (add-to-list 'emulting-extension-result '(,variable-name) t))))

(defun emulting-input-match (input content)
  "Check if INPUT is matched with CONTENT."
  (ivy--re-filter (ivy--regex input) content))

(defun emulting-change-candidate (extension candidate)
  "Change EXTENSION's CANDIDATE."
  (let ((index (spring/get-index extension emulting-extension-alist)))
    (unless (equal (cdr (nth index emulting-extension-result))
                   candidate)
      (setf (nth index emulting-extension-result)
            (cons extension candidate))
      (emulting-update-result-buffer))))

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
      (when (string-prefix-p backlist-buf buf)
        (throw 'failed nil)))
    t))

(defun emulting-extension-kill-buffer ()
  "Kill buffer."
  (interactive)
  (if (string-empty-p (with-current-buffer emulting-result-buffer
                        (buffer-string)))
      (user-error "[Emulting]: There's no candidate.")
    (when (and emulting-current-extension
               emulting-selected-candidate)
      (kill-buffer (emulting-get-main-candidate emulting-selected-candidate))
      (emulting-exit))))

(emulting-define-extension "BUFFER"
  (lambda (content)
    (emulting-extension-buffer-icon content))

  (lambda (input)
    (let (buffer-name result)
      (dolist (buf (buffer-list))
        (setq buffer-name (buffer-name buf))
        (when (and (emulting-extension-buffer-not-blacklist-buffer buffer-name)
                   (emulting-input-match input (list buffer-name)))
          (emulting-fliter-append result buffer-name)))
      (emulting-change-candidate 'emulting-extension-var-buffer result)))

  (lambda (content)
    (emulting-exit)
    (switch-to-buffer content)))

(defvar emulting-extension-command nil
  "commands")

(defun emulting-extension-command-get-commands ()
  (let (cmds)
    (mapatoms (lambda (s) (when (commandp s) (push (symbol-name s) cmds))))
    (setq emulting-extension-command cmds)))

(add-hook 'after-init-hook #'emulting-extension-command-get-commands)

(run-with-idle-timer 2 nil #'emulting-extension-command-get-commands)

(run-with-idle-timer 60 t #'emulting-extension-command-get-commands)

(defun emulting-extension-command-wrap-command-with-key (command)
  (let ((keys (mapconcat
               'key-description
               (where-is-internal (intern command))
               " ")))
    (if (equal keys "")
        command
      (format "%s 「 %s 」" command keys))))

(emulting-define-extension "COMMAND"
  nil
  (lambda (input)
    (let (candidates)
      (catch 'stop
        (dolist (cmd emulting-extension-command)
          (when (emulting-input-match input (list cmd))
            (emulting-fliter-append candidates
                                    (emulting-extension-command-wrap-command-with-key
                                     cmd)))
          (when (> (length candidates) 20)
            (throw 'stop t))))
      (emulting-change-candidate 'emulting-extension-var-command candidates)))

  (lambda (candidate)
    (emulting-exit)
    (call-interactively (intern candidate))))

;;; Global keymap init
(global-set-key (kbd "M-z") #'emulting)

(provide 'emulting)

;;; emulting.el ends here
