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

;;; Changelog
;;; 2021-08-29
;; 	* etc/tools/emulting.el: Added some daily used extension, next step to add async extension

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

(defcustom emulting-selected-candidate-data-list nil
  "The list for candidate data."
  :type 'list
  :group 'emulting)

(defcustom emulting-adjusting-overlay nil
  "If now the overlay is adjusting."
  :type 'boolean
  :group 'emulting)

(defcustom emulting-only-extension nil
  "The only used extension."
  :type 'number
  :group 'emulting)

(defcustom emulting-start-prefix nil
  "If starting to insert prefix."
  :type 'boolean
  :group 'emulting)

(defcustom emulting-whole-start nil
  "If this time is using `emulting' to start."
  :type 'boolean
  :group 'emulting)

(defcustom emulting-just-refreshed nil
  "If the current extension's result has been refreshed."
  :type 'boolean
  :group 'emulting)

(defcustom emulting-clear-variables nil
  "The variables that need to be set to nil after process."
  :type 'list
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
    (define-key map (kbd "M-<") #'emulting-goto-beginning)
    (define-key map (kbd "M->") #'emulting-goto-end)
    (define-key map (kbd "C-<") #'emulting-select-first-item)
    (define-key map (kbd "C->") #'emulting-select-last-item)
    (define-key map (kbd "RET") #'emulting-candidate-do)
    (define-key map (kbd "C-m") #'emulting-candidate-do)
    (define-key map (kbd "TAB") #'emulting-complete)
    (define-key map (kbd "C-i") #'emulting-complete)
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
(defun emulting (&optional extension)
  "Start emulting."
  (interactive)
  (if (or (get-buffer emulting-input-buffer)
          (get-buffer emulting-result-buffer))
      
      (emulting-exit)
    (setq emulting-last-buffer (current-buffer)
          emulting-last-directory default-directory)

    (let ((tab-bar-mode t))
      (tab-bar-new-tab))
    ;; (setq emulting-current-tab (tab-bar--current-tab-index))
    ;; (tab-bar-select-tab emulting-current-tab)

    (emulting-input-buffer-init)
    (emulting-result-buffer-init)

    (switch-to-buffer-other-window emulting-input-buffer)

    (if extension
        (with-current-buffer emulting-input-buffer
          (erase-buffer)
          (insert "#"
                  (replace-regexp-in-string "-" " " (symbol-name extension))
                  ":"))
      (setq emulting-whole-start t))

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
      (let ((tab-bar-mode t))
        (tab-bar-close-tab))))
  (when (timerp emulting-input-match-timer)
    (cancel-timer emulting-input-match-timer)
    (setq emulting-input-match-timer nil))
  (setq emulting-whole-start nil
        emulting-start-prefix nil
        emulting-current-extension nil
        emulting-adjusting-overlay nil
        emulting-selected-candidate nil
        emulting-selected-overlay nil
        emulting-selected-candidate-data-list nil
        emulting-only-extension nil
        emulting-last-buffer nil
        emulting-last-directory nil
        emulting-just-refreshed nil)
  (emulting-clear-result)
  (emulting-clear-variable))

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
      (when (and (null emulting-only-extension)
                 (nth extension (emulting-get-extension-has-result)))
        (delete-overlay emulting-selected-overlay)
        (setq emulting-current-extension extension
              emulting-just-refreshed nil)
        (emulting-goto-extension extension)
        (forward-line)
        (emulting-select-current-candidate)))))

(defun emulting-prev-extension ()
  "Goto the prev extension."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (let ((extension (1- emulting-current-extension)))
      (when (and (null emulting-only-extension)
                 (nth extension (emulting-get-extension-has-result)))
        (delete-overlay emulting-selected-overlay)
        (setq emulting-current-extension extension
              emulting-just-refreshed nil)
        (emulting-goto-extension extension)
        (forward-line)
        (emulting-select-current-candidate)))))

(defun emulting-select-first-item ()
  "Select the first item."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (if emulting-only-extension
        (progn
          (goto-char (point-min))
          (forward-line))
      (emulting-goto-extension emulting-current-extension)
      (forward-line))
    (delete-overlay emulting-selected-overlay)
    (emulting-select-current-candidate)))

(defun emulting-select-last-item ()
  "Select the last item."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (if emulting-only-extension
        (progn
          (goto-char (point-max))
          (forward-line -2))
      (emulting-goto-extension emulting-current-extension)
      (while (not (= (line-beginning-position) (line-end-position)))
        (forward-line))
      (forward-line -1))
    (delete-overlay emulting-selected-overlay)
    (emulting-select-current-candidate)))

(defun emulting-goto-beginning ()
  "Goto the beginning."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (goto-char (point-min))
    (forward-line)
    (unless emulting-only-extension
      (setq emulting-current-extension 0))
    (delete-overlay emulting-selected-overlay)
    (emulting-select-current-candidate)))

(defun emulting-goto-end ()
  "Goto the end."
  (interactive)
  (with-current-buffer emulting-result-buffer
    (goto-char (point-max))
    (forward-line -2)
    (unless emulting-only-extension
      (setq emulting-current-extension (1- (length (emulting-get-extension-has-result)))))
    (delete-overlay emulting-selected-overlay)
    (emulting-select-current-candidate)))

(defun emulting-candidate-do ()
  "Execute the function for current candidate."
  (interactive)
  (if (string-empty-p (with-current-buffer emulting-result-buffer
                        (buffer-string)))
      (user-error "[Emulting]: There's no candidate.")
    (when (and emulting-current-extension
               emulting-selected-candidate)
      (let* ((candidate (emulting-get-main-candidate emulting-selected-candidate))
             (data (emulting-extension-get-data candidate
                                                (if emulting-only-extension
                                                    emulting-only-extension
                                                  emulting-current-extension)
                                                (when emulting-only-extension
                                                  t)))
             (func (alist-get 'execute
                              (symbol-value
                               (if emulting-only-extension
                                   (nth emulting-only-extension
                                        emulting-extension-alist)
                                 (nth emulting-current-extension
                                      (emulting-get-extension-has-result)))))))
        (when func
          (funcall func (if data
                            data
                          candidate)))))))

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
               (emulting-get-input)))))

(defun emulting-complete ()
  "Complete by the selected candidate."
  (interactive)
  (with-current-buffer emulting-input-buffer
    (let ((input (buffer-string))
          completion prefix content condidate)
      (when (and emulting-current-extension
                 emulting-selected-candidate)
        (setq condidate (emulting-get-main-candidate emulting-selected-candidate))
        (prog1 (string-match "^#\\(.*\\):\\(.*\\)" input)
          (setq prefix (ignore-errors
                         (concat "#" (match-string 1 input) ":"))
                content (ignore-errors
                          (match-string 2 input))))

        (if (setq completion
                  (alist-get
                   'complete
                   (symbol-value (if emulting-only-extension
                                     (nth emulting-only-extension
                                          emulting-extension-alist)
                                   (nth emulting-current-extension
                                        (emulting-get-extension-has-result))))))
            (setq input (concat (when content
                                  prefix)
                                (funcall completion (if content
                                                        content
                                                      input)
                                         condidate)))
          (if content
              (progn
                (setq content condidate)
                (setq input (concat prefix content)))
            (setq input condidate)))
        
        (erase-buffer)
        (insert input)))))

(defun emulting-clear-result ()
  "Clear the match results."
  (let (extensions)
    (dolist (result emulting-extension-result)
      (setq extensions (append extensions (list (list (car result))))))
    (setq emulting-extension-result extensions)))

(defun emulting-clear-variable ()
  "Set all the items in `emulting-clear-variables' to nil."
  (mapc (lambda (v)
          (set v nil))
        emulting-clear-variables))

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
    (setq emulting-selected-candidate-data-list nil)
    (let (extension icon)
      (dolist (result (if emulting-only-extension
                          (list (nth emulting-only-extension
                                     emulting-extension-result))
                        (emulting-get-extension-result)))
        (setq extension (symbol-value (car result))
              icon (alist-get 'icon extension))
        (insert (propertize (alist-get 'name extension)
                            'face 'emulting-header-title-face)
                "\n")
        (mapcar (lambda (r)
                  (let ((s (if (consp r)
                               (progn
                                 (add-to-list 'emulting-selected-candidate-data-list
                                              (append r (list (car result)))
                                              t 'equal)
                                 (car r))
                             r)))
                    (ignore-errors
                      (insert (if icon
                                  (funcall icon s)
                                "")))
                    (insert " " s "\n")))
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
          (setq emulting-current-extension 0
                emulting-just-refreshed nil)
          (emulting-select-current-candidate))

      (setq emulting-adjusting-overlay t)

      (if moved
          (let ((line-arg (when (eq moved 'prev)
                            -1)))
            (goto-char (overlay-start emulting-selected-overlay))
            (delete-overlay emulting-selected-overlay)

            (forward-line line-arg)
            (cond ((save-excursion
                     (forward-line)
                     (eobp))
                   (forward-line (unless line-arg
                                   -1))
                   (emulting-select-current-candidate))

                  ((or (= (line-beginning-position) (line-end-position))
                       (emulting-header-title-p))
                   (if (or emulting-only-extension
                           (= (length (emulting-get-extension-has-result)) 1))
                       (progn
                         (if line-arg
                             (forward-line)
                           (forward-line -1))
                         (emulting-select-current-candidate))
                     (if line-arg
                         (emulting-prev-extension)
                       (emulting-next-extension))))

                  
                  (t (emulting-select-current-candidate))))

        (unless (string-empty-p (buffer-string))
          (delete-overlay emulting-selected-overlay)
          (if (or emulting-only-extension
                  (= (length (emulting-get-extension-has-result)) 1))
              (goto-char (point-min))
            (emulting-goto-extension emulting-current-extension))
          (forward-line)
          (let (temp)
            (setq temp
                  (catch 'stop
                    (save-excursion
                      (while (and (not (emulting-header-title-p))
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
              (emulting-select-current-candidate)))))
      (setq emulting-adjusting-overlay nil))))

(defun emulting-match-input ()
  "Match input for extensions."
  (if (not (or (get-buffer emulting-input-buffer)
               (get-buffer emulting-result-buffer)))
      (emulting-exit)
    (let ((input (emulting-get-input)))
      (if emulting-only-extension
          (funcall (alist-get 'filter
                              (symbol-value (nth emulting-only-extension
                                                 emulting-extension-alist)))
                   input)
        (dolist (extension emulting-extension-alist)
          (funcall (alist-get 'filter (symbol-value extension)) input))))))

(defun emulting-async-run (func)
  "Run FUNC asynchronously."
  (run-with-idle-timer
   0.15 nil func)
  (setq emulting-just-refreshed t)
  (print 'test))

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
      (setq emulting-current-extension 0
            emulting-just-refreshed nil))))

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
  (if (stringp candidate)
      (if (string-prefix-p " " candidate)
          (substring candidate 1)
        (substring candidate 2))
    candidate))

(defun emulting-get-input ()
  "Get input text."
  (let ((input (with-current-buffer emulting-input-buffer
                 (buffer-string)))
        prefix content tmp)
    (if (prog1 (string-match "^#\\(.*\\):\\(.*\\)" input)
          (setq prefix (ignore-errors
                         (match-string 1 input))
                content (ignore-errors
                          (match-string 2 input))))
        (progn
          (setq prefix (intern (concat "emulting-extension-var-"
                                       (replace-regexp-in-string
                                        " " "-" (downcase prefix)))))
          (when (setq tmp
                      (spring/get-index prefix emulting-extension-alist))
            (unless (eq tmp emulting-only-extension)
              (setq emulting-just-refreshed nil))
            (setq emulting-only-extension tmp))
          content)
      (when emulting-only-extension
        (setq emulting-only-extension nil
              emulting-just-refreshed nil))
      (when (string-match-p "^#" input)
        (setq emulting-start-prefix t))
      input)))

(defun emulting-extension-buffer-icon (buffer)
  "Icon function for BUFFER."
  (with-current-buffer buffer
    (if (derived-mode-p buffer 'eaf-mode)
        (all-the-icons-faicon "html5" :v-adjust 0.01)
      (all-the-icons-icon-for-buffer))))

(defun emulting-extension-get-data (candidate index &optional from-alist)
  "Get the extension data by its CANDIATE and extension INDEX.
When FROM-ALIST is non-nil, get the extension from alist."
  (let ((extension (nth index (if from-alist
                                  emulting-extension-alist
                                (emulting-get-extension-has-result)))))
    (when extension
      (catch 'stop
        (dolist (data emulting-selected-candidate-data-list)
          (when (and (string= (car data) candidate)
                     (eq (nth 2 data) extension))
            (throw 'stop (nth 1 data))))))))

;;; Functional functions for extension

(defmacro emulting-define-extension (name clear-vars refresh-function icon-function
                                          filter-function execute-function
                                          &optional complete-function)
  "The macro to define emulting extension.
NAME is the head-title which show in the result buffer.
CLEAR-VARS is the variables that needs to be set to nil after process.
REFRESH-FUNCTION is used for refreshing result when there's no matched item.
ICON-FUNCTION is used for providing the icon for the result.
FILTER-FUNCTION is used to filter result.
EXECUTE-FUNCTION is used to handle the result.
COMPLETE-FUNCTION is used to complete the input."
  (declare (indent defun))
  (let* ((async (eq (car filter-function) 'async))
         (extension-symbol-name (replace-regexp-in-string " " "-" (downcase name)))
         (function-name (intern (concat "emulting-extension-"
                                        extension-symbol-name)))
         (variable-name (intern (concat "emulting-extension-var-"
                                        extension-symbol-name))))
    (when (and clear-vars
               (symbolp clear-vars))
      (setq clear-vars (list clear-vars)))

    `(progn
       (defun ,function-name (content)
         (funcall ,filter-function content))

       (defvar ,variable-name)

       (setq ,variable-name
             '((name . ,name)
               (filter . ,function-name)
               (icon . ,icon-function)
               (execute . ,execute-function)
               (complete . ,complete-function)
               (refresh . ,refresh-function)))

       (add-to-list 'emulting-extension-alist ',variable-name t)
       (add-to-list 'emulting-extension-result '(,variable-name) t)
       (when ',clear-vars
         (mapc (lambda (v)
                 (add-to-list 'emulting-clear-variables v t))
               ',clear-vars)))))

(defun emulting-input-match (input content)
  "Check if INPUT is matched with CONTENT."
  (ivy--re-filter (ivy--regex input) content))

(defun emulting-change-candidate (extension candidate)
  "Change EXTENSION's CANDIDATE."
  (let ((index (spring/get-index extension emulting-extension-alist))
        tmp)
    (unless (equal (cdr (nth index emulting-extension-result))
                   candidate)
      (when (and (or (eq emulting-current-extension
                         (spring/get-index extension (emulting-get-extension-has-result)))
                     (eq index emulting-only-extension))
                 (null candidate)
                 (null emulting-just-refreshed)
                 (setq tmp (alist-get 'refresh (symbol-value extension))))
        (emulting-async-run tmp))
      (setf (nth index emulting-extension-result)
            (cons extension candidate))
      (emulting-update-result-buffer))))

(defmacro emulting-filter-append (candidate val)
  "Add VAL to CANDIDATE."
  (declare ((debug t)))
  `(setq ,candidate (append ,candidate (list ,val))))

;;; Extensions
;;; Prefix

(defun emulting-get-all-extension-prefix ()
  "Get all the extension prefix."
  (let (result)
    (dolist (extension emulting-extension-alist)
      (unless (eq extension 'emulting-extension-var-prefix)
        (emulting-filter-append
         result
         (format "#%s:"
                 (replace-regexp-in-string
                  "-" " " (progn
                            (string-match "^emulting-extension-var-\\(.*\\)"
                                          (symbol-name extension))
                            (match-string 1 (symbol-name extension))))))))
    result))

(emulting-define-extension "PREFIX"
  nil nil nil

  (lambda (input)
    (if emulting-start-prefix
        (let ((prefix (emulting-get-all-extension-prefix))
              candidate)
          (setq candidate (emulting-input-match input prefix))
          (unless candidate
            (setq emulting-start-prefix nil))
          (emulting-change-candidate 'emulting-extension-var-prefix candidate))
      (emulting-change-candidate 'emulting-extension-var-prefix nil)))

  nil)

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

(defvar emulting-extension-buffer-kill-mode nil
  "Kill mode for buffer.")

(defun emulting-extension-buffer-not-blacklist-buffer (buf)
  (catch 'failed
    (dolist (backlist-buf emulting-extension-buffer-blacklist)
      (when (string-prefix-p backlist-buf buf)
        (throw 'failed nil)))
    t))

(defun emulting-extension-kill-buffer (&optional buffer)
  "Kill BUFFER."
  (interactive)
  (if buffer
      (kill-buffer buffer)
    (if (string-empty-p (with-current-buffer emulting-result-buffer
                          (buffer-string)))
        (user-error "[Emulting]: There's no candidate.")
      (when (and emulting-current-extension
                 emulting-selected-candidate)
        (kill-buffer (emulting-get-main-candidate emulting-selected-candidate))
        (emulting-exit)))))

(emulting-define-extension "BUFFER"
  emulting-extension-buffer-kill-mode nil

  (lambda (content)
    (emulting-extension-buffer-icon content))

  (lambda (input)
    (let (buffer-name result)
      (dolist (buf (buffer-list))
        (setq buffer-name (buffer-name buf))
        (when (and (emulting-extension-buffer-not-blacklist-buffer buffer-name)
                   (emulting-input-match input (list buffer-name)))
          (emulting-filter-append result buffer-name)))
      (emulting-change-candidate 'emulting-extension-var-buffer result)))

  (lambda (content)
    (let ((killp emulting-extension-buffer-kill-mode))
      (emulting-exit)
      (if killp
          (emulting-extension-kill-buffer content)
        (switch-to-buffer content)))))

;;; Command

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
  nil emulting-extension-command-get-commands nil

  (lambda (input)
    (let (candidates)
      (setq candidates (sniem--nth-utill 0 19
                                         (emulting-input-match input emulting-extension-command)))
      (emulting-change-candidate 'emulting-extension-var-command candidates)))

  (lambda (candidate)
    (emulting-exit)
    (call-interactively (intern candidate))))

;;; Imenu
(defvar emulting-extension-imenu-cached-candidates nil)

(defvar emulting-extension-imenu-cached-buffer nil)

(defun emulting-extension-imenu-candidates (buffer)
  (with-current-buffer buffer
    (prog1
        (if
            (and emulting-extension-imenu-cached-candidates
                 (or
                  (not emulting-extension-imenu-cached-buffer)
                  (equal emulting-extension-imenu-cached-buffer buffer)))
            emulting-extension-imenu-cached-candidates
          (setq emulting-extension-imenu-cached-candidates
                (let ((index (ignore-errors (imenu--make-index-alist t))))
                  (when index
                    (emulting-extension-imenu-build-candidates
                     (delete (assoc "*Rescan*" index) index))))))
      (setq emulting-extension-imenu-cached-buffer buffer))))

(defun emulting-extension-imenu-build-candidates (alist)
  (cl-remove-if
   (lambda (c)
     (or (string-equal (car c) "Types")
         (string-equal (car c) "Variables")))
   (cl-loop for elm in alist
            nconc (cond
                   ((imenu--subalist-p elm)
                    (emulting-extension-imenu-build-candidates
                     (cl-loop for (e . v) in (cdr elm) collect
                              (cons
                               e
                               (if (integerp v) (copy-marker v) v)))))
                   ((listp (cdr elm))
                    (and elm (list elm)))
                   (t
                    (and (cdr elm)
                         (setcdr elm (pcase (cdr elm)
                                       ((and ov (pred overlayp))
                                        (copy-overlay ov))
                                       ((and mk (or (pred markerp)
                                                    (pred integerp)))
                                        (copy-marker mk))))
                         (list elm)))))))

(emulting-define-extension "IMENU"
  nil nil nil

  (lambda (input)
    (let ((imenu-items (emulting-extension-imenu-candidates emulting-last-buffer))
          candidates)
      (dolist (item imenu-items)
        (when (emulting-input-match input (list (car item)))
          (emulting-filter-append candidates (list (car item)
                                                   (marker-position (cdr item))))))
      (emulting-change-candidate 'emulting-extension-var-imenu candidates)))

  (lambda (candidate)
    (emulting-exit)
    (goto-char candidate)))

;;; File
(emulting-define-extension "FILE"
  nil nil

  (lambda (file)
    (all-the-icons-icon-for-file file))

  (lambda (input)
    (let* ((current-directory emulting-last-directory)
           (absolute-path current-directory)
           filepath
           candidates)
      (when (string-match-p "/" input)
        (setq absolute-path (expand-file-name input absolute-path)
              current-directory (file-name-directory absolute-path))
        (if (directory-name-p input)
            (setq input "")
          (setq input (file-name-base absolute-path))))

      (dolist (file (directory-files current-directory nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
        (when (emulting-input-match input (list file))
          (setq filepath (concat current-directory file))
          (emulting-filter-append candidates (list file filepath))))
      (emulting-change-candidate 'emulting-extension-var-file candidates)))

  (lambda (candidate)
    (emulting-exit)
    (find-file candidate))

  (lambda (input candidate)
    (let (prefix content string-list)
      (setq string-list (split-string input "/"))
      (if (= (length string-list) 1)
          (setq input candidate)
        (setf (nth (1- (length string-list)) string-list) candidate)
        (setq input (mapconcat (lambda (s) s) string-list "/")))
      input)))

(emulting-define-extension "BOOKMARK"
  nil nil

  (lambda (candidate)
    (all-the-icons-faicon "bookmark" :v-adjust -0.03))

  (lambda (input)
    (let ((bookmarks (bookmark-all-names))
          candidates)
      (setq candidates (emulting-input-match input bookmarks))
      (emulting-change-candidate 'emulting-extension-var-bookmark candidates)))

  (lambda (candidate)
    (emulting-exit)
    (bookmark-jump candidate)))

(emulting-define-extension "CONFIG"
  nil nil nil

  (lambda (input)
    (unless emulting-whole-start
      (let ((dir '("settings" "languages" "tools" "init" "third-party"))
            (input-list (split-string input "/"))
            tmp candidates)
        (if (and (> (length input-list) 1)
                 (setq tmp (spring/get-index (car input-list) dir)))
            (progn
              (setq dir (nth tmp dir))
              (setq tmp (delete
                         ".."
                         (delete "."
                                 (directory-files
                                  (if (string= dir "third-party")
                                      "~/.emacs.d/third-party/"
                                    (format "~/.emacs.d/etc/%s/" 
                                            dir))))))
              (setq input (nth 1 input-list))
              (dolist (file tmp)
                (when (emulting-input-match input (list file))
                  (emulting-filter-append
                   candidates
                   (list file (format "~/.emacs.d/%s/%s"
                                      (if (string= dir "third-party")
                                          "third-party"
                                        (concat "etc/" dir))
                                      file))))))
          (setq candidates (emulting-input-match input dir)))
        (emulting-change-candidate 'emulting-extension-var-config candidates))))

  (lambda (candidate)
    (let ((candidate-list (split-string candidate "/")))
      (if (= (length candidate-list) 1)
          (if (string= candidate "init")
              (progn
                (find-file "~/.emacs.d/etc/init-config.el")
                (emulting-exit))
            (emulting-complete))
        (emulting-exit)
        (find-file candidate))))

  (lambda (input candidate)
    (let ((input-list (split-string input "/")))
      (if (= (length input-list) 1)
          (setq input (concat candidate "/"))
        (setf (nth 1 input-list) candidate)
        (setq input (mapconcat (lambda (s) s) input-list "/")))
      input)))

;;; Callable

(defvar emulting-extension-callables nil
  "Callable list.")

(defun emulting-extension-get-callables ()
  "Get callables."
  (let (cmds)
    (mapatoms (lambda (a) (when (fboundp a) (push (symbol-name a) cmds))))
    (setq emulting-extension-callables cmds)))

(run-with-idle-timer 60 t #'emulting-extension-get-callables)

(emulting-define-extension "CALLABLE"
  nil emulting-extension-get-callables nil

  (lambda (input)
    (unless emulting-whole-start
      (let (callables candidates)
        (unless emulting-extension-callables
          (emulting-extension-get-callables))
        (setq callables emulting-extension-callables)
        (setq candidates (sniem--nth-utill 0 39 
                                           (emulting-input-match input callables)))
        (emulting-change-candidate 'emulting-extension-var-callable candidates))))

  (lambda (candidate)
    (emulting-exit)
    (funcall helpful-switch-buffer-function
             (helpful--buffer (intern candidate) t))
    (helpful-update)))

;;; variable
(defvar emulting-extension-variables nil)

(defun emulting-extension-get-variables ()
  "Get callables."
  (let (cmds)
    (mapatoms (lambda (a) (when (helpful--variable-p a) (push (symbol-name a) cmds))))
    (setq emulting-extension-variables cmds)))

(run-with-idle-timer 60 t #'emulting-extension-get-variables)

(emulting-define-extension "VARIABLE"
  nil emulting-extension-get-variables nil

  (lambda (input)
    (unless emulting-whole-start
      (let (variables candidates)
        (unless emulting-extension-variables
          (emulting-extension-get-variables))
        (setq variables emulting-extension-variables)
        (setq candidates (sniem--nth-utill 0 39 
                                           (emulting-input-match input variables)))
        (emulting-change-candidate 'emulting-extension-var-variable candidates))))

  (lambda (candidate)
    (emulting-exit)
    (funcall helpful-switch-buffer-function
             (helpful--buffer (intern candidate) nil))
    (helpful-update)))

(emulting-define-extension "DEFINITION"
  nil
  (lambda ()
    (emulting-extension-get-callables)
    (emulting-extension-get-variables))
  nil

  (lambda (input)
    (let (candidates)
      (setq candidates (sniem--nth-utill 0 39
                                         (emulting-input-match
                                          input
                                          (append emulting-extension-callables
                                                  emulting-extension-variables))))
      (emulting-change-candidate 'emulting-extension-var-definition candidates)))

  (lambda (candidate)
    (emulting-exit)
    (setq candidate (intern candidate))
    (cond ((fboundp candidate)
           (find-function candidate))
          ((boundp candidate)
           (find-variable candidate)))))

;;; Global keymap init
(global-set-key (kbd "M-z") #'emulting)
(global-set-key (kbd "C-q c") (lambda () (interactive) (emulting 'config)))
(global-set-key (kbd "C-q C-m b") (lambda () (interactive) (emulting 'bookmark)))
(global-set-key (kbd "C-x b") (lambda () (interactive) (emulting 'buffer)))
(global-set-key (kbd "C-x k") (lambda ()
                                (interactive)
                                (setq emulting-extension-buffer-kill-mode t)
                                (emulting 'buffer)))
(global-set-key (kbd "M-x") (lambda () (interactive) (emulting 'command)))
(global-set-key (kbd "C-h f") (lambda () (interactive) (emulting 'callable)))
(global-set-key (kbd "C-h v") (lambda () (interactive) (emulting 'variable)))
(sniem-leader-set-key
 "." (lambda () (interactive) (emulting 'definition)))

(provide 'emulting)

;;; emulting.el ends here
