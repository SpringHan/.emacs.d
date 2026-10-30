;;; Dsh emacs  -*- lexical-binding: t; -*-

;;; Deepseek Harness
(use-package dsh-emacs
  :init
  (git-download-ensure "dsh-emacs" "SpringHan/dsh-emacs")
  :load-path "~/.emacs.d/third-party/dsh-emacs"
  :bind (("C-q d" . dsh-emacs)
         ("C-q C-s" . dsh-emacs-server-start)
         ("C-q C-q" . dsh-emacs-server-stop)
         ("C-q C-h" . dsh-emacs-plugin-routes)
         ("C-q C-i" . dsh-emacs-plugin-insert-route-call)
         ("C-q C-w" . spring/dsh-emacs-get-cost)
         ("C-q C-d" . spring/dsh-archived-sessions))
  :config
  (setq dsh-emacs-server-auto-start nil))

(defun spring/dsh-emacs-get-cost ()
  (interactive)
  (let ((result (dsh-emacs-plugin-routes "dsh-whale" "GET" "/dsh-whale/balance.json")))
    (if (not (alist-get 'ok result))
        (user-error "[Dsh]: Failed to get current cost!")
      (message "[Dsh]: %.2f, today cost: %.2f"
               (alist-get 'totalBalance result)
               (alist-get 'todayUsage result)))))


;;; Archived session deletion through dsh-session-manager

(require 'cl-lib)
(require 'tabulated-list)

(defvar spring/dsh-archived-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "d") #'spring/dsh-archived-sessions-mark-delete)
    (define-key map (kbd "u") #'spring/dsh-archived-sessions-unmark)
    (define-key map (kbd "x") #'spring/dsh-archived-sessions-execute)
    (define-key map (kbd "q") #'kill-buffer-and-window)
    map)
  "Keymap for `spring/dsh-archived-sessions-mode'.")

(defvar-local spring/dsh-archived-sessions--rows nil
  "Archived session rows shown by `spring/dsh-archived-sessions-mode'.")

(defvar-local spring/dsh-archived-sessions--marked nil
  "Session ids marked for deletion in the archived-session buffer.")

(defun spring/dsh-archived-sessions--updated (session)
  "Return SESSION's last-updated time as a display string."
  (let ((milliseconds (dsh-protocol-session-updated-at session)))
    (if (and (integerp milliseconds) (> milliseconds 0))
        (format-time-string "%Y-%m-%d %H:%M"
                            (seconds-to-time (/ milliseconds 1000.0)))
      "")))

(defun spring/dsh-archived-sessions--sessions ()
  "Return the current `session/list' rows."
  (let ((response (dsh-emacs--rpc-request
                   "session/list" (dsh-protocol-session-list-request))))
    (unless (car response)
      (user-error "Could not read dsh sessions: %s" (cdr response)))
    (dsh-protocol-session-list--from-alist (cdr response))))

(defun spring/dsh-archived-sessions--archived-set ()
  "Return the host's archived session-id set, refreshing it.
The archive set has no unary RPC: it arrives in the `workspace/follow'
baseline.  Reuse the live session-list connection when present, or open a
hidden one for the baseline and tear it down again."
  (let* ((existing (get-buffer dsh-emacs-sessions-buffer))
         (buffer (or existing (get-buffer-create dsh-emacs-sessions-buffer)))
         (created (null existing))
         (previous dsh-emacs--archived-sessions)
         (deadline (+ (float-time) 10)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (unless (derived-mode-p 'dsh-emacs-session-mode)
              (dsh-emacs-session-mode))
            (dsh-emacs-events-host-connect)
            (when (and (bound-and-true-p dsh-emacs--host-ready)
                       (fboundp 'dsh-emacs-events--core-workspace-rebaseline))
              (when (dsh-emacs-events--core-workspace-rebaseline)
                (setq dsh-emacs--archived-sessions nil))))
          (while (and (null dsh-emacs--archived-sessions)
                      (< (float-time) deadline))
            (accept-process-output nil 0.1))
          (or dsh-emacs--archived-sessions
              (if previous
                  (progn
                    (setq dsh-emacs--archived-sessions previous)
                    (message "Using cached archived sessions; refresh timed out")
                    previous)
                (user-error "Timed out while reading archived sessions from dsh"))))
      (when (and created (buffer-live-p buffer))
        (kill-buffer buffer)))))

(defun spring/dsh-archived-sessions--trash-ids ()
  "Return the session ids currently in dsh-session-manager's trash."
  (let* ((value (dsh-emacs-plugin-routes
                 "dsh-session-manager" "GET" "/dsh-session-manager/trash"))
         (entries (alist-get 'entries value)))
    (unless (eq t (alist-get 'ok value))
      (user-error "Could not read the dsh-session-manager trash"))
    (mapcar (lambda (entry) (alist-get 'sessionId entry))
            (append entries nil))))

(defun spring/dsh-archived-sessions--rows ()
  "Return the archived session rows that are not already in the trash."
  (dsh-emacs-server-ensure)
  (let ((sessions (spring/dsh-archived-sessions--sessions))
        (archived (spring/dsh-archived-sessions--archived-set))
        (trash (spring/dsh-archived-sessions--trash-ids)))
    (sort
     (cl-loop
      for session in sessions
      for id = (dsh-protocol-session-session-id session)
      when (and id
                (gethash id archived)
                (not (member id trash)))
      collect (list :id id
                    :title (or (dsh-emacs-session--display-title session) id)
                    :cwd (or (dsh-protocol-session-cwd session) "")
                    :updated (spring/dsh-archived-sessions--updated session)))
     (lambda (left right)
       (string-lessp (plist-get left :title) (plist-get right :title))))))

(defun spring/dsh-archived-sessions--entries ()
  "Return `tabulated-list' entries for the current archived rows."
  (mapcar
   (lambda (row)
     (let ((id (plist-get row :id)))
       (list id
             (vector (if (member id spring/dsh-archived-sessions--marked)
                         "D" " ")
                     (plist-get row :title)
                     id
                     (plist-get row :cwd)
                     (plist-get row :updated)))))
   spring/dsh-archived-sessions--rows))

(defun spring/dsh-archived-sessions--refresh ()
  "Reload archived rows and drop marks for sessions no longer listed."
  (setq spring/dsh-archived-sessions--rows
        (spring/dsh-archived-sessions--rows))
  (setq spring/dsh-archived-sessions--marked
        (cl-remove-if-not
         (lambda (id)
           (cl-find id spring/dsh-archived-sessions--rows
                    :key (lambda (row) (plist-get row :id))
                    :test #'equal))
         spring/dsh-archived-sessions--marked)))

(define-derived-mode spring/dsh-archived-sessions-mode tabulated-list-mode
  "DSH Archived Sessions"
  "Major mode for deleting archived dsh sessions.
\\<spring/dsh-archived-sessions-mode-map>
\\[spring/dsh-archived-sessions-mark-delete] toggles the deletion mark on
the session at point, \\[spring/dsh-archived-sessions-unmark] clears it,
and \\[spring/dsh-archived-sessions-execute] deletes every marked session
through dsh-session-manager."
  (setq tabulated-list-format
        [("D" 1 t)
         ("Session" 35 t)
         ("ID" 24 t)
         ("Directory" 30 t)
         ("Updated" 16 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'spring/dsh-archived-sessions--entries)
  (add-hook 'tabulated-list-revert-hook
            #'spring/dsh-archived-sessions--refresh nil t)
  (tabulated-list-init-header))

(defun spring/dsh-archived-sessions-mark-delete ()
  "Toggle the deletion mark on the archived session at point."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (was-marked (member id spring/dsh-archived-sessions--marked)))
    (unless id
      (user-error "No session on this line"))
    (if was-marked
        (setq spring/dsh-archived-sessions--marked
              (delete id spring/dsh-archived-sessions--marked))
      (push id spring/dsh-archived-sessions--marked))
    (tabulated-list-set-col 0 (if was-marked " " "D") t)
    (forward-line 1)))

(defun spring/dsh-archived-sessions-unmark ()
  "Remove the deletion mark from the archived session at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No session on this line"))
    (setq spring/dsh-archived-sessions--marked
          (delete id spring/dsh-archived-sessions--marked))
    (tabulated-list-set-col 0 " " t)
    (forward-line 1)))

(defun spring/dsh-archived-sessions-execute ()
  "Delete every session marked with \\`d'.
Each deletion posts the session id to dsh-session-manager's `/delete'
route through `dsh-emacs-plugin-routes'."
  (interactive)
  (let ((ids spring/dsh-archived-sessions--marked))
    (unless ids
      (user-error "No sessions marked for deletion"))
    (unless (yes-or-no-p
             (format "Delete %d archived session%s? "
                     (length ids) (if (= (length ids) 1) "" "s")))
      (user-error "Cancelled"))
    (let ((deleted 0)
          (failed nil))
      (dolist (id ids)
        (condition-case err
            (progn
              (dsh-emacs-plugin-routes
               "dsh-session-manager" "POST" "/dsh-session-manager/delete"
               (dsh-protocol-write-json (list (cons 'sessionId id))))
              (setq deleted (1+ deleted))
              (setq spring/dsh-archived-sessions--rows
                    (cl-remove-if
                     (lambda (row) (equal id (plist-get row :id)))
                     spring/dsh-archived-sessions--rows)))
          (user-error
           (push (cons id (error-message-string err)) failed))))
      (setq spring/dsh-archived-sessions--marked (mapcar #'car failed))
      (tabulated-list-print t)
      (if failed
          (user-error "Deleted %d; failed %d: %s"
                      deleted (length failed)
                      (mapconcat (lambda (entry)
                                   (format "%s (%s)" (car entry) (cdr entry)))
                                 (nreverse failed) "; "))
        (message "Deleted %d archived session%s"
                 deleted (if (= deleted 1) "" "s"))))))

(defun spring/dsh-archived-sessions ()
  "List archived dsh sessions and delete marked ones.
Sessions are marked with \\`d' and deleted together with \\`x' through
dsh-session-manager.  The list excludes sessions already in the plugin's
trash."
  (interactive)
  (let ((buffer (get-buffer-create "*dsh-archived-sessions*")))
    (with-current-buffer buffer
      (spring/dsh-archived-sessions-mode)
      (spring/dsh-archived-sessions--refresh)
      (tabulated-list-print)
      (goto-char (point-min))
      (while (and (not (eobp)) (null (tabulated-list-get-id)))
        (forward-line 1)))
    (pop-to-buffer buffer)
    (unless spring/dsh-archived-sessions--rows
      (message "No archived dsh sessions"))))


(provide 'init-dsh)
