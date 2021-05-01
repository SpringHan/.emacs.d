;;;; This file is used for the useful functions
(defun spring/get-index (item seq)
  "Get the earliest index of ITEM in SEQ."
  (let ((index nil)
        (indexf -1))
    (dolist (ele seq)
      (if (equal item ele)
          (setq index indexf)
        (setq indexf (+ 1 indexf))))
    index))

(defun open-config-file ()
  "Open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-etc-config ()
  "Open the config file in the etc directory."
  (interactive)
  (let (path filename)
    (while (or (null filename)
               (string= filename ".."))
      (setq path (pcase (completing-read
                         "Enter the index of config: "
                         '("settings" "languages" "tools" "init" "third-party"))
                   ("languages" "~/.emacs.d/etc/languages/")
                   ("settings" "~/.emacs.d/etc/settings/")
                   ("tools" "~/.emacs.d/etc/tools/")
                   ("init" "~/.emacs.d/etc/init-config.el")
                   ("third-party" "~/.emacs.d/third-party/")))
      (if (not (string= path "~/.emacs.d/etc/init-config.el"))
          (setq filename (completing-read "Enter the filename: "
                                          (delete "." (directory-files path))))
        (setq filename "")))
    (find-file (concat path filename))))

(defun open-vterm (&optional dir)
  "Open the vterm by DIR"
  (interactive "DInput the directory: ")
  (find-file dir)
  (let ((current-buffer-name (buffer-name)))
    (vterm)
    (linum-mode -1)
    (kill-buffer current-buffer-name)))

(defun open-the-dir (dir-name)
  "Open some directory by the DIR-NAME."
  (interactive (list
                (completing-read "The directory's name: "
                                 '("emacs" "git" "gtd" "C" "python" "go"
                                   "clojure" "blog"))))
  (find-file (pcase dir-name
               ("gtd" "~/.emacs.d/gtd")
               ("git" "~/Github")
               ("emacs" "~/.emacs.d")
               ("C" "~/Code/C/src/Study")
               ("python" "~/Code/python")
               ("go" "~/go")
               ("clojure" "~/Code/clojure")
               ("blog" "~/Github/Blog"))))

(defun set-alpha ()
  "Set the backgroud alpha by VAR."
  (interactive)
  (let ((var (when (= (car (frame-parameter nil 'alpha)) 100)
               t)))
    (if var
        (set-frame-parameter nil 'alpha '(75 . 100))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun window-move (way)
  "Move the buffer window position by WAY."
  (interactive "cEnter the way(n-e-u-i): ")
  (let ((current-window-buffer (window-buffer))
        (current-window (get-buffer-window)))
    (pcase way
      (110 (windmove-left))
      (101 (windmove-down))
      (117 (windmove-up))
      (105 (windmove-right)))
    (setq another-window-buffer (get-buffer-window))
    (if (not (eql current-window-buffer another-window-buffer))
        (progn
          (set-window-buffer current-window (window-buffer))
          (set-window-buffer (get-buffer-window) current-window-buffer))))) ; Move the window

(defun sudo-save ()
  "Save the current buffer file with sudo."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun write-scratch ()
  "Open the write scratch buffer."
  (interactive)
  (switch-to-buffer "*Write-Scratch*")
  (markdown-mode))

(defun markdown-table-keymap ()
  "Add table map in markdown mode."
  (define-key markdown-mode-map (kbd "C-c C-c TAB") 'markdown-table-align))

(defun day-or-night ()
  "Return t/nil.
If it's daytime now,return t.Otherwise return nil."
  (let ((now-time
         (string-to-number (cl-subseq (current-time-string) 11 13))))
    (if (and (>= now-time 6) (< now-time 18))
        t
      nil)))

(defun load-the-theme (&optional time)
  "Load the theme by time."
  (interactive (list (completing-read "Enter the theme type: "
                                      '("day" "night"))))
  (let (time-result)
    (cond (time
           (setq time-result
                 (pcase time
                   ("day" t)
                   ("night" nil))))
          ((null time)
           (setq time-result (day-or-night))))
    (if time-result
        (progn
          (load-the-theme--enable-theme 'lab-light)
          (when (string= spring/time-block "night")
            (eaf-browser-set "day")
            (spring/disable-modeline))
          (setq spring/time-block "daytime"))
      (load-the-theme--enable-theme 'nord)
      (when (string= spring/time-block "daytime")
        (eaf-browser-set "night")
        (spring/disable-modeline))
      (setq spring/time-block "night"))
    (when time
      (spring/disable-modeline))
    (posframe-delete-all)))

(defun load-the-theme--enable-theme (current-theme)
  "Delete all the other themes."
  (unless (memq current-theme custom-enabled-themes)
    (load-theme current-theme t))
  (unless (= 1 (length (memq current-theme custom-enabled-themes)))
    (dolist (theme custom-enabled-themes)
      (unless (eq theme current-theme)
        (disable-theme theme)))))

(defun kill-unwanted-buffer ()
  "Kill the unwanted buffers."
  (interactive)
  (dolist (buffer spring/unwanted-buffer)
    (when (get-buffer buffer)
      (kill-buffer buffer)))
  (let (buffer-name)
    (dolist (buffer (buffer-list))
      (setq buffer-name (buffer-name buffer))
      (when (or (string-match-p "^*\\(.*\\):stderr*" buffer-name)
                (string-match-p "^*Flycheck\\(.*\\)*" buffer-name)
                (string-match-p "^*\\(.*\\)doc\\(.*\\)" buffer-name)
                (string-match-p "^*helpful\\(.*\\)*" buffer-name))
        (kill-buffer buffer)))))

(defun tab-bar-new-with-buffer (buffer-name)
  "Create a new tab then select a buffer."
  (interactive "bBuffer Name: ")
  (tab-bar-new-tab)
  (switch-to-buffer buffer-name))

(defun spring/tab-bar-new-scratch ()
  "Create a new tab then select the *Scratch* buffer."
  (interactive)
  (tab-bar-new-tab)
  (switch-to-buffer "*scratch*"))

(defun spring/tab-bar-close-tab-kill-buffer ()
  "Kill the current buffer and close the current tab."
  (interactive)
  (kill-buffer)
  (tab-bar-close-tab))

(defun spring/open-scratch ()
  "Open the scratch buffer after closing it."
  (interactive)
  (if (get-buffer "*scratch*")
      (switch-to-buffer "*scratch*")
    (switch-to-buffer "*scratch*")
    (insert initial-scratch-message)
    (message "Open the scratch action done.")))

(defun spring/scratch-erase-contents ()
  "Erase all the contents of *scratch* buffer."
  (interactive)
  (with-current-buffer "*scratch*"
    (let ((content  (buffer-string)))
      (unless (string= content initial-scratch-message)
        (erase-buffer)
        (insert initial-scratch-message)
        (message "Erased contents in *scratch* buffer.")
        (end-of-buffer)))))

(defun spring/use-space-indent ()
  "Use the space indent in org-mode."
  (interactive)
  (setq indent-tabs-mode nil))

(defun spring/touch-not-alpha ()
  "Make the not-alpha file."
  (interactive)
  (let ((file-name
         (expand-file-name (locate-user-emacs-file "not-alpha"))))
    (unless (file-exists-p file-name)
      (make-empty-file file-name))))

(defun spring/open-erc ()
  "Open the erc with only one time."
  (interactive)
  (let ((erc-file-path
         (expand-file-name (locate-user-emacs-file
                            "erc-userinfo"))))
    (if (file-exists-p erc-file-path)
        (let ((user-info
               (with-temp-buffer (insert-file-contents
                                  erc-file-path)
                                 (split-string (buffer-string)
                                               "\n" t))))
          (erc :nick (car user-info) :password (nth 1 user-info)))
      (let ((user-name (read-string "ERC Nick: "))
            (user-password (read-passwd "ERC Password: "))
            save-y-or-n)
        (if (or (string= user-name "")
                (string= user-password ""))
            (error "The user name or password can't be null!")
          (setq save-y-or-n (read-char "Do you want to save your account?(y/n)"))
          (when (= save-y-or-n 121)
            (with-temp-file erc-file-path
              (insert (format "%s\n" user-name))
              (insert (format "%s" user-password))))
          (erc :nick user-name :password user-password))))))

(defun spring/downcase-word-first-letter ()
  "Downcase the first letter in the word at point."
  (interactive)
  (let ((letter (cl-subseq (thing-at-point 'word t) 0 1)))
    (delete-char 1)
    (insert (downcase letter))))

(defun spring/set-volume (mode &optional changes)
  "Change the volume."
  (interactive (list (completing-read "Enter the set mode: "
                                      '("set" "up" "down"))))
  (let (volume)
    (pcase mode
      ("set"
       (setq volume (format "%s%%"
                            (read-string "Enter the volume you want: "))))
      ("up"
       (setq volume (format "+%s%%"
                            (read-string "Enter the volume you want to add: "))))
      ("down"
       (setq volume (format "-%s%%"
                            (read-string "Enter the volume you want to reduce: "))))
      ("ups"
       (setq volume (format "+%d%%" changes)))
      ("downs"
       (setq volume (format "-%d%%" changes)))
      ("0"
       (setq volume "0%")))
    ;; TODO: Need new function to get the audio card
    (shell-command (concat "pactl set-sink-volume alsa_output.pci-0000_00_1b.0.analog-stereo " volume) "*Volume Set*")
    (when (get-buffer "*Volume Set*")
      (kill-buffer "*Volume Set*"))
    (message "[Spring Emacs]: The current volume: %s" (spring/get-volume))))

(defun spring/up-5-volume (&optional times)
  "Up 5 volume."
  (interactive "P")
  (if times
      (spring/set-volume "ups" (* 5 times))
    (spring/set-volume "ups" 5)))

(defun spring/down-5-volume (&optional times)
  "Down 5 volume."
  (interactive "P")
  (if times
      (spring/set-volume "downs" (* 5 times))
    (spring/set-volume "downs" 5)))

(defun spring/no-volume ()
  "Set the volume to 0."
  (interactive)
  (spring/set-volume "0"))

(defun spring/get-volume ()
  "Get the volume and return it."
  (let (volume)
    (shell-command "amixer get Master | tail -n1 | sed -r \"s/.*\\[(.*)%\\].*/\\1/\""
                   "*Volume Value*")
    (when (get-buffer "*Volume Value*")
      (with-current-buffer "*Volume Value*"
        (setq volume (car (split-string (buffer-string) "\n" t))))
      (kill-buffer "*Volume Value*"))
    volume))

(defun spring/show-volume ()
  "Show the volume."
  (interactive)
  (message "[Spring Emacs]: Current Volume is: %s" (spring/get-volume)))

(defun spring/show-packages-required ()
  "Show all the packages required."
  (interactive)
  (message "[Spring Emacs]: Now Emacs has required %d packages."
           (length package-activated-list)))

(defun spring/search (content)
  "Open search page."
  (interactive "MEnter the search content: ")
  (let ((engine (pcase (substring content 0 2)
                  ("bg" "https://cn.bing.com/search?q=")
                  ("bi" "https://search.bilibili.com/all?keyword=")
                  ("gh" "https://github.com/search?q=")
                  ("gt" "https://search.gitee.com/?type=repository&q="))))
    (setq content (substring content 2))
    (eaf-open-browser (concat engine content))))

(defun spring/kill-magit ()
  "Kill the magit buffers."
  (interactive)
  (magit-mode-bury-buffer)
  (unless (null (magit-mode-get-buffers))
    (dolist (buffer (magit-mode-get-buffers))
      (kill-buffer buffer))))

(defun spring/kill-all-else-buffers (&optional type)
  "Kill the buffers without *scratch*, *Message* and *eaf*."
  (interactive "P")
  (let ((wanted-buffer '("*scratch*" "*Messages*" "*eaf*")))
    (dolist (buffer (buffer-list))
      (unless (or (spring/get-index (buffer-name buffer) wanted-buffer)
                  (string= (cl-subseq (buffer-name buffer) 0 1) " "))
        (if (and type (equal buffer (current-buffer)))
            nil
          (kill-buffer buffer))))))

(defun spring/shell-clear ()
  "Clear the shell buffer."
  (interactive)
  (with-current-buffer "*shell*"
    (erase-buffer)
    (comint-send-input)
    (beginning-of-buffer)
    (kill-line 2)
    (end-of-buffer)))

(defun spring/open-shell (&optional type)
  "Open the shell."
  (interactive "P")
  (shell)
  (when type
    (with-current-buffer "*shell*"
      (delete-other-windows))))

(defun spring/edit-snippets (type)
  "Edit the snippets in current mode."
  (interactive (list (completing-read "Enter the edit type: "
                                      '("add" "edit" "delete"))))
  (let ((path (format "~/.emacs.d/snippets/%S/" major-mode))
        snippet-name)
    (if (string= type "add")
        (setq snippet-name (read-string "Snippet name: "))
      (setq snippet-name (completing-read "Snippet name: "
                                          (delete "."
                                                  (delete ".."
                                                          (directory-files path))))))
    (pcase type
      ("add"
       (find-file (concat path snippet-name)))
      ("edit"
       (find-file (concat path snippet-name)))
      ("delete"
       (delete-file (concat path snippet-name))))))

(defun spring/change-indent-type (type)
  "Change the indent type."
  (interactive (list (completing-read "Enter the indent type: "
                                      '("tab" "space"))))
  (pcase type
    ("tab" (setq-local indent-tabs-mode t))
    ("space" (setq-local indent-tabs-mode nil))))

(defun spring/show-current-url ()
  "The function to show the url at current buffer."
  (interactive)
  (message eaf--buffer-url)
  eaf--buffer-url)

(defun spring/copy-current-url ()
  "Copy the current buffer's url."
  (interactive)
  (let ((url (spring/show-current-url)))
    (with-temp-buffer
      (insert url)
      (kill-ring-save (line-beginning-position) (line-end-position)))))

(defun spring/terlat-translate (&optional type)
  "The function to translate the CONTENT by terlat."
  (interactive)
  (let ((content (read-string "Enter the content you want to translate: "))
        result)
    (shell-command (concat "terlat " "\"" content "\"") " *Spring-Translate*")
    (setq result (with-current-buffer " *Spring-Translate*"
                   (buffer-string)))
    (if type
        (insert (car-safe (split-string result "\n" t)))
      (message (concat "[Spring-Translate:Result]: " result)))
    (kill-buffer " *Spring-Translate*")))

(defun spring/terlat-translate-insert ()
  "The function to insert the translate result."
  (interactive)
  (spring/terlat-translate t))

(defun spring/test-color ()
  "Test color by input color."
  (interactive)
  (let (color)
    (while (not (string=
                 (setq color (read-string "Enter the color: "))
                 ""))
      (message (concat color ": "
                       (propertize "TEST-COLOR"
                                   'face `((t :foreground ,color)))))
      (read-char))))

(defun spring/input-char-number (&optional times)
  "Input the char number."
  (interactive "P")
  (let (char)
    (unless times
      (setq times 1))
    (dotimes (n times)
      (setq char (read-char))
      (insert (number-to-string char))
      (unless (= times 1)
        (insert " ")))))

(defun spring/format-commit ()
  "Git commit with formatted text."
  (interactive)
  (let* ((type (completing-read "Enter the commit type: "
                                '("docs" "file" "modifiy" "feat" "style" "fix")))
         (files (read-string (format
                              "Enter the files with %s(use space to split): "
                              type)))
         content)
    ;; Commit
    (setq content (read-string (format
                                "Enter the short content with %s(%s): "
                                type files)))
    (setq content (concat "\"" type "(" files "): " content "\""))
    (when (= 121 (read-char (format "Content: %s(y/n)" content)))
      (shell-command (concat "git commit -m " content) " *Format-Commit*")
      (kill-buffer " *Format-Commit*")))
  (when (eq major-mode 'magit-status-mode)
    (magit-refresh)))

(defun spring/set-value-at-point (symbol value)
  "Set value for the symbol at point."
  (interactive (list (let ((s (symbol-at-point)))
                       (if s
                           s
                         (intern (read-string "Enter symbol name: "))))
                     (read--expression "Enter the value: ")))
  (set symbol value))

(defun spring/print-vars-value (var)
  "Print the var's value."
  (interactive (let ((variable (thing-at-point 'symbol)))
                 (list (read-string "Enter the variable's name: "
                               variable))))
  (print (symbol-value (intern var))))

(defun spring/change-input-method (method)
  "Change input METHOD."
  (interactive)
  (if (symbolp method)
      (set-input-method method)
    (toggle-input-method))
  (when (get-buffer "*Quail Completions*")
    (kill-buffer "*Quail Completions*")))

(defun spring/initialize-input-method ()
  "Initialize input method."
  (interactive)
  (set-input-method 'pyim)
  (spring/change-input-method 0))

(defun spring/use-colemak-keyboard ()
  "Run shell to use colemak keyboard."
  (interactive)
  (shell-command "setxkbmap us colemak -option -option ctrl:nocaps" "*Colemak*")
  (when (get-buffer "*Colemak*")
    (kill-buffer "*Colemak*")))

(defun spring/disable-modeline ()
  "The function to disable the modeline."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground awesome-tray-mode-line-active-color
                      :background awesome-tray-mode-line-active-color
                      :height 0.1
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground awesome-tray-mode-line-inactive-color
                      :background awesome-tray-mode-line-inactive-color
                      :height 0.1
                      :box nil
                      :inherit 'unspecified))

(defun spring/enable-modeline ()
  "Enable mode-line."
  (interactive)
  (set-face-attribute 'mode-line nil
                      :foreground (nth 0 awesome-tray-mode-line-colors)
                      :background (nth 1 awesome-tray-mode-line-colors)
                      :family (nth 2 awesome-tray-mode-line-colors)
                      :box (nth 3 awesome-tray-mode-line-colors)
                      :height awesome-tray-mode-line-default-height)
  (set-face-attribute 'mode-line-inactive nil
                      :foreground (nth 4 awesome-tray-mode-line-colors)
                      :background (nth 5 awesome-tray-mode-line-colors)
                      :family (nth 6 awesome-tray-mode-line-colors)
                      :box (nth 7 awesome-tray-mode-line-colors)
                      :height awesome-tray-mode-line-default-height))

(defun spring/copy-directory-path ()
  "Copy the current directory path."
  (interactive)
  (let ((dir default-directory))
    (with-temp-buffer
      (insert dir)
      (kill-ring-save (point-min) (point-max)))))

(defun spring/todo-undo-p ()
  "If there're todos have not been donw."
  (interactive)
  (when org-agenda-files
    (let ((files (delete "." (delete ".." (directory-files (car-safe org-agenda-files)))))
          (undo-regexp "\\* \\(TODO\\|STUDY\\|WAIT\\|BUG\\|KNOWN\\)")
          undo)
      (catch 'stop
        (dolist (file files)
          (if undo
              (throw 'stop t)
            (with-temp-buffer
              (insert-file-contents (concat (car-safe org-agenda-files)
                                            "/"
                                            file))
              (goto-char (point-min))
              (when (let ((case-fold-search nil))
                      (re-search-forward undo-regexp nil t))
                (setq undo t))))))
      (when undo
        (with-current-buffer "*scratch*"
          (insert ";; [Spring Emacs]: There're todos haven't been done.\n\n"))))))

(defun spring/insert-result (command)
  "Insert COMMAND's result."
  (interactive (list (read--expression "Eval: ")))
  (insert (format "%S" (eval command))))

(defun spring/find-function (fn)
  "A new `find-function'."
  (interactive (list (let ((demo (ignore-errors
                                   (intern (substring-no-properties
                                            (thing-at-point 'symbol))))))
                       (if (functionp demo)
                           demo
                         nil))))
  (find-function (if fn
                     fn
                   (car (find-function-read)))))

(provide 'init-functions)
