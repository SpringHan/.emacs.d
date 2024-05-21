;;;; This file is used for the useful functions
(defun spring/get-index (item seq &optional listp)
  "Get the earliest index of ITEM in SEQ.
Optional argument LISTP means the ITEM is the first element of list."
  (catch 'index
    (when seq
      (dotimes (i (length seq))
        (when (equal item
                     (spring/car-safe (nth i seq) listp))
          (throw 'index i))))))

(defun spring/car-safe (arg &optional carp)
  "Like `car-safe', but return arg if arg is not a list.
When carp is non-nil, return the car if it has."
  (or (and carp (car-safe arg))
      arg))

(defun open-config-file ()
  "Open the init.el file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-vterm (&optional dir)
  "Open the vterm by DIR"
  (interactive "DInput the directory: ")
  (find-file dir)
  (let ((current-buffer-name (buffer-name)))
    (vterm)
    (linum-mode -1)
    (kill-buffer current-buffer-name)))

(defun open-the-dir ()
  "Open some directory by the DIR-NAME."
  (interactive)
  (let ((dir-file (locate-user-emacs-file "dir-path"))
        dir-name path-map path-string tmp)
    (unless (file-exists-p dir-file)
      (make-empty-file dir-file)
      (open-the-dir--init-dirs))
    (setq tmp (open-the-dir--get-path))
    (setq path-map (car tmp))
    (setq path-string (nth 1 tmp))
    (setq dir-name (completing-read "The directory's name:"
                                    (append (list "Add")
                                            path-string)))
    (if (string-equal dir-name "Add")
        (let ((name (read-string "Enter the dir name:"))
              (path (read-file-name "Enter the dir path:"))
              (file-content (with-temp-buffer
                              (insert-file-contents dir-file)
                              (buffer-string))))
          (with-temp-file dir-file
            (insert file-content)
            (insert (format "(%s %s)\n" name path))))
      (find-file (alist-get dir-name path-map nil nil #'string-equal)))))

(defun open-the-dir--get-path ()
  "Get path from dir-files."
  (catch 'stop
    (with-temp-buffer
      (insert-file-contents (locate-user-emacs-file "dir-path"))
      (goto-char (point-min))
      (let ((start t) path-map path-string tmp)
        (when (eq (point-min) (point-max))
          (throw 'stop t))
        (while (not (= (line-beginning-position) (line-end-position)))
          (setq tmp (buffer-substring (line-beginning-position)
                                      (line-end-position)))
          (string-match "^(\\(.*\\) \\(.*\\))$" tmp)
          (add-to-list 'path-map
                       (cons (match-string 1 tmp) (match-string 2 tmp))
                       t)
          (add-to-list 'path-string (match-string 1 tmp) t)
          (forward-line))
        (list path-map path-string)))))

(defun open-the-dir--init-dirs ()
  "Initialize the dir file."
  (with-temp-file (locate-user-emacs-file "dir-path")
    (insert "(emacs ~/.emacs.d)\n(gtd ~/.emacs.d/gtd)\n(var ~/.emacs.d/var)\n(git ~/Github)\n")))

(defun set-alpha ()
  "Set the backgroud alpha by VAR."
  (interactive)
  (let ((var (when (or (null (frame-parameter nil 'alpha-background))
                       (eq (frame-parameter nil 'alpha-background) 100))
               t)))
    (if var
        (set-frame-parameter nil 'alpha-background 70)
      (set-frame-parameter nil 'alpha-background 100))))

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
  (let* ((now-time
          (string-to-number (substring (current-time-string) 11 13)))
         (mounth
          (pcase (substring (current-time-string) 4 7)
            ("Jan" 1) ("Feb" 2) ("Mar" 3) ("Apr" 4) ("May" 5) ("Jun" 6) ("Jul" 7)
            ("Aug" 8) ("Sep" 9) ("Oct" 10) ("Nov" 11) ("Dec" 12)))
         (dark-start (if (and (>= mounth 4) (< mounth 11))
                         19
                       17)))
    (if (and (>= now-time 6) (< now-time dark-start))
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
          (load-the-theme--enable-theme 'storybook)
          (when (string= spring/time-block "night")
            ;; (eaf-browser-set "day")
            (spring/disable-modeline))
          (setq spring/time-block "daytime"))
      (load-the-theme--enable-theme 'nord)
      (when (string= spring/time-block "daytime")
        ;; (eaf-browser-set "night")
        (spring/disable-modeline))
      (setq spring/time-block "night"))
    (spring/disable-modeline)
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
      (kill-buffer buffer))))

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
  (switch-to-buffer "*scratch*")
  (goto-char (point-max)))

(defun spring/erase-contents ()
  "Erase all the contents of specific buffer."
  (interactive)
  (if (string-equal (buffer-name (current-buffer))
                    "*scratch*")
      (with-current-buffer "*scratch*"
           (let ((content  (buffer-string)))
             (unless (string= content initial-scratch-message)
               (erase-buffer)
               (insert initial-scratch-message)
               (message "Erased contents in *scratch* buffer.")
               (end-of-buffer))))
    (with-current-buffer (current-buffer)
      (erase-buffer))))

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
  (let* ((basic-engine "https://google.com/search?q=")
         (engine (pcase (substring content 0 2)
                   ("go" basic-engine)
                   ("bg" "https://cn.bing.com/search?q=")
                   ("bi" "https://search.bilibili.com/all?keyword=")
                   ("gh" "https://github.com/search?q=")
                   ("gt" "https://search.gitee.com/?type=repository&q=")
                   ("ec" "https://emacs-china.org/search?expanded=true&q=")
                   ("zh" "https://www.zhihu.com/search?utm_content=search_history&type=content&q="))))
    (if engine
        (setq content (substring content 2))
      (setq engine basic-engine))
    (eaf-open-browser (concat engine content))))

(defun spring/kill-magit ()
  "Kill the magit buffers."
  (interactive)
  (when (magit-mode-get-buffers)
    (if (eq major-mode 'magit-status-mode)
        (progn
          (magit-mode-bury-buffer 100)
          (delete-window))
      (quit-window)
      (other-window 1))))

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
                                      '("add" "edit" "delete"
                                        "add_license" "edit_license"))))
  (let ((path (format "~/.emacs.d/snippets/%s/" (if (string-suffix-p "license" type)
                                                    "license-mode"
                                                  major-mode)))
        snippet-name)
    (if (string= type "add")
        (setq snippet-name (read-string "Snippet name: "))
      (setq snippet-name (completing-read "Snippet name: "
                                          (delete "."
                                                  (delete ".."
                                                          (directory-files path))))))
    (pcase type
      ((pred (string-prefix-p "add"))
       (find-file (concat path snippet-name)))
      ((pred (string-prefix-p "edit"))
       (find-file (concat path snippet-name)))
      ("delete"
       (delete-file (concat path snippet-name))))))

(defun spring/insert-license ()
  "Insert LICENSE."
  (interactive)
  (let* ((license-dir "~/.emacs.d/snippets/license-mode/")
         (license (completing-read "License: "
                                  (delete "." (delete ".."
                                                      (directory-files license-dir)))
                                  nil t))
         (content (with-temp-buffer
                    (insert-file-contents (concat license-dir license))
                    (buffer-string))))
    (save-excursion
      (insert content))))

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

(defun spring/set-value-at-point (symbol value)
  "Set value for the symbol at point."
  (interactive (list (let ((s (symbol-at-point)))
                       (if s
                           s
                         (intern (read-string "Enter symbol name: "))))
                     (read--expression "Enter the value: ")))
  (set symbol value))

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
                      :inherit 'unspecified)
  (when (timerp spring/calendar-disable-modeline-timer)
    (cancel-timer spring/calendar-disable-modeline-timer)
    (setq spring/calendar-disable-modeline-timer nil)))

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

(defun spring/refresh-packages ()
  "Refresh packages if the packages' info had't been updated yet."
  (let ((file-name (locate-user-emacs-file "refresh-package"))
        (date (substring (current-time-string) 9 11))
        last-refresh)
    (unless (file-exists-p file-name)
      (make-empty-file file-name))
    (setq last-refresh (with-temp-buffer
                         (insert-file-contents file-name)
                         (buffer-string)))
    (when (or (string= "" last-refresh)
              (not (string= last-refresh date)))
      (with-temp-file file-name
        (erase-buffer)
        (insert date))
      (package-refresh-contents)
      (message "[Spring Emacs]: Package refreshed."))))

(defun spring/vue-build ()
  "Build vue project at current directory."
  (interactive)
  (async-shell-command "npm run build"))

(defun spring/byte-compile-directory (directory)
  "Byte-Compiling the DIRECTORY."
  (interactive (list default-directory))
  (let ((dir-files (directory-files directory)))
    (dolist (file dir-files)
      (when (string-match-p "\\(.*\\)\\.el$" file)
        (byte-compile-file file)))))

(defun spring/char-to-string-output (char)
  "Convert the CHAR to string and output it."
  (interactive (list (thing-at-point 'number)))
  (print (char-to-string char)))

(defun spring/eval-expression ()
  "Like `eval-expression'.
But it will paste the content which was marked before eval this function."
  (interactive)
  (let ((marked-content (when (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end)))))
    (eval-expression
     (read--expression "Eval: " marked-content))))

(defun spring/compile-test (packages)
  "Test compile the current package.
PACKAGES is the dependences."
  (interactive "sEnter the packages: ")
  (let ((expression `((add-to-list 'load-path
                                   ,(file-name-directory (buffer-file-name)))
                      (byte-compile-file
                       ,(buffer-file-name))
                      (if (get-buffer "*Compile-Log*")
                          (with-current-buffer "*Compile-Log*"
                            (buffer-string))
                        (print "No warning or error!"))))
        (test-file "/tmp/test.el")
        (current-window (selected-window))
        (output-buffer "*Compile-Log*")
        tmp)
    (when (not (string= packages ""))
      (setq packages (split-string packages " " t))
      (dolist (package packages)
        (setq tmp (file-name-directory (locate-library package)))
        (unless (string-match-p "^/usr/share/emacs/\\(.*\\)/lisp/emacs-lisp/"
                                tmp)
          (setq expression
                (append (list
                         `(add-to-list 'load-path ,tmp))
                        expression)))))

    (setq expression (append '(progn) expression))
    (unless (file-exists-p test-file)
      (make-empty-file test-file))
    (with-temp-file test-file
      (insert (format "%S" expression)))
    (async-shell-command "emacs -q --no-site-file --batch -l /tmp/test.el" output-buffer)

    (delete-other-windows current-window)
    (split-window nil nil t)
    (other-window 1)
    (switch-to-buffer output-buffer)
    (with-current-buffer output-buffer
      (when (eq (sniem-current-mode) 'insert)
        (sniem-change-mode 'normal))
      (goto-char (point-min)))
    (select-window current-window)))

(defun spring/move-to-window (direction)
  "Move to the window by DIRECTION."
  (interactive "cEnter the direction: ")
  (pcase direction
    (117 (windmove-up))
    (101 (windmove-down))
    (110 (windmove-left))
    (105 (windmove-right))))

(defun spring/child-frame ()
  "The function to operate child frame."
  (interactive)
  (pcase (read-char)
    (?t (let ((current-frame (selected-frame))
              frame current-buffer)
          (if (and (frame-parameter current-frame 'fullscreen)
                   (yes-or-no-p "Picture in picture?"))
              (progn
                (toggle-frame-fullscreen)
                (setq frame (make-frame '((minibuffer . nil))))
                (setq current-buffer (current-buffer))
                (with-selected-frame frame
                  (switch-to-buffer current-buffer))
                (with-selected-frame current-frame
                  (spring/open-scratch))
                ;; Add video parameter for main frame.
                ;; To avoid switching the video buffer in main frame to avoid influencing the video buffer.
                (set-frame-parameter current-frame 'video frame)
                (set-frame-parameter frame 'video-parent current-frame))
            (setq frame (make-frame))
            (with-selected-frame frame
              (spring/open-scratch)))))
    (?1 (dolist (frame (remq (selected-frame) (visible-frame-list)))
          (delete-frame frame))
        (unless (frame-parameter nil 'fullscreen)
          (toggle-frame-fullscreen))
        (when (frame-parameter nil 'video)
          (set-frame-parameter nil 'video nil))
        (when (frame-parameter nil 'video-parent)
          (set-frame-parameter nil 'video-parent nil)))
    (?0 (let ((video-parent (frame-parameter nil 'video-parent)))
          (when video-parent
            (set-frame-parameter video-parent 'video nil))
          (delete-frame)))))

(defun spring/kill-space-line-content ()
  "Find the line filled with space and kill its content."
  (interactive)
  (with-current-buffer (current-buffer)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (unless (remq 32 (string-to-list
                          (buffer-substring (line-beginning-position)
                                            (line-end-position))))
          (delete-region (line-beginning-position) (line-end-position)))
        (forward-line))
      (save-buffer))))

(defun spring/reopen-file (file)
  "Reopen current file."
  (interactive (list (buffer-file-name (current-buffer))))
  (kill-current-buffer)
  (find-file file))

(defun spring/enter-with-semicolon ()
  "Enter the new line with semicolon."
  (interactive)
  (save-mark-and-excursion
    (goto-char (line-end-position))
    (insert ";"))
  (call-interactively (key-binding (kbd "RET"))))

(defun spring/enter-with-comma ()
  "Enter the new line with comma."
  (interactive)
  (save-mark-and-excursion
    (goto-char (line-end-position))
    (insert ","))
  (call-interactively (key-binding (kbd "RET"))))

(defun spring/update-diff ()
  "Update diff."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name buffer)
                 (citre-project-root))
        (diff-hl-update)))))

(defun spring/kill-project-buffers (only-current)
  "Kill files belong to the same directory.
When only-current is non-nil, only kill buffers related to current buffer."
  (interactive (list (y-or-n-p "Only current?")))
  (let (roots temp-root)
    (add-to-list 'roots (citre-project-root))
    (if (memq nil roots)
        (progn
          (when only-current
            (keyboard-quit))
          (setq roots nil))
      (kill-current-buffer))

    (dolist (buffer (buffer-list))
      (when (and (or (buffer-file-name buffer)
                     (with-current-buffer buffer
                       (eq major-mode 'dired-mode)))
                 (setq temp-root (with-current-buffer buffer
                                   (citre-project-root))))
        (if only-current
            (and (member temp-root roots)
                 (kill-buffer buffer))
          (add-to-list 'roots temp-root)
          (kill-buffer buffer))))

    (dolist (dir roots)
      (add-to-list 'spring/projects-in-use dir))))

(defun spring/notepad ()
  "Open notepad."
  (interactive)
  (let ((notepad (locate-user-emacs-file "spring-notepad")))
    (find-file notepad)))

(defun spring/toggle-truncate-lines ()
  "Make windows in current tab truncate lines or not."
  (interactive)
  (if (> (length (window-list)) 1)
      (dolist (window (window-list))
        (with-current-buffer (window-buffer window)
          (toggle-truncate-lines t)))
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          (toggle-truncate-lines -1))))))

(defun spring/set-st-working-dir ()
  "Set current working directory for st."
  (interactive)
  (let ((target-file "~/.cache/st-working-directory"))
    (unless (file-exists-p target-file)
      (make-empty-file target-file))
    (with-temp-file target-file
      (insert (file-truename default-directory)))))

;;; Native Compilation

(defun spring/native-compile-or-load (file &optional o3 force)
  "If FILE's eln file is exists, load it.
Otherwise compile it natively.
When O3 is non-nil, use it as compile speed.
When FROCE is non-nil, compiling FILE forcely."
  (let (eln-file real-file-name)
    (unless (string-suffix-p ".el" file)
      (setq real-file-name (concat file ".el")))
    (when (file-exists-p (or real-file-name file))
      (setq eln-file (comp-el-to-eln-filename (if real-file-name
                                                  real-file-name
                                                file)))
      (if (and (file-exists-p eln-file)
               (null force))
          (native-elisp-load eln-file)
        (let ((native-comp-speed (if o3
                                     3
                                   2)))
          (native-compile-async
           (if real-file-name (file-name-directory file) file)
           5 t))))))

(defun spring/extra-add-to-list (item &optional o3p)
  "If `spring/extra-items-compiled' is nil, add ITEM to
`spring/extra-native-compile-items'.
Otherwise compile item natively.
When O3P is non-nil, use o3 as its compile speed."
  (if spring/extra-items-compiled
      (spring/native-compile-or-load item o3p)
    (add-to-list 'spring/extra-native-compile-items item)))

;;; Advice
(advice-add 'set-window-buffer :around
            (lambda (orig window buffer-or-name &optional keep-margins)
              (let ((video-frame (frame-parameter nil 'video))
                    video-buffer)
                (when video-frame
                  (with-selected-frame video-frame
                    (setq video-buffer (current-buffer)))
                  (when (and (eq video-buffer (get-buffer buffer-or-name))
                             (with-current-buffer video-buffer
                               (eq major-mode 'eaf-mode)))
                    (setq buffer-or-name "*scratch*")))
                (apply orig window buffer-or-name keep-margins))))

(provide 'init-functions)
