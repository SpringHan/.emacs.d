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

(defun open-etc-config (index)
	"Open the config file in the etc directory."
	(interactive (list (completing-read "Enter the index of config: "
																			'("languages" "settings"))))
	(let* ((path (pcase index
								 ("languages" "~/.emacs.d/etc/languages/")
								 ("settings" "~/.emacs.d/etc/settings/")))
				 (filename
					(completing-read "Enter the filename: "
													 (delete ".." (delete "." (directory-files path))))))
		(find-file (concat path filename))))

(defun open-etc-config-by-char (char)
	"Call the open-etc-config with its index."
	(interactive "cEnter the char: ")
	(open-etc-config (pcase char
										 (115 "settings")
										 (108 "languages"))))

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
																 '("emacs" "git" "gtd" "C"))))
	(pcase dir-name
		("gtd" (find-file "~/.emacs.d/gtd"))
		("git" (find-file "~/Github"))
		("emacs" (find-file "~/.emacs.d"))
		("C" (find-file "~/Code/C/src/Study"))))

(defun set-alpha (var)
	"Set the backgroud alpha by VAR."
	(interactive "sAlpha or not(y-or-n): ")
	(pcase var
		("y" (set-frame-parameter nil 'alpha '(90 . 100)))
		("n" (set-frame-parameter nil 'alpha '(100 . 100)))))

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

(defun load-the-theme ()
	"Load the theme by time."
	(interactive)
	(if (day-or-night)
			(progn
				(package-require
				 'atom-one-light
				 :outside
				 :before-load-eval '(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
				 :load-theme 'atom-one-light)
				(when (string= spring/time-block "night")
					(eaf-browser-set))
				(setq spring/time-block "daytime"))
		(package-require
		 'atom-one-dark
		 :outside
		 :before-load-eval '(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
		 :load-theme 'atom-one-dark)
		(when (string= spring/time-block "daytime")
			(eaf-browser-set))
		(setq spring/time-block "night")))

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

(defun spring/copy-license (license-name)
	"Copy the license file to current directory."
	(interactive (list
								(completing-read "sLincense name: "
																 '("MIT" "GPL-3.0"))))
	(pcase license-name
		("MIT"
		 (copy-file "~/.emacs.d/license/MIT" "./LICENSE")
		 (message "Copy license action done."))
		("GPL-3.0"
		 (copy-file "~/.emacs.d/license/GPL-3.0" "./LICENSE")
		 (find-file "./LICENSE")
		 (message "Copy license action done."))))

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
		(let ((content	(buffer-string)))
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
					(setq save-y-or-n (read-minibuffer
														 "Do you want to save your ERC user info?(y/n)"
														 "y"))
					(when (string= save-y-or-n "y")
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

(defun spring/add-todo-in-code ()
	"Add todo content in code."
	(interactive)
	(comment-dwim 2)
	(insert "<TODO(SpringHan)> "))

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
		(shell-command (concat "pactl set-sink-volume 0 " volume) "*Volume Set*")
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
	(eaf-open-browser (concat "https://cn.bing.com/search?q=" content)))

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

(provide 'init-functions)
