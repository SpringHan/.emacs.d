;;;; This file is used for the keybindings
(define-prefix-command 'ctl-z-map)		 ; Create the C-z map
(define-prefix-command 'more-functions)
(global-set-key (kbd "C-q") 'ctl-z-map)		 ; Set the ctl-z-map
(global-set-key (kbd "C-=") 'more-functions)
(global-set-key (kbd "C-q i") 'open-config-file) ; Open the init.el
(global-set-key (kbd "C-q C-b") 'buffer-menu) ; Open the buffer menu
(global-set-key (kbd "C-q C-i") 'spring/open-erc) ;Open the erc
(global-set-key (kbd "C-q C-p") 'previous-buffer) ; Goto previous buffer
(global-set-key (kbd "C-q C-n") 'next-buffer)	  ;Goto Next buffer
(global-set-key (kbd "<f12>") 'tab-bar-mode) ; Open or close the tab-bar-mode
(global-set-key (kbd "C-q C-m w") 'window-move) ; Move the window
(global-set-key (kbd "C-q C-a") 'set-alpha) ; Set the emacs' alpha
(global-set-key (kbd "C-q C-o") 'open-the-dir) ; Open the gtd/github directory
(global-set-key (kbd "C-q r") 'undo-redo) ; Redo
(global-set-key (kbd "C-q t") 'spring/toggle-truncate-lines) ; Touch file
(global-set-key (kbd "C-q s") #'(lambda () (interactive)
                                  (start-process "Simple Terminal" nil "st")))
(global-set-key (kbd "C-q w") 'write-scratch) ; New a write scratch buffer
(global-set-key (kbd "C-q f") 'mark-defun) ; Mark the function SAME LIKE C-M-h
(global-set-key (kbd "C-q e") 'eshell) ; Open eshell
(global-set-key (kbd "C-q C-t") 'load-the-theme) ; Load theme
(global-set-key (kbd "C-x t O") 'tab-bar-switch-to-prev-tab) ; tab-previous
(global-set-key (kbd "C-q k") 'kill-unwanted-buffer) ; Kill the unwanted buffers made in init load
(global-set-key (kbd "C-x t 3") 'tab-bar-new-with-buffer) ; Create a new tab then select a buffer
(global-set-key (kbd "C-x t 4") 'tab-bar-close-tab-by-name) ; Close the tab by its name
(global-set-key (kbd "C-x t s") 'spring/tab-bar-new-scratch) ; Create a new tab then select scratch buffer
(global-set-key (kbd "C-x t k") 'spring/tab-bar-close-tab-kill-buffer) ; Kill the current buffer and close the tab
(global-set-key (kbd "C-q C") 'spring/copy-license) ; Copy the license to current directory
(global-set-key (kbd "C-q o") 'spring/open-scratch) ; Open the scratch buffer
(global-set-key (kbd "C-q C-d") 'delete-char) ; Delete the char
(global-set-key (kbd "C-q C-r") 'revert-buffer) ; Revert current buffer
(global-set-key (kbd "C-q M-c") 'spring/downcase-word-first-letter) ; Downcase the first letter in the word at point
(global-set-key (kbd "C-q O") 'spring/erase-contents) ; Erase all the contents of *scratch* buffer
(global-set-key (kbd "C-q P") 'list-processes) ; Show the processes buffer
(global-set-key (kbd "C-q C-v u") 'spring/up-5-volume) ; Up 5 volume
(global-set-key (kbd "C-q C-v d") 'spring/down-5-volume) ; Down 5 volume
(global-set-key (kbd "C-q v") 'spring/set-volume) ; Set volume
(global-set-key (kbd "C-q V") 'spring/show-volume) ; Show current volume
(global-set-key (kbd "C-q 0") 'spring/no-volume) ; Set the volume to 0
(global-set-key (kbd "C-q l") 'spring/show-packages-required) ; Show all the packages have required
(global-set-key (kbd "C-q S") 'sudo-save)
(global-set-key (kbd "C-q E") 'spring/edit-snippets) ; Edit the snippets
(global-set-key (kbd "C-= SPC") 'spring/change-indent-type) ; Change indent type
(global-set-key (kbd "C-= f") 'recentf-open-files) ; Open recentf files
(global-set-key (kbd "C-x K") 'kill-current-buffer)	; Kill the current buffer
(global-set-key (kbd "C-q T") 'spring/terlat-translate)	;Translate the content
(global-set-key (kbd "C-q F") 'spring/format-commit) ;Format Commit
(global-set-key (kbd "C-\\") 'nil)
(global-set-key (kbd "C-\\ j") (lambda () (interactive) (spring/change-input-method 'japanese)))
(global-set-key (kbd "C-\\ c") (lambda () (interactive) (spring/change-input-method 'pyim)))
(global-set-key (kbd "C-\\ C-\\") (lambda () (interactive) (spring/change-input-method 0)))
(global-set-key (kbd "M-f") 'toggle-input-method)
(global-set-key (kbd "<f6>") 'spring/disable-modeline)
(global-set-key (kbd "<f7>") 'spring/initialize-input-method)
(global-set-key (kbd "C-:") 'spring/insert-result)
(global-set-key (kbd "C-x o") 'spring/move-to-window)
(global-set-key (kbd "C-q m") #'spring/child-frame)
(global-set-key (kbd "C-<return>") #'spring/enter-with-semicolon)
(global-set-key (kbd "C-,") #'spring/enter-with-comma)


(provide 'init-keymaps)
