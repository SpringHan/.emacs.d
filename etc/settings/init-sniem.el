;;;; This is the sniem config

;;; Sniem
(gpack sniem
  :repo "SpringHan/sniem")

(sniem-leader-set-key
 "q" 'sniem-keypad)

(sniem-set-keyboard-layout 'colemak)

(provide 'init-sniem)
