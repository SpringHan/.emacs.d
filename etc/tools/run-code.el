;;; This file is used for code running.
(defun spring-run-code (&optional not-run)
  "The function to run code."
  (interactive "P")
  (let (file-name command unknow-mode)
  	(setq file-name (file-name-base (buffer-name)))
    (pcase major-mode
      ('c-mode
       (setq command (format "gcc --std=c17 %s -o %s; ./%s"
                             (buffer-name) file-name file-name)))
      ('c++-mode
       (setq command (format "g++ -std=c++20 %s -o %s -l SDL2 -l SDL2_image; ./%s"
                             (buffer-name) file-name file-name)))
      ('python-mode
       (setq command (concat "python3 " (buffer-name))))
      (_ (message "There're no running way for current filetype.")
         (setq unknow-mode t)))
    (unless unknow-mode
      (if (get-buffer-window "*eshell*")
          (select-window (get-buffer-window "*eshell*"))
        (split-window nil nil 'above)
        (eshell))
      (insert command)
      )))

(provide 'run-code)
