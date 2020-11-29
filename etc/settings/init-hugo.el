;;;; Hugo functions here.

(defgroup spring/hugo nil
  "Hugo group."
  :group 'applications)

(defcustom spring/hugo-directory "~/Github/Blog/"
  "Hugo dir."
  :type 'string
  :group 'spring/hugo)

(defcustom spring/hugo-process nil
  "Hugo process."
  :type 'process
  :group 'spring/hugo)

(defun spring/hugo-start-server (theme)
  "Start hugo server process."
  (interactive (list (completing-read "Enter the theme: "
                                      (delete
                                       ".."
                                       (delete
                                        "."
                                        (directory-files
                                         (concat spring/hugo-directory "themes/")))))))
  (setq spring/hugo-process (start-process "Hugo Process"
                                           " *Hugo Process*"
                                           "hugo"
                                           (format "server -t %s --buildDrafts"
                                                   theme)))
  (eaf-open-url "http://127.0.0.1:1313/blog"))

(defun spring/hugo-kill-process ()
  "Kill hugo process."
  (interactive)
  (kill-process spring/hugo-process)
  (setq spring/hugo-process nil))

(provide 'init-hugo)
