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
  (setq spring/hugo-process (start-process-shell-command "Hugo Process"
                                           " *Hugo Process*"
                                           (format
                                            "cd %s; hugo server -t %s --buildDrafts"
                                            spring/hugo-directory theme)))
  (sleep-for 1)
  (eaf-open-browser "http://127.0.0.1:1313/blog"))

(defun spring/hugo-kill-process ()
  "Kill hugo process."
  (interactive)
  (kill-process spring/hugo-process)
  (setq spring/hugo-process nil)
  (message "[Hugo]: Hugo server killed."))

(defun spring/hugo-new-article (item art-name)
  "Create a new hugo article."
  (interactive (let ((items (delete ".." (delete "."
                                                 (directory-files
                                                  (concat spring/hugo-directory
                                                          "content/"))))))
                 (list (completing-read "Enter item: " items)
                       (read-string "Enter article name: "))))
  (spring/hugo-run-command "new" (concat item "/" art-name ".md")))

(defun spring/hugo-build (theme base-url)
  "Build hugo."
  (interactive (let ((themes (delete ".." (delete "."
                                                 (directory-files
                                                  (concat spring/hugo-directory
                                                          "themes/"))))))
                 (list (completing-read "Enter theme name: " themes)
                       (read-string "Enter base-url: "
                                    "https://springhan.gitee.io/blog"))))
  (spring/hugo-run-command (format "--theme=%s --baseUrl=\"%s\"" theme base-url)
                           "--buildDrafts"))

(defun spring/open-hugo-directory ()
  "Open the hugo directory."
  (interactive)
  (find-file spring/hugo-directory))

(defun spring/hugo-open-blog (url)
  "Open the remote blog."
  (interactive (list (read-string "Enter url: "
                                  "https://springhan.gitee.io/blog")))
  (eaf-open-browser url))

(defun spring/hugo-run-command (&rest args)
  "Run hugo command."
  (let ((command (concat "cd " spring/hugo-directory "; hugo")))
    (mapc #'(lambda (a) (setq command (concat command " " a)))
          args)
    (shell-command command " *Hugo*")
    (kill-buffer " *Hugo*")))

(transient-define-prefix spring/hugo ()
  "Hugo functions."
  :info-manual "(Spring/Hugo)Hugo functions."
  [["New"
    ("n" "New article" spring/hugo-new-article)]
   ["Build"
    ("b" "Build" spring/hugo-build)
    ("s" "Server" spring/hugo-start-server)
    ("k" "Kill Server" spring/hugo-kill-process)]
   ["Open"
    ("d" "Directory" spring/open-hugo-directory)
    ("u" "Remote" spring/hugo-open-blog)]])

(provide 'init-hugo)
