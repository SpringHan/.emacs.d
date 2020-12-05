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

(defun spring/hugo-new-article (item art-name)
  "Create a new hugo article."
  (interactive (let ((items (delete ".." (delete "."
                                                 (directory-files
                                                  (concat spring/hugo-directory
                                                          "content/"))))))
                 (list (completing-read "Enter item: " items)
                       "MEnter article name: ")))
  (spring/hugo-run-command "new" item "/" art-name))

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

(defun spring/hugo-run-command (&rest args)
  "Run hugo command."
  (let ((command (concat "cd " spring/hugo-directory "; hugo")))
    (mapc #'(lambda (a) (setq command (concat command " " a)))
          args)
    (shell-command command "* Hugo*")
    (kill-buffer "* Hugo*")))

(transient-define-prefix spring/hugo ()
  "Hugo functions."
  :info-manual "(Spring/Hugo)Hugo functions."
  [["New"
    ("n" "New article" spring/hugo-new-article)]
   ["Build"
    ("b" "Build" spring/hugo-build)
    ("s" "Server" spring/hugo-start-server)
    ("k" "Kill Server" spring/hugo-kill-process)]])

(provide 'init-hugo)
