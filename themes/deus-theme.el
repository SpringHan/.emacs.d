;;; -*- lexical-binding: t -*-

(deftheme deus "The emacs version of a theme originated from vim.")

(custom-theme-set-faces
 'deus
 `(default ((((type graphic))
             :background "#242a32"
             :foreground "#ebdbb2")))
 `(hl-line ((((type graphic))
             :background "#242a32")))
 `(cursor  ((t (:background "#ebdbb2" :foreground "#242a32"))))
 '(region  ((t (:inherit cursor))))
 '(fringe  ((t ())))
 `(font-lock-comment-face  ((t (:foreground "#d2d2d2" :italic t))))
 `(font-lock-doc-face  ((t (:foreground "#d2d2d2" :bold t))))
 `(font-lock-warning-face  ((t (:foreground "#fb4934"))))
 `(font-lock-string-face  ((t (:foreground "#98c379"))))
 `(font-lock-function-name-face  ((t (:inherit font-lock-string-face :bold t))))
 `(font-lock-keyword-face  ((t (:inherit font-lock-warning-face))))
 `(font-lock-constant-face  ((t (:foreground "#c678dd"))))
 `(font-lock-builtin-face  ((t ())))
 `(font-lock-variable-name-face  ((t ())))
 `(font-lock-type-face  ((t (:foreground "#fabd2f"))))
 `(font-lock-preprocessor-face  ((t (:foreground "#8ec07c"))))
 '(dired-directory  ((t (:inherit font-lock-function-name-face))))
 '(mode-line  ((((type graphic))
                :foreground "#8be9fd"
                :background "#44475a")))
 '(mode-line-inactive  ((((type graphic))
                         :background "#44475a")))
 '(highlight-symbol-face  ((t ())))
 `(line-number-current-line  ((((type graphic))
                               :bold t
                               :inherit default)))
 `(line-number  ((((type graphic))
                  :foreground "#7c6f64")))
 )
