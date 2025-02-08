;;; -*- lexical-binding: t -*-

(deftheme deus "A retro-modern color theme ported from Vim's deus scheme.")

(custom-theme-set-faces
 'deus
 ;; Basic UI
 `(default                        ((t :background "#2C323B" :foreground "#d2d2d2")))
 `(hl-line                        ((t :background "#3c3836")))
 `(cursor                         ((t :background "#fabd2f")))
 `(region                         ((t :background "#665c54")))
 `(fringe                         ((t :background "#2C323B")))
 `(mode-line                      ((t :background "#3c3836" :foreground "#ebdbb2" :box (:line-width -1 :color "#242a32"))))
 `(mode-line-inactive             ((t :background "#242a32" :foreground "#928374" :box (:line-width -1 :color "#3c3836"))))
 `(vertical-border                ((t :foreground "#665c54")))

 ;; Syntax Highlighting
 `(font-lock-comment-face         ((t :foreground "#928374" :italic t)))
 `(font-lock-doc-face             ((t :foreground "#98C379" :italic t)))
 `(font-lock-string-face          ((t :foreground "#98C379" :background "#3c3836")))
 `(font-lock-keyword-face         ((t :foreground "#fb4934" :bold t))) 
 `(font-lock-function-name-face   ((t :foreground "#98C379" :bold t)))
 `(font-lock-variable-name-face   ((t :foreground "#83a598")))
 `(font-lock-type-face            ((t :foreground "#fabd2f")))
 `(font-lock-constant-face        ((t :foreground "#C678DD")))
 `(font-lock-builtin-face         ((t :foreground "#8ec07c")))
 `(font-lock-warning-face         ((t :foreground "#fb4934" :bold t)))

 ;; Special
 `(isearch                        ((t :background "#fabd2f" :foreground "#2C323B")))
 `(lazy-highlight                 ((t :background "#665c54")))
 `(link                           ((t :foreground "#83a598" :underline t)))
 `(minibuffer-prompt              ((t :foreground "#8ec07c" :bold t)))
 `(highlight                      ((t :background "#665c54")))

 ;; Org-mode
 `(org-level-1                    ((t :foreground "#fb4934" :bold t :height 1.3)))
 `(org-level-2                    ((t :foreground "#98C379" :bold t :height 1.2)))
 `(org-code                       ((t :foreground "#C678DD")))
 `(org-block                      ((t :background "#242a32" :extend t)))

 ;; Programming
 `(rainbow-delimiters-depth-1-face ((t :foreground "#83a598")))
 `(rainbow-delimiters-depth-2-face ((t :foreground "#C678DD")))
 `(show-paren-match               ((t :background "#665c54" :bold t)))
 
 ;; Dired
 `(dired-directory                ((t :foreground "#83a598" :bold t)))
 `(dired-ignored                  ((t :foreground "#928374"))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'deus)
;;; deus-theme.el ends here
