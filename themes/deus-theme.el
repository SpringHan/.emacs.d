;;; -*- lexical-binding: t -*-

(deftheme deus "Dark retro-modern theme with full IDE-level syntax support")

(custom-theme-set-faces
 'deus
 ;; 核心 UI 元素
 `(default                        ((t :background "#2C323B" :foreground "#d2d2d2")))
 `(hl-line                        ((t :background "#3c3836" :extend t)))
 `(cursor                         ((t :background "#fabd2f")))
 `(region                         ((t :background "#665c54" :distant-foreground "#ebdbb2")))
 `(fringe                         ((t :background "#242a32")))
 `(vertical-border                ((t :foreground "#665c54")))
 `(window-divider                 ((t :foreground "#7c6f64")))
 `(line-number                    ((t :foreground "#928374" :background "#242a32")))
 `(line-number-current-line       ((t :foreground "#ebdbb2" :background "#3c3836" :bold t)))

 ;; 模式行增强
 `(mode-line                      ((t :background "#3c3836" :foreground "#98C379" 
                                   :box (:line-width -1 :color "#242a32" :style flat-button))))
 `(mode-line-inactive             ((t :background "#242a32" :foreground "#928374" 
                                   :box (:line-width -1 :color "#3c3836"))))
 `(mode-line-buffer-id            ((t :foreground "#83a598" :bold t)))
 `(mode-line-highlight            ((t :foreground "#fe8019" :box nil)))

 ;; 语法高亮体系（严格对应 Vim 语义）
 `(font-lock-comment-face         ((t :foreground "#928374" :italic t :background "#2C323B")))
 `(font-lock-doc-face             ((t :inherit font-lock-comment-face :bold t)))
 `(font-lock-keyword-face         ((t :foreground "#fb4934" :bold t)))  ; 关键字红色加粗
 `(font-lock-function-name-face   ((t :foreground "#98C379" :bold t)))  ; 函数名绿色加粗
 `(font-lock-variable-name-face   ((t :foreground "#83a598" :slant normal))) 
 `(font-lock-string-face          ((t :foreground "#98C379" :background "#3c3836"))) ; 字符串绿底灰
 `(font-lock-type-face            ((t :foreground "#fabd2f")))          ; 类型黄色
 `(font-lock-constant-face        ((t :foreground "#C678DD")))         ; 常量紫色
 `(font-lock-builtin-face         ((t :foreground "#8ec07c" :weight semi-bold))) 
 `(font-lock-warning-face         ((t :foreground "#fb4934" :background "#3c3836" :bold t)))

 ;; 交互组件增强
 `(isearch                        ((t :background "#fabd2f" :foreground "#2C323B" :bold t)))
 `(lazy-highlight                 ((t :background "#665c54" :foreground "#d5c4a1")))
 `(show-paren-match               ((t :background "#665c54" :bold t :underline "#fe8019")))
 `(minibuffer-prompt              ((t :foreground "#8ec07c" :bold t :height 1.1)))
 `(ivy-current-match              ((t :background "#665c54" :distant-foreground "#fabd2f")))
 `(ivy-minibuffer-match-highlight ((t :foreground "#fe8019" :underline t)))
 `(company-tooltip-selection      ((t :background "#3c3836" :foreground "#83a598" :bold t)))

 ;; 开发工具集成
 `(rainbow-delimiters-depth-1     ((t :foreground "#83a598")))
 `(rainbow-delimiters-depth-2     ((t :foreground "#C678DD")))
 `(rainbow-delimiters-depth-3     ((t :foreground "#8ec07c")))
 `(lsp-ui-doc-background          ((t :background "#242a32" :foreground "#a89984")))
 `(lsp-face-highlight-read        ((t :background "#665c54" :underline "#fe8019")))

 ;; 文件管理
 `(dired-directory                ((t :foreground "#83a598" :bold t :height 1.05)))
 `(dired-ignored                  ((t :foreground "#928374" :strike-through t)))
 `(dired-subtree-depth-1-face     ((t :background "#242a32")))
 `(dired-subtree-depth-2-face     ((t :background "#3c3836")))

 ;; 通信工具
 `(telega-entity-type-code        ((t :inherit fixed-pitch :background "#3c3836")))
 `(telega-msg-heading             ((t :foreground "#fabd2f" :bold t :height 1.1)))

 ;; Markdown/Org 模式
 `(markdown-header-face-1         ((t :foreground "#fb4934" :bold t :height 1.8)))
 `(markdown-code-face             ((t :inherit font-lock-constant-face :background "#242a32")))
 `(org-level-1                    ((t :foreground "#fb4934" :bold t :height 1.6 :overline "#665c54")))
 `(org-block                      ((t :background "#242a32" :extend t :weight light)))
 `(org-table                      ((t :foreground "#83a598" :background "#3c3836")))

 ;; 滚动条/侧边栏
 `(yascroll:thumb-fringe          ((t :background "#665c54" :foreground "#3c3836")))
 `(neotree-dir-face               ((t :inherit dired-directory :weight ultra-bold)))
 `(neotree-file-face              ((t :foreground "#a89984" :slant italic)))

 ;; 编程语言增强
 `(web-mode-html-tag-face         ((t :foreground "#8ec07c" :weight semi-bold)))
 `(js2-function-param-face        ((t :foreground "#83a598" :underline "#665c54")))
 `(rust-question-mark-face        ((t :foreground "#fe8019" :bold t)))
 `(python-builtin-face            ((t :foreground "#C678DD" :weight light))) )

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'deus)
;;; deus-theme.el ends here
