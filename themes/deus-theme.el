;;; deus-theme.el --- A retro-modern dark theme, port of vim-deus -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2026 Avery Wagar
;; Original Vim colorscheme: colors/deus.vim
;; URL: https://github.com/ajmwagar/vim-deus
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: faces, themes, color

;; This file is not part of GNU Emacs.

;;; License:

;; MIT License.  See the LICENSE file in the vim-deus repository:
;; https://github.com/ajmwagar/vim-deus/blob/master/LICENSE

;;; Commentary:

;; Deus is a retro-modern dark colorscheme.  This file is a port of
;; `vim-deus' (the colorscheme Vim actually renders) plus the
;; 16-color palette declared in `deus.json'.
;;
;; Palette (mirrors s:ds in colors/deus.vim):
;;
;;   name    hex       vim variable     used for
;;   ------  --------  ---------------  --------------------------------
;;   bg0     #2c323b   ds.dark0         background
;;   bg1     #3c3836   ds.dark1         color column, error background
;;   bg2     #242a32   ds.dark2         cursor line, popup menu
;;   bg3     #665c54   ds.dark3         selection, statusline highlight
;;   bg4     #7c6f64   ds.dark4         line numbers, tab fill
;;   gray    #928374   ds.gray_245      comments, folded text
;;   fg0     #d2d2d2   ds.light0        plain text in documents
;;   fg1     #ebdbb2   ds.light1        plain text in programming buffers
;;   fg2     #d5c4a1   ds.light2        soft text
;;   fg3     #bdae93   ds.light3        punctuation, delimiters
;;   fg4     #a89984   ds.light4        inactive text, split border
;;   red     #fb4934   ds.bright_red    statements, errors
;;   green   #98c379   ds.bright_green  strings, functions, diffs
;;   yellow  #fabd2f   ds.bright_yellow types, search, warnings
;;   blue    #83a598   ds.bright_blue   identifiers, links
;;   purple  #c678dd   ds.bright_purple constants, numbers
;;   aqua    #8ec07c   ds.bright_aqua   preprocessor, structures
;;   orange  #fe8019   ds.bright_orange special chars, prompts, cursor
;;
;; `deus.json' declares a slightly different red (#d54e53) and yellow
;; (#e5c07b) than the ones colors/deus.vim renders (#fb4934, #fabd2f).
;; Syntax highlighting follows the Vim file, because that is what Deus
;; looks like in practice; the ANSI/terminal palette follows deus.json,
;; because that is the only place those sixteen colours are declared.
;;
;; Plain text: documents (org, markdown, info, help, dired, ...) use the
;; old-deus white #d2d2d2 (ds.light0), so prose does not look yellow.
;; Programming buffers keep the Vim Normal foreground #ebdbb2 (ds.light1)
;; through a buffer-local `face-remap' installed from `prog-mode-hook' when
;; the theme is enabled; `disable-theme' removes it again.  This is the one
;; piece of behaviour that cannot be expressed with face specs alone.
;;
;; Mapping from the Vim highlight groups to Emacs faces:
;;
;;   Vim group                  Emacs face(s)
;;   -------------------------  -----------------------------------------
;;   Normal                     default
;;   CursorLine / CursorColumn  hl-line
;;   Visual / VisualNOS         region
;;   Search                     lazy-highlight, match
;;   IncSearch                  isearch
;;   LineNr / CursorLineNr      line-number, line-number-current-line
;;   SignColumn                 fringe
;;   ColorColumn                fill-column-indicator
;;   VertSplit                  vertical-border, window-divider
;;   WildMenu                   completions-highlight
;;   StatusLine / StatusLineNC  mode-line, mode-line-inactive
;;   TabLine / TabLineFill      tab-bar, tab-line
;;   Pmenu / PmenuSel           company-*, corfu-*, completions-*
;;   Error / ErrorMsg           error, font-lock-warning-face
;;   WarningMsg                 warning
;;   Question                   minibuffer-prompt
;;   Directory / Title          dired-directory, info-title-*
;;   Statement, Conditional,    font-lock-keyword-face
;;     Repeat, Label, Keyword
;;   Identifier                 font-lock-variable-name-face
;;   Function                   font-lock-function-name-face
;;   Comment                    font-lock-comment-face
;;   String                     font-lock-string-face
;;   Constant, Number, Float    font-lock-constant-face,
;;                                font-lock-number-face
;;   PreProc, Include, Macro    font-lock-preprocessor-face
;;   Special, builtins          font-lock-builtin-face
;;   Type, Typedef              font-lock-type-face
;;   Operator                   font-lock-operator-face
;;   Diff add/change/delete     diff-*, magit-diff-*
;;   SpellBad, SpellCap, ...    flyspell-*, flymake-*
;;   IndentGuides               highlight-indent-guides-*
;;   Rainbow parentheses        rainbow-delimiters-*
;;   Syntastic / ALE            flycheck-*, flymake-*
;;   GitGutter / Signify        diff-hl-*, git-gutter:*
;;   CtrlP / FZF / Startify     vertico, ivy, helm
;;   Deoplete / YCM / coc       company, corfu
;;   EasyMotion / Sneak         avy, isearch
;;   Netrw / NERDTree           dired, treemacs
;;
;; Deliberate deviations from the Vim file, each documented at the face
;; that differs:
;;
;;   * `region': colors/deus.vim draws Visual with `inverse', which would
;;     paint selected text in the selection colour.  A readable dark
;;     background (bg3) is used instead.
;;   * `mode-line': follows old-deus, not colors/deus.vim: green on bg1
;;     with a flat-button box.  (The Vim file sets a light StatusLine and
;;     then resets it to `guibg=NONE guifg=NONE' for BuftabLine.)
;;   * `vertical-border': The Vim split separator is the `|' glyph in fg4
;;     on bg2.  Emacs draws a one-pixel rule, where fg4 would dominate, so
;;     the dimmer bg3 is used.
;;
;; Usage:
;;
;;   (add-to-list 'custom-theme-load-path "/path/to/vim-deus/")
;;   (load-theme 'deus t)
;;
;; or, once the directory is on `load-path', simply
;;
;;   M-x load-theme RET deus RET
;;
;; The theme is dark-only; it does not register a light variant.

;;; Code:

(deftheme deus
  "A retro-modern dark theme, ported from vim-deus by Avery Wagar.
The palette and group assignments are taken from colors/deus.vim; the
16-color terminal palette is taken from deus.json.  Plain text is the
old-deus white in documents and the Vim pale yellow in programming
buffers.")

(let ((class '((class color) (min-colors 89)))
      ;; Background ramp (s:ds.dark0 .. s:ds.dark4).
      (bg0 "#2c323b")
      (bg1 "#3c3836")
      (bg2 "#242a32")
      (bg3 "#665c54")
      (bg4 "#7c6f64")
      ;; Neutral (s:ds.gray_245).
      (gray "#928374")
      ;; Foreground ramp (s:ds.light0 .. s:ds.light4).
      (fg0 "#d2d2d2")
      (fg1 "#ebdbb2")
      (fg2 "#d5c4a1")
      (fg3 "#bdae93")
      (fg4 "#a89984")
      ;; Accents (s:ds.bright_*).
      (red "#fb4934")
      (green "#98c379")
      (yellow "#fabd2f")
      (blue "#83a598")
      (purple "#c678dd")
      (aqua "#8ec07c")
      (orange "#fe8019")
      ;; 16-color terminal palette, verbatim from deus.json ("color" array).
      (ansi-black "#000000")
      (ansi-red "#d54e53")
      (ansi-green "#98c379")
      (ansi-yellow "#e5c07b")
      (ansi-blue "#83a598")
      (ansi-magenta "#c678dd")
      (ansi-cyan "#70c0ba")
      (ansi-white "#ffffff")
      (ansi-bright-black "#666666")
      (ansi-bright-red "#ec3e45")
      (ansi-bright-green "#90c966")
      (ansi-bright-yellow "#edbf69")
      (ansi-bright-blue "#73ba9f")
      (ansi-bright-magenta "#c858e9")
      (ansi-bright-cyan "#2bcec2")
      (ansi-bright-white "#ffffff"))

  (custom-theme-set-faces
   'deus

   ;; -----------------------------------------------------------------
   ;; Base UI -- "General UI" in colors/deus.vim
   ;; -----------------------------------------------------------------
   ;; Plain text follows old-deus: #d2d2d2 for documents.  Programming
   ;; buffers get #ebdbb2 back through `deus-theme--prog-remap-add'.
   `(default ((,class (:background ,bg0 :foreground ,fg0))))
   ;; Cursor: old-deus style, a solid yellow block (#fabd2f).
   `(cursor ((,class (:background ,yellow))))
   `(mouse ((,class (:background ,fg1 :foreground ,bg0))))
   `(fringe ((,class (:background ,bg0 :foreground ,fg4))))
   `(region ((,class (:background ,bg3 :foreground ,fg0 :extend t))))
   `(secondary-selection ((,class (:background ,bg2 :foreground ,fg1 :extend t))))
   `(highlight ((,class (:background ,bg2 :foreground ,fg0))))
   `(shadow ((,class (:foreground ,gray))))
   `(link ((,class (:foreground ,blue :underline t))))
   `(link-visited ((,class (:foreground ,purple :underline t))))
   `(button ((,class (:foreground ,blue :underline t))))
   `(escape-glyph ((,class (:foreground ,orange))))
   `(nobreak-space ((,class (:foreground ,green))))
   `(glyphless-char ((,class (:foreground ,gray))))
   ;; CursorLine: the screen line the cursor is on.
   `(hl-line ((,class (:background ,bg2 :extend t))))
   `(trailing-whitespace ((,class (:background ,bg2))))
   `(header-line ((,class (:background ,bg2 :foreground ,fg1 :weight bold))))
   `(header-line-highlight ((,class (:background ,bg3 :foreground ,fg0))))
   `(vertical-border ((,class (:foreground ,bg3 :background ,bg0))))
   `(window-divider ((,class (:foreground ,bg3))))
   `(window-divider-first-pixel ((,class (:foreground ,bg2))))
   `(window-divider-last-pixel ((,class (:foreground ,bg2))))
   ;; `internal-border' and `child-frame-border' are deliberately left alone.
   ;; Emacs's own spec for both is `(t nil)', i.e. "no attributes", which makes
   ;; a frame fill its border with its own background colour.  posframe paints
   ;; the border of a child frame with `child-frame-border' and only overrides
   ;; that when the caller passes a border colour; emacs-rime passes none
   ;; (`rime-posframe-properties' only sets `:internal-border-width 10') and
   ;; gives the frame the background of `rime-default-face' (#333333), so any
   ;; colour here turns into a ring of the wrong colour around the candidate
   ;; window.  Same for `internal-border' on non-child frames.
   `(tooltip ((,class (:background ,bg2 :foreground ,fg1))))
   `(menu ((,class (:background ,bg2 :foreground ,fg1))))
   `(scroll-bar ((,class (:background ,bg2 :foreground ,bg4))))
   `(help-key-binding ((,class (:background ,bg2 :foreground ,blue))))
   `(minibuffer-prompt ((,class (:foreground ,orange :weight bold))))
   `(error ((,class (:foreground ,red :weight bold))))
   `(warning ((,class (:foreground ,red :weight bold))))
   `(success ((,class (:foreground ,green :weight bold))))

   ;; Mode line: old-deus style -- green on bg1 with a flat-button box.
   `(mode-line ((,class (:background ,bg1 :foreground ,green
                                    :box (:line-width -1 :color ,bg2 :style flat-button)))))
   `(mode-line-inactive ((,class (:background ,bg2 :foreground ,gray
                                             :box (:line-width -1 :color ,bg1)))))
   `(mode-line-buffer-id ((,class (:foreground ,blue :weight bold))))
   `(mode-line-highlight ((,class (:foreground ,orange :box nil))))

   ;; TabLineFill / TabLineSel: the active tab is painted with bg4.
   `(tab-bar ((,class (:background ,bg0 :foreground ,bg4))))
   `(tab-bar-tab ((,class (:background ,bg4 :foreground ,bg0 :weight bold))))
   `(tab-bar-tab-inactive ((,class (:background ,bg0 :foreground ,bg4))))
   `(tab-bar-tab-ungrouped ((,class (:background ,bg0 :foreground ,gray))))
   `(tab-line ((,class (:background ,bg0 :foreground ,bg4))))
   `(tab-line-tab ((,class (:background ,bg4 :foreground ,bg0))))
   `(tab-line-tab-current ((,class (:background ,bg4 :foreground ,bg0 :weight bold))))
   `(tab-line-tab-inactive ((,class (:background ,bg0 :foreground ,bg4))))
   `(tab-line-highlight ((,class (:background ,bg2 :foreground ,fg1))))
   `(tab-line-close-highlight ((,class (:foreground ,red))))

   ;; -----------------------------------------------------------------
   ;; Search -- Search / IncSearch / MatchParen
   ;; -----------------------------------------------------------------
   `(isearch ((,class (:background ,orange :foreground ,bg0))))
   `(isearch-fail ((,class (:background ,red :foreground ,bg0))))
   `(isearch-group-1 ((,class (:background ,orange :foreground ,bg0))))
   `(isearch-group-2 ((,class (:background ,yellow :foreground ,bg0))))
   `(lazy-highlight ((,class (:background ,yellow :foreground ,bg0))))
   `(match ((,class (:background ,yellow :foreground ,bg0))))
   `(query-replace ((,class (:background ,green :foreground ,bg0))))
   `(occur-match ((,class (:background ,yellow :foreground ,bg0))))
   `(show-paren-match ((,class (:background ,bg3 :foreground ,fg0 :weight bold))))
   `(show-paren-match-expression ((,class (:background ,bg3 :foreground ,fg0))))
   `(show-paren-mismatch ((,class (:background ,red :foreground ,bg0 :weight bold))))

   ;; -----------------------------------------------------------------
   ;; Gutter -- LineNr / SignColumn / Folded
   ;; -----------------------------------------------------------------
   `(line-number ((,class (:background ,bg0 :foreground ,bg4))))
   `(line-number-current-line ((,class (:background ,bg2 :foreground ,fg1 :weight bold))))
   `(line-number-major-tick ((,class (:background ,bg2 :foreground ,gray))))
   `(line-number-minor-tick ((,class (:background ,bg0 :foreground ,gray))))
   `(fill-column-indicator ((,class (:foreground ,bg1))))

   ;; -----------------------------------------------------------------
   ;; Syntax -- "Syntax Highlighting" in colors/deus.vim
   ;; -----------------------------------------------------------------
   `(font-lock-comment-face ((,class (:foreground ,gray :slant italic))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,gray :slant italic))))
   `(font-lock-doc-face ((,class (:foreground ,gray :slant italic))))
   `(font-lock-keyword-face ((,class (:foreground ,red))))
   `(font-lock-builtin-face ((,class (:foreground ,orange))))
   `(font-lock-function-name-face ((,class (:foreground ,green :weight bold))))
   `(font-lock-function-call-face ((,class (:foreground ,green :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,blue))))
   `(font-lock-variable-use-face ((,class (:foreground ,blue))))
   `(font-lock-type-face ((,class (:foreground ,yellow))))
   `(font-lock-constant-face ((,class (:foreground ,purple))))
   `(font-lock-number-face ((,class (:foreground ,purple))))
   `(font-lock-property-name-face ((,class (:foreground ,blue))))
   `(font-lock-property-use-face ((,class (:foreground ,blue))))
   `(font-lock-string-face ((,class (:foreground ,green))))
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,aqua :weight bold))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground ,aqua))))
   `(font-lock-negation-char-face ((,class (:foreground ,red))))
   `(font-lock-operator-face ((,class (:foreground ,fg1))))
   `(font-lock-bracket-face ((,class (:foreground ,fg3))))
   `(font-lock-warning-face ((,class (:foreground ,red :weight bold))))
   `(font-lock-doc-markup-face ((,class (:foreground ,orange :weight bold))))
   `(syntax-error ((,class (:underline (:style wave :color ,red)))))

   ;; `Todo' is bold+italic in the normal foreground colour in the Vim file.
   `(hl-todo ((,class (:foreground ,fg1 :weight bold :slant italic))))

   ;; -----------------------------------------------------------------
   ;; Diffs -- DiffAdd / DiffChange / DiffText / DiffDelete, which the Vim
   ;; file draws with `inverse': accent background, background-coloured text.
   ;; -----------------------------------------------------------------
   `(diff-added ((,class (:background ,green :foreground ,bg0 :extend t))))
   `(diff-removed ((,class (:background ,red :foreground ,bg0 :extend t))))
   `(diff-changed ((,class (:background ,aqua :foreground ,bg0 :extend t))))
   `(diff-refine-added ((,class (:background ,green :foreground ,bg0 :weight bold :extend t))))
   `(diff-refine-removed ((,class (:background ,red :foreground ,bg0 :weight bold :extend t))))
   `(diff-refine-changed ((,class (:background ,yellow :foreground ,bg0 :weight bold :extend t))))
   `(diff-header ((,class (:background ,bg2 :foreground ,fg1 :weight bold))))
   `(diff-file-header ((,class (:background ,bg2 :foreground ,orange :weight bold))))
   `(diff-hunk-header ((,class (:background ,bg2 :foreground ,yellow :weight bold))))
   `(diff-function ((,class (:foreground ,aqua))))
   `(diff-index ((,class (:foreground ,gray))))
   `(diff-nonexistent ((,class (:background ,red :foreground ,bg0))))

   ;; -----------------------------------------------------------------
   ;; Spelling -- SpellBad / SpellCap / SpellLocal / SpellRare
   ;; -----------------------------------------------------------------
   `(flyspell-incorrect ((,class (:underline (:style wave :color ,red)))))
   `(flyspell-duplicate ((,class (:underline (:style wave :color ,blue)))))
   `(flymake-error ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flymake-note ((,class (:underline (:style wave :color ,aqua)))))
   `(flymake-error-echo ((,class (:foreground ,red :weight bold))))
   `(flymake-warning-echo ((,class (:foreground ,yellow :weight bold))))
   `(flymake-note-echo ((,class (:foreground ,aqua :weight bold))))
   `(flymake-end-of-line-diagnostics-face ((,class (:foreground ,gray :slant italic))))
   `(flycheck-error ((,class (:underline (:style wave :color ,red)))))
   `(flycheck-warning ((,class (:underline (:style wave :color ,yellow)))))
   `(flycheck-info ((,class (:underline (:style wave :color ,aqua)))))
   `(flycheck-fringe-error ((,class (:foreground ,red :background ,bg2))))
   `(flycheck-fringe-warning ((,class (:foreground ,yellow :background ,bg2))))
   `(flycheck-fringe-info ((,class (:foreground ,aqua :background ,bg2))))
   `(flycheck-error-list-error ((,class (:foreground ,red :weight bold))))
   `(flycheck-error-list-warning ((,class (:foreground ,yellow :weight bold))))
   `(flycheck-error-list-info ((,class (:foreground ,aqua :weight bold))))
   `(flycheck-error-list-line-number ((,class (:foreground ,blue))))
   `(flycheck-error-list-column-number ((,class (:foreground ,purple))))

   ;; -----------------------------------------------------------------
   ;; Completion menu -- Pmenu / PmenuSel / PmenuSbar / PmenuThumb
   ;; (deoplete, YCM and coc.nvim in the Vim README map to these two.)
   ;; -----------------------------------------------------------------
   `(completions-highlight ((,class (:background ,blue :foreground ,bg2 :weight bold))))
   `(completions-common-part ((,class (:foreground ,yellow :weight bold))))
   `(completions-first-difference ((,class (:foreground ,red :weight bold))))

   `(company-tooltip ((,class (:background ,bg2 :foreground ,fg1))))
   `(company-tooltip-selection ((,class (:background ,blue :foreground ,bg2 :weight bold))))
   `(company-tooltip-common ((,class (:foreground ,yellow :weight bold))))
   `(company-tooltip-common-selection ((,class (:foreground ,bg0 :weight bold))))
   `(company-tooltip-annotation ((,class (:foreground ,gray))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,bg1))))
   `(company-tooltip-scrollbar-thumb ((,class (:background ,bg4))))
   `(company-tooltip-scrollbar-track ((,class (:background ,bg2))))
   `(company-preview ((,class (:foreground ,gray))))
   `(company-preview-common ((,class (:foreground ,yellow))))
   `(company-preview-search ((,class (:background ,bg3))))
   `(company-scrollbar-bg ((,class (:background ,bg2))))
   `(company-scrollbar-fg ((,class (:background ,bg4))))

   `(corfu-default ((,class (:background ,bg2 :foreground ,fg1))))
   `(corfu-current ((,class (:background ,blue :foreground ,bg2 :weight bold))))
   `(corfu-bar ((,class (:background ,bg4))))
   `(corfu-border ((,class (:background ,bg3))))
   `(corfu-echo ((,class (:foreground ,gray))))
   `(corfu-popupinfo ((,class (:background ,bg2 :foreground ,fg1))))

   ;; -----------------------------------------------------------------
   ;; Minibuffer completion -- WildMenu / CtrlP / FZF / Startify
   ;; -----------------------------------------------------------------
   `(vertico-current ((,class (:background ,bg2 :foreground ,fg1 :weight bold))))
   `(vertico-group-title ((,class (:foreground ,yellow :weight bold))))
   `(vertico-group-separator ((,class (:foreground ,gray))))
   `(vertico-multiline ((,class (:foreground ,gray))))
   `(vertico-quick1 ((,class (:background ,orange :foreground ,bg0))))
   `(vertico-quick2 ((,class (:background ,purple :foreground ,bg0))))

   `(ivy-current-match ((,class (:background ,bg3 :foreground ,fg0 :weight bold))))
   `(ivy-minibuffer-match-face-1 ((,class (:foreground ,gray))))
   `(ivy-minibuffer-match-face-2 ((,class (:foreground ,yellow :weight bold))))
   `(ivy-minibuffer-match-face-3 ((,class (:foreground ,orange :weight bold))))
   `(ivy-minibuffer-match-face-4 ((,class (:foreground ,purple :weight bold))))
   `(ivy-prompt-match ((,class (:background ,bg2))))
   `(ivy-virtual ((,class (:foreground ,gray :slant italic))))
   `(ivy-action ((,class (:foreground ,blue))))
   `(ivy-match-required-face ((,class (:foreground ,red :weight bold))))
   `(ivy-confirm-face ((,class (:foreground ,green))))
   `(ivy-remote ((,class (:foreground ,purple))))
   `(ivy-separator ((,class (:foreground ,gray))))

   `(helm-selection ((,class (:background ,bg3 :foreground ,fg0))))
   `(helm-match ((,class (:foreground ,yellow :weight bold))))
   `(helm-source-header ((,class (:background ,bg2 :foreground ,orange :weight bold))))
   `(helm-candidate-number ((,class (:background ,bg2 :foreground ,yellow))))
   `(helm-separator ((,class (:foreground ,gray))))
   `(helm-ff-directory ((,class (:foreground ,green :weight bold))))
   `(helm-ff-file ((,class (:foreground ,fg1))))
   `(helm-ff-executable ((,class (:foreground ,green))))
   `(helm-ff-symlink ((,class (:foreground ,aqua :slant italic))))
   `(helm-grep-file ((,class (:foreground ,blue))))
   `(helm-grep-lineno ((,class (:foreground ,gray))))
   `(helm-grep-match ((,class (:foreground ,yellow))))
   `(helm-visible-mark ((,class (:background ,bg2 :foreground ,orange))))
   `(helm-buffer-directory ((,class (:foreground ,green :weight bold))))
   `(helm-buffer-file ((,class (:foreground ,fg1))))
   `(helm-buffer-size ((,class (:foreground ,gray))))
   `(helm-non-file-buffer ((,class (:foreground ,gray))))
   `(helm-etags-file ((,class (:foreground ,blue))))

   ;; -----------------------------------------------------------------
   ;; EasyMotion / Sneak -- avy, search
   ;; -----------------------------------------------------------------
   `(avy-lead-face ((,class (:background ,orange :foreground ,bg0 :weight bold))))
   `(avy-lead-face-0 ((,class (:background ,blue :foreground ,bg0 :weight bold))))
   `(avy-lead-face-1 ((,class (:background ,gray :foreground ,bg0 :weight bold))))
   `(avy-lead-face-2 ((,class (:background ,purple :foreground ,bg0 :weight bold))))
   `(avy-background-face ((,class (:foreground ,gray))))
   `(avy-goto-char-timer-face ((,class (:background ,orange :foreground ,bg0))))

   ;; -----------------------------------------------------------------
   ;; Rainbow parentheses
   ;; -----------------------------------------------------------------
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,red))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,purple))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,blue))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,aqua))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,green))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,yellow))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,gray))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,orange))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red :weight bold))))
   `(rainbow-delimiters-mismatched-face ((,class (:background ,red :foreground ,bg0))))
   `(rainbow-delimiters-base-error-face ((,class (:background ,bg1 :foreground ,red))))

   ;; -----------------------------------------------------------------
   ;; Version control -- GitGutter / Signify
   ;; -----------------------------------------------------------------
   `(diff-hl-insert ((,class (:foreground ,green :background ,green))))
   `(diff-hl-change ((,class (:foreground ,blue :background ,blue))))
   `(diff-hl-delete ((,class (:foreground ,red :background ,red))))

   `(git-gutter:added ((,class (:foreground ,green :background ,bg2))))
   `(git-gutter:modified ((,class (:foreground ,blue :background ,bg2))))
   `(git-gutter:deleted ((,class (:foreground ,red :background ,bg2))))
   `(git-gutter:separator ((,class (:foreground ,yellow :background ,bg2))))
   `(git-gutter:unchanged ((,class (:background ,bg2))))
   `(git-gutter-fr:added ((,class (:foreground ,green :background ,green))))
   `(git-gutter-fr:modified ((,class (:foreground ,blue :background ,blue))))
   `(git-gutter-fr:deleted ((,class (:foreground ,red :background ,red))))

   `(magit-section-highlight ((,class (:background ,bg2 :extend t))))
   `(magit-section-heading ((,class (:foreground ,orange :weight bold))))
   `(magit-section-heading-selection ((,class (:foreground ,yellow :weight bold))))
   `(magit-section-secondary-heading ((,class (:foreground ,aqua :weight bold))))
   `(magit-branch-local ((,class (:foreground ,blue :weight bold))))
   `(magit-branch-remote ((,class (:foreground ,aqua :weight bold))))
   `(magit-branch-current ((,class (:foreground ,blue :weight bold :underline t))))
   `(magit-tag ((,class (:foreground ,yellow))))
   `(magit-hash ((,class (:foreground ,gray))))
   `(magit-dimmed ((,class (:foreground ,gray))))
   `(magit-log-author ((,class (:foreground ,orange))))
   `(magit-log-date ((,class (:foreground ,gray))))
   `(magit-log-graph ((,class (:foreground ,fg4))))
   `(magit-diff-added ((,class (:background ,green :foreground ,bg0 :extend t))))
   `(magit-diff-added-highlight ((,class (:background ,green :foreground ,bg0 :weight bold :extend t))))
   `(magit-diff-removed ((,class (:background ,red :foreground ,bg0 :extend t))))
   `(magit-diff-removed-highlight ((,class (:background ,red :foreground ,bg0 :weight bold :extend t))))
   `(magit-diff-context ((,class (:foreground ,gray))))
   `(magit-diff-context-highlight ((,class (:background ,bg2 :foreground ,fg2))))
   `(magit-diff-file-heading ((,class (:foreground ,fg1 :weight bold))))
   `(magit-diff-file-heading-highlight ((,class (:background ,bg2 :foreground ,fg0 :weight bold))))
   `(magit-diff-hunk-heading ((,class (:background ,bg2 :foreground ,fg1))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg3 :foreground ,fg0))))
   `(magit-diff-lines-heading ((,class (:background ,purple :foreground ,bg0))))
   `(magit-diffstat-added ((,class (:foreground ,green))))
   `(magit-diffstat-removed ((,class (:foreground ,red))))
   `(magit-process-ok ((,class (:foreground ,green :weight bold))))
   `(magit-process-ng ((,class (:foreground ,red :weight bold))))
   `(magit-blame-heading ((,class (:background ,bg2 :foreground ,fg2 :extend t))))
   `(magit-blame-name ((,class (:foreground ,orange))))
   `(magit-blame-date ((,class (:foreground ,gray))))
   `(magit-blame-hash ((,class (:foreground ,gray))))
   `(magit-blame-summary ((,class (:foreground ,fg1))))
   `(magit-blame-content ((,class (:foreground ,fg1))))
   `(magit-cherry-equivalent ((,class (:foreground ,purple))))
   `(magit-cherry-unmatched ((,class (:foreground ,aqua))))

   ;; -----------------------------------------------------------------
   ;; Netrw / NERDTree -- dired, treemacs
   ;; -----------------------------------------------------------------
   `(dired-directory ((,class (:foreground ,green :weight bold))))
   `(dired-flagged ((,class (:foreground ,red))))
   `(dired-header ((,class (:foreground ,aqua :weight bold))))
   `(dired-ignored ((,class (:foreground ,gray))))
   `(dired-mark ((,class (:foreground ,purple))))
   `(dired-marked ((,class (:foreground ,yellow :weight bold))))
   `(dired-perm-write ((,class (:foreground ,red))))
   `(dired-symlink ((,class (:foreground ,aqua :slant italic))))
   `(dired-warning ((,class (:foreground ,red :weight bold))))

   `(treemacs-directory-face ((,class (:foreground ,green :weight bold))))
   `(treemacs-file-face ((,class (:foreground ,fg1))))
   `(treemacs-root-face ((,class (:foreground ,orange :weight bold :underline t))))
   `(treemacs-tags-face ((,class (:foreground ,blue))))
   `(treemacs-git-added-face ((,class (:foreground ,green))))
   `(treemacs-git-modified-face ((,class (:foreground ,blue))))
   `(treemacs-git-deleted-face ((,class (:foreground ,red))))
   `(treemacs-git-renamed-face ((,class (:foreground ,purple))))
   `(treemacs-git-untracked-face ((,class (:foreground ,yellow))))
   `(treemacs-git-ignored-face ((,class (:foreground ,gray))))

   ;; -----------------------------------------------------------------
   ;; Org-mode
   ;; -----------------------------------------------------------------
   `(org-hide ((,class (:foreground ,bg0))))
   `(org-level-1 ((,class (:foreground ,green :weight bold))))
   `(org-level-2 ((,class (:foreground ,yellow :weight bold))))
   `(org-level-3 ((,class (:foreground ,aqua :weight bold))))
   `(org-level-4 ((,class (:foreground ,blue :weight bold))))
   `(org-level-5 ((,class (:foreground ,purple :weight bold))))
   `(org-level-6 ((,class (:foreground ,orange))))
   `(org-level-7 ((,class (:foreground ,fg2))))
   `(org-level-8 ((,class (:foreground ,gray))))
   `(org-todo ((,class (:foreground ,red :weight bold))))
   `(org-done ((,class (:foreground ,green :weight bold))))
   `(org-headline-done ((,class (:foreground ,gray))))
   `(org-priority ((,class (:foreground ,yellow :weight bold))))
   `(org-tag ((,class (:foreground ,gray))))
   `(org-special-keyword ((,class (:foreground ,orange))))
   `(org-drawer ((,class (:foreground ,gray))))
   `(org-table ((,class (:foreground ,blue))))
   `(org-formula ((,class (:foreground ,yellow))))
   `(org-code ((,class (:foreground ,aqua))))
   `(org-verbatim ((,class (:foreground ,purple))))
   `(org-block ((,class (:background ,bg2 :foreground ,fg1 :extend t))))
   `(org-block-begin-line ((,class (:background ,bg2 :foreground ,gray :slant italic :extend t))))
   `(org-block-end-line ((,class (:background ,bg2 :foreground ,gray :slant italic :extend t))))
   `(org-meta-line ((,class (:foreground ,gray))))
   `(org-document-title ((,class (:foreground ,green :weight bold))))
   `(org-document-info ((,class (:foreground ,fg2))))
   `(org-document-info-keyword ((,class (:foreground ,gray))))
   `(org-link ((,class (:foreground ,blue :underline t))))
   `(org-footnote ((,class (:foreground ,aqua :underline t))))
   `(org-date ((,class (:foreground ,purple :underline t))))
   `(org-ellipsis ((,class (:foreground ,gray))))
   `(org-quote ((,class (:foreground ,gray :slant italic))))
   `(org-verse ((,class (:foreground ,gray))))
   `(org-checkbox ((,class (:foreground ,green :weight bold))))
   `(org-checkbox-statistics-todo ((,class (:foreground ,red))))
   `(org-checkbox-statistics-done ((,class (:foreground ,green))))
   `(org-warning ((,class (:foreground ,red :weight bold))))
   `(org-upcoming-deadline ((,class (:foreground ,orange))))
   `(org-scheduled ((,class (:foreground ,green))))
   `(org-scheduled-today ((,class (:foreground ,aqua))))
   `(org-agenda-structure ((,class (:foreground ,orange :weight bold))))
   `(org-agenda-date ((,class (:foreground ,blue))))
   `(org-agenda-date-weekend ((,class (:foreground ,fg4))))
   `(org-agenda-date-today ((,class (:foreground ,yellow :weight bold))))
   `(org-agenda-current-time ((,class (:foreground ,green))))
   `(org-agenda-done ((,class (:foreground ,green))))
   `(org-agenda-clocking ((,class (:background ,bg2))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,gray))))
   `(org-agenda-filter-tags ((,class (:foreground ,yellow :weight bold))))

   ;; -----------------------------------------------------------------
   ;; Outline / info headings
   ;; -----------------------------------------------------------------
   `(outline-1 ((,class (:foreground ,green :weight bold))))
   `(outline-2 ((,class (:foreground ,yellow :weight bold))))
   `(outline-3 ((,class (:foreground ,aqua :weight bold))))
   `(outline-4 ((,class (:foreground ,blue :weight bold))))
   `(outline-5 ((,class (:foreground ,purple :weight bold))))
   `(outline-6 ((,class (:foreground ,orange))))
   `(outline-7 ((,class (:foreground ,fg2))))
   `(outline-8 ((,class (:foreground ,gray))))
   `(info-title-1 ((,class (:foreground ,green :weight bold :height 1.2))))
   `(info-title-2 ((,class (:foreground ,yellow :weight bold :height 1.1))))
   `(info-title-3 ((,class (:foreground ,aqua :weight bold :height 1.05))))
   `(info-title-4 ((,class (:foreground ,blue :weight bold))))
   `(info-menu-header ((,class (:foreground ,orange :weight bold))))

   ;; -----------------------------------------------------------------
   ;; Markdown
   ;; -----------------------------------------------------------------
   `(markdown-header-face-1 ((,class (:foreground ,green :weight bold))))
   `(markdown-header-face-2 ((,class (:foreground ,green :weight bold))))
   `(markdown-header-face-3 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-4 ((,class (:foreground ,yellow :weight bold))))
   `(markdown-header-face-5 ((,class (:foreground ,yellow))))
   `(markdown-header-face-6 ((,class (:foreground ,yellow))))
   `(markdown-header-delimiter-face ((,class (:foreground ,orange))))
   `(markdown-bold-face ((,class (:foreground ,fg0 :weight bold))))
   `(markdown-italic-face ((,class (:foreground ,fg3 :slant italic))))
   `(markdown-link-face ((,class (:foreground ,purple :underline t))))
   `(markdown-url-face ((,class (:foreground ,blue))))
   `(markdown-code-face ((,class (:background ,bg2 :foreground ,aqua :extend t))))
   `(markdown-inline-code-face ((,class (:foreground ,aqua))))
   `(markdown-pre-face ((,class (:foreground ,aqua))))
   `(markdown-list-face ((,class (:foreground ,purple))))
   `(markdown-blockquote-face ((,class (:foreground ,gray))))
   `(markdown-hr-face ((,class (:foreground ,gray))))
   `(markdown-metadata-key-face ((,class (:foreground ,gray))))
   `(markdown-metadata-value-face ((,class (:foreground ,fg2))))
   `(markdown-html-tag-name-face ((,class (:foreground ,aqua :weight bold))))
   `(markdown-html-tag-delimiter-face ((,class (:foreground ,fg3))))
   `(markdown-html-attr-name-face ((,class (:foreground ,blue))))
   `(markdown-html-attr-value-face ((,class (:foreground ,green))))
   `(markdown-html-entity-face ((,class (:foreground ,orange))))
   `(markdown-reference-face ((,class (:foreground ,blue))))
   `(markdown-footnote-marker-face ((,class (:foreground ,orange))))
   `(markdown-missing-link-face ((,class (:foreground ,red :weight bold))))
   `(markdown-line-break-face ((,class (:foreground ,orange))))

   ;; -----------------------------------------------------------------
   ;; Shells -- netrw / vimshell / terminal colours
   ;; -----------------------------------------------------------------
   `(eshell-prompt ((,class (:foreground ,green :weight bold))))
   `(eshell-ls-directory ((,class (:foreground ,blue :weight bold))))
   `(eshell-ls-executable ((,class (:foreground ,green))))
   `(eshell-ls-symlink ((,class (:foreground ,aqua :slant italic))))
   `(eshell-ls-archive ((,class (:foreground ,purple))))
   `(eshell-ls-backup ((,class (:foreground ,gray))))
   `(eshell-ls-clutter ((,class (:foreground ,gray))))
   `(eshell-ls-missing ((,class (:foreground ,red))))
   `(eshell-ls-product ((,class (:foreground ,yellow))))
   `(eshell-ls-readonly ((,class (:foreground ,yellow))))
   `(eshell-ls-special ((,class (:foreground ,orange))))
   `(eshell-ls-unreadable ((,class (:foreground ,red))))

   `(term-color-black ((,class (:background ,ansi-black :foreground ,ansi-black))))
   `(term-color-red ((,class (:background ,ansi-red :foreground ,ansi-red))))
   `(term-color-green ((,class (:background ,ansi-green :foreground ,ansi-green))))
   `(term-color-yellow ((,class (:background ,ansi-yellow :foreground ,ansi-yellow))))
   `(term-color-blue ((,class (:background ,ansi-blue :foreground ,ansi-blue))))
   `(term-color-magenta ((,class (:background ,ansi-magenta :foreground ,ansi-magenta))))
   `(term-color-cyan ((,class (:background ,ansi-cyan :foreground ,ansi-cyan))))
   `(term-color-white ((,class (:background ,ansi-white :foreground ,ansi-white))))

   ;; The 16 colours `ansi-color' uses for SGR 30-37/40-47 and 90-97/100-107
   ;; (Emacs 28 and later).
   `(ansi-color-black ((,class (:foreground ,ansi-black :background ,ansi-black))))
   `(ansi-color-red ((,class (:foreground ,ansi-red :background ,ansi-red))))
   `(ansi-color-green ((,class (:foreground ,ansi-green :background ,ansi-green))))
   `(ansi-color-yellow ((,class (:foreground ,ansi-yellow :background ,ansi-yellow))))
   `(ansi-color-blue ((,class (:foreground ,ansi-blue :background ,ansi-blue))))
   `(ansi-color-magenta ((,class (:foreground ,ansi-magenta :background ,ansi-magenta))))
   `(ansi-color-cyan ((,class (:foreground ,ansi-cyan :background ,ansi-cyan))))
   `(ansi-color-white ((,class (:foreground ,ansi-white :background ,ansi-white))))
   `(ansi-color-bright-black ((,class (:foreground ,ansi-bright-black :background ,ansi-bright-black))))
   `(ansi-color-bright-red ((,class (:foreground ,ansi-bright-red :background ,ansi-bright-red))))
   `(ansi-color-bright-green ((,class (:foreground ,ansi-bright-green :background ,ansi-bright-green))))
   `(ansi-color-bright-yellow ((,class (:foreground ,ansi-bright-yellow :background ,ansi-bright-yellow))))
   `(ansi-color-bright-blue ((,class (:foreground ,ansi-bright-blue :background ,ansi-bright-blue))))
   `(ansi-color-bright-magenta ((,class (:foreground ,ansi-bright-magenta :background ,ansi-bright-magenta))))
   `(ansi-color-bright-cyan ((,class (:foreground ,ansi-bright-cyan :background ,ansi-bright-cyan))))
   `(ansi-color-bright-white ((,class (:foreground ,ansi-bright-white :background ,ansi-bright-white))))

   ;; -----------------------------------------------------------------
   ;; Compilation / grep
   ;; -----------------------------------------------------------------
   `(compilation-error ((,class (:foreground ,red :weight bold))))
   `(compilation-warning ((,class (:foreground ,yellow :weight bold))))
   `(compilation-info ((,class (:foreground ,green))))
   `(compilation-line-number ((,class (:foreground ,blue))))
   `(compilation-column-number ((,class (:foreground ,purple))))
   `(next-error ((,class (:background ,bg3))))

   ;; -----------------------------------------------------------------
   ;; Which-key, indent guides, whitespace
   ;; -----------------------------------------------------------------
   `(which-key-key-face ((,class (:foreground ,green :weight bold))))
   `(which-key-command-description-face ((,class (:foreground ,fg1))))
   `(which-key-group-description-face ((,class (:foreground ,orange))))
   `(which-key-separator-face ((,class (:foreground ,gray))))
   `(which-key-special-key-face ((,class (:foreground ,red :weight bold))))

   `(highlight-indent-guides-odd-face ((,class (:background ,bg2))))
   `(highlight-indent-guides-even-face ((,class (:background ,bg1))))

   `(whitespace-space ((,class (:foreground ,bg3 :background ,bg0))))
   `(whitespace-hspace ((,class (:foreground ,bg3 :background ,bg0))))
   `(whitespace-tab ((,class (:foreground ,bg3 :background ,bg0))))
   `(whitespace-newline ((,class (:foreground ,bg3 :background ,bg0))))
   `(whitespace-indentation ((,class (:foreground ,bg3 :background ,bg0))))
   `(whitespace-trailing ((,class (:foreground ,red :background ,bg1))))
   `(whitespace-line ((,class (:foreground ,red :background ,bg1))))
   `(whitespace-empty ((,class (:foreground ,red :background ,bg1))))

   ;; -----------------------------------------------------------------
   ;; Evil (the closest Emacs analogue of Vim's search and substitute)
   ;; -----------------------------------------------------------------
   `(evil-ex-search ((,class (:background ,yellow :foreground ,bg0))))
   `(evil-ex-lazy-highlight ((,class (:background ,bg3 :foreground ,fg0))))
   `(evil-ex-substitute-matches ((,class (:background ,red :foreground ,bg0))))
   `(evil-ex-substitute-replacement ((,class (:background ,green :foreground ,bg0))))
   `(evil-search-highlight-persist-highlight-face ((,class (:background ,yellow :foreground ,bg0)))))

  ;; 8-color palette from deus.json, verbatim (the first eight entries of its
  ;; "color" array; the bright half is applied through the `ansi-color-*'
  ;; faces above).  The vector must have exactly 8 entries: `ansi-color' maps
  ;; it onto SGR 30-37 and 40-47 of a 50-element map, so a longer vector
  ;; signals `args-out-of-range' when `ansi-color' is loaded.
  (custom-theme-set-variables
   'deus
   `(ansi-color-names-vector
     [,ansi-black ,ansi-red ,ansi-green ,ansi-yellow
      ,ansi-blue ,ansi-magenta ,ansi-cyan ,ansi-white])))

;;; Plain-text colour: documents white, code pale yellow

(require 'face-remap)

;; `default' is #d2d2d2 (the old-deus white) so that prose, org, markdown,
;; info and help buffers read as documents.  Programming buffers get the
;; Vim Normal foreground #ebdbb2 back through a buffer-local face remap,
;; installed from `prog-mode-hook' while this theme is enabled.  A face
;; spec cannot distinguish "plain text in prog-mode" from "plain text in
;; text-mode", hence the small amount of code below.
(defconst deus-theme--prog-foreground "#ebdbb2"
  "Plain-text colour in programming buffers (colors/deus.vim's ds.light1).")

(defvar-local deus-theme--prog-remap nil
  "Cookie of the buffer-local `default' remap, or nil.")

(defun deus-theme--prog-remap-add ()
  "Give the current programming buffer the pale-yellow plain-text colour."
  (unless deus-theme--prog-remap
    (setq deus-theme--prog-remap
          (face-remap-add-relative 'default
                                   :foreground deus-theme--prog-foreground))))

(defun deus-theme--prog-remap-remove ()
  "Undo `deus-theme--prog-remap-add' in the current buffer."
  (when deus-theme--prog-remap
    (face-remap-remove-relative deus-theme--prog-remap)
    (setq deus-theme--prog-remap nil)))

(defun deus-theme--enable (theme)
  "Install the code/document plain-text split when THEME is `deus'."
  (when (eq theme 'deus)
    (add-hook 'prog-mode-hook #'deus-theme--prog-remap-add)
    (add-hook 'change-major-mode-hook #'deus-theme--prog-remap-remove)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (derived-mode-p 'prog-mode)
          (deus-theme--prog-remap-add))))))

(defun deus-theme--disable (theme)
  "Remove the code/document plain-text split when THEME is `deus'."
  (when (eq theme 'deus)
    (remove-hook 'prog-mode-hook #'deus-theme--prog-remap-add)
    (remove-hook 'change-major-mode-hook #'deus-theme--prog-remap-remove)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (deus-theme--prog-remap-remove)))))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'deus-theme--enable)
  (add-hook 'disable-theme-functions #'deus-theme--disable))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'deus)

;;; deus-theme.el ends here
