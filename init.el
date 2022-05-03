;;;;; SpringHan's Emacs Configuration
;;; Mirror Config
(require 'package)
(require 'comp)
(require 'cl-lib) ; Common Lisp
;;; Package Require
(setq package-archives '(("gnu" . "http://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "http://mirrors.ustc.edu.cn/elpa/melpa/")))

;;; Native comp
(setq native-comp-async-report-warnings-errors nil)
(add-to-list 'load-path "~/.emacs.d/third-party/gpack")
(require 'gpack)

;;; Theme
(gpack my-themes
  :load-path ("~/.emacs.d/themes" . custom-theme-load-path)
  :un-require)

;; GC
(setq gc-cons-threshold (* 50 1024 1024))

;;; Variables
(defvar spring/time-block nil
  "If the the time-block changed, it is t.
Otherwise it's nil.")

(defvar spring/unwanted-buffer
  '("*dashboard*" "notes.org" "tasks.org" "user-init.el" "*Help*" "*Backtrace*" "*Compile-Log*"
    "TAGS" "*lsp-log*" "*vc*" "*Calendar*" "*cargo-run*")
  "The buffers that I don't need.")

(defvar spring/my-child-frame nil
  "My child frame.")

(defvar spring/extra-native-compile-items nil
  "The items need extra natively compilation.")

(defvar spring/extra-items-compiled nil
  "If the extra items compiled.")

(load-file "~/.emacs.d/etc/init-config.el")

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
