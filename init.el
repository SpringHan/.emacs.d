;;;;; SpringHan's Emacs Configuration
;;; Mirror Config
(require 'package)
(require 'cl-lib) ; Common Lisp
;;; Package Require
(setq package-archives '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
												 ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(add-to-list 'load-path "~/.emacs.d/third-party/package-require")
(require 'package-require)

;;; Theme
(package-require atom-one-themes
	:load-path ("~/.emacs.d/themes" . custom-theme-load-path)
	:un-require)

;; GC
(setq gc-cons-threshold (* 50 1024 1024))

;;; Variables
(defvar spring/time-block nil
	"If the the time-block changed, it is t.
Otherwise it's nil.")
(defvar spring/unwanted-buffer
	'("*dashboard*" "notes.org" "tasks.org" "user-init.el" "*Help*" "*Backtrace*")
	"The buffers that I don't need.")

(load-file "~/.emacs.d/etc/init-config.el")

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
