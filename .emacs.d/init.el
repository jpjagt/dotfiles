;;; -*- lexical-binding: t; -*-
;; temp
;; ;; Increase gc to 500MB for quick & easy startup
(setq gc-cons-threshold (* 10000 1024 1024))

;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; GC when idling. Also see below.
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

;; package manager
(require 'package)
;; configure package sources
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; melpa.org/packages hangs (see https://emacs.stackexchange.com/questions/37353/can-not-access-melpa-packages-hung-up-at-contacting-host-elpa-gnu-org80)
                         ("melpa"        . "https://stable.melpa.org/packages/")
                         ;; ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ;; ("org"          . "http://orgmode.org/elpa/")
                         ))

(package-initialize)

;; set environment from shell
;; you may first need to do: M-x package-install RET exec-path-from-shell RET
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Use git version of use-package
(add-to-list 'load-path "~/.emacs.d/elpa/use-package")

;; From use-package Readme
(eval-when-compile
  (require 'use-package))

;; add straight as package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; from protesilaos/dotfiles
;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs.  This is done to load the latest version
;; of my code upon startup.
;;
;; Also helps with initialisation times.  Not that I care too much about
;; those… Hence why I no longer bother with deferring package loading
;; either by default or on a case-by-case basis.
(defun load-config-org-or-el (fname)
  (let* ((conf (concat user-emacs-directory fname))
         (el (concat conf ".el"))
         (org (concat conf ".org")))
    (if (file-exists-p el)
        (load-file el)
      (use-package org-mode :straight (:type built-in))
      (org-babel-load-file org))))

(load-config-org-or-el "README")

;; ;; gc - decrease threshold to 5 MB
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(ac-ispell-fuzzy-limit 4)
;;  '(ac-ispell-requires 4)
;;  '(counsel-mode t)
;;  '(custom-safe-themes
;;    (quote
;;     ("3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
;;  '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
;;  '(help-at-pt-timer-delay 0.5)
;;  '(ivy-mode t)
;;  ;; '(js-auto-format-command "prettier")
;;  ;; '(js-auto-format-command-args "--write --single-quote --no-semi")
;;  '(package-selected-packages
;;    (quote
;;     (exwm tide rjsx-mode scss-mode js-format htmlize smooth-scroll smooth-scrolling slack neotree ts org-mime oauth2 auctex-lua request auctex srefactor emojify-logos call-graph indium wiki-summary simple-call-tree google-this undo-tree smartparens swoop tree-mode magit swiper-helm counsel-tramp tramp-term use-package)))
;;  '(tab-width 2))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
