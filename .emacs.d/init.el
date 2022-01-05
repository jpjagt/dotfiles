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
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu"          . "https://elpa.gnu.org/packages/")
                         ;; ("melpa"     . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                         ;; melpa.org/packages hangs (see https://emacs.stackexchange.com/questions/37353/can-not-access-melpa-packages-hung-up-at-contacting-host-elpa-gnu-org80)
                         ;; ("melpa"        . "https://stable.melpa.org/packages/")
                         ;; ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ;; ("marmalade"    . "https://marmalade-repo.org/packages/")
                         ("org"          . "http://orgmode.org/elpa/")
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

;; from protesilaos/dotfiles:
;; I create an "el" version of my Org configuration file as a final step
;; before closing down Emacs.  This is done to load the latest version
;; of my code upon startup.
;;
;; Also helps with initialisation times.  Not that I care too much about
;; those… Hence why I no longer bother with deferring package loading
;; either by default or on a case-by-case basis.
;;
;; jpj: currently UNUSED in favour of load-org below.
(defun load-config-org-or-el (fname)
  (let* ((conf (concat user-emacs-directory fname))
         (el (concat conf ".el"))
         (org (concat conf ".org")))
    (if (file-exists-p el)
        (load-file el)
      (use-package org-mode :straight (:type built-in))
      (org-babel-load-file org))))

(defun load-org (f)
  (org-babel-load-file (concat user-emacs-directory f)))

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; full path of buffer in mode-line
(setq uniquify-buffer-name-style 'forward)

(load-org "jpj.org")

;; gc - decrease threshold to 5 MB
;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
