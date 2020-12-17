;;; -*- lexical-binding: t; -*-
;; temp
;; (load-theme 'deeper-blue')
;; ;; Increase gc to 500MB for quick & easy startup
(setq gc-cons-threshold (* 10000 1024 1024))

;; https://emacs.stackexchange.com/questions/34342/is-there-any-downside-to-setting-gc-cons-threshold-very-high-and-collecting-ga
;; GC when idling. Also see below.
(run-with-idle-timer 30 t (lambda () (garbage-collect)))

(require 'package)
(dolist (arch '((("melpa" . "http://melpa.org/packages/")
                 ("gnu"          . "https://elpa.gnu.org/packages/")
                 ("melpa"     . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
                 ;; melpa.org/packages hangs (see https://emacs.stackexchange.com/questions/37353/can-not-access-melpa-packages-hung-up-at-contacting-host-elpa-gnu-org80)
                ("melpa"        . "https://stable.melpa.org/packages/")
                ("melpa"     . "http://melpa.milkbox.net/packages/")
                ("melpa-stable" . "https://stable.melpa.org/packages/")
                ("marmalade"    . "https://marmalade-repo.org/packages/")
                ("org"          . "http://orgmode.org/elpa/")
                 )))
  (add-to-list 'package-archives arch))

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

;; Use git version of use-package
(add-to-list 'load-path "~/.emacs.d/elpa/use-package")
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents 'async)
;;   (package-install 'use-package))

;; From use-package Readme
(eval-when-compile
  (require 'use-package))
;; (require 'diminish "/home/paul/.emacs.d/elpa/diminish.el") ; if you  use :diminish
;; (require 'bind-key) ; if you use any :bind variant

;; Load config.org - my Emacs configuration
(org-babel-load-file (concat user-emacs-directory "README.org"))

;; ;; ;; gc - decrease threshold to 5 MB
;; ;; (add-hook 'after-init-hook (lambda () (setq gc-cons-threshold (* 5 1024 1024))))

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(package-selected-packages (quote (wrap- use-package org-edna blacken auctex))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;; (setq org-indented-mode nil)

;; ;;; prettify-utils.el --- Helper functions for prettify-symbols-mode			-*- lexical-binding: t; -*-

;; ;; Copyright © 2016 Ilazki

;; ;; Author:              Ilazki
;; ;; Created:             31 Oct 2016
;; ;; URL:                 https://github.com/Ilazki/prettify-utils.el
;; ;; Keywords:            lisp tools prettify utils prettify-symbols
;; ;; Version:             1.0.2
;; ;; Package-Version:     20161110.0430
;; ;; Package-Requires:    ((emacs "24.4"))

;; ;; This file is not part of GNU Emacs.

;; ;;; Commentary:

;; ;; Prettify-utils provides helper functions to simplify configuration of emacs'
;; ;; prettify-symbols-mode.  For the unfamiliar, prettify-symbols-mode detects
;; ;; user-defined character combinations and replaces them with different ones
;; ;; (usually ligatures) on-screen without changing the source file.  For example,
;; ;; it can be used to display "≥" any time ">=" is found.
;; ;;
;; ;; Unfortunately, setup of prettify-symbols-mode is more complicated than it
;; ;; needs to be, especially if you want the replacement string to be multiple
;; ;; characters instead of just one.  For example, displaying "→ " for "->" in
;; ;; order to improve appearance while still preserving the real  character length.
;; ;;
;; ;; To use prettify-symbols, you have to set a buffer-local variable,
;; ;; prettify-symbols-alist, to an alist containing string/replacement pairs.  This
;; ;; package provides a helper macro, prettify-utils-generate, to create the alist
;; ;; in a simple, uniform manner, like so:
;; ;;
;; ;;	(setq prettify-symbols-alist
;; ;;		  (prettify-utils-generate
;; ;;		   ("lambda" "λ")
;; ;;		   ("<="     "≤")
;; ;;		   (">="     "≥")
;; ;;		   ("->"     "→ ")))
;; ;;
;; ;;
;; ;; Since prettify-symbols uses buffer-local settings, you have to add the alist
;; ;; for each buffer.  The easiest way to do this is to add it to a mode's hooks.
;; ;; Example:
;; ;;
;; ;;	(add-hook 'prog-mode-hook (lambda ()
;; ;;								(setq prettify-symbols-alist
;; ;;								  (prettify-utils-generate
;; ;;								   ("lambda" "λ")))
;; ;;                              (prettify-symbols-mode 1)))
;; ;;
;; ;; For convenience, a macro named prettify-utils-add-hook is also available
;; ;; that generates the above automatically.
;; ;; Example:
;; ;;
;; ;; (prettify-utils-add-hook prog-mode-hook
;; ;;                          ("lambda" "λ")))
;; ;;
;; ;; Or, if you're using the same alist for multiple modes, you can create a named
;; ;; function and pass that to the mode hooks:
;; ;;
;; ;;	(defun my-symbols-alist ()
;; ;;		 (setq prettify-symbols-alist
;; ;;			(prettify-utils-generate
;; ;;			 ("lambda" "λ")
;; ;;			 ("->" "→")
;; ;;			 ("=>" "⇒"))))
;; ;;	(add-hook 'prog-mode-hook 'my-symbols-alist)
;; ;;
;; ;; Generally, prettify-utils-generate and prettify-utils-add-hook should be the
;; ;; only things needed, but some additional helpers are provided in case a need
;; ;; arises:
;; ;;
;; ;; * prettify-utils-generate-f is the function equivalent of using
;; ;;   prettify-utils-generate.  Should only be needed for creating higher-order
;; ;;   functions.
;; ;; * prettify-utils-create-pair is the function used by other functions to create
;; ;;   alist pairs.
;; ;; * prettify-utils-string converts a string into the list format required by
;; ;;   prettify-symbols-alist.
;; ;;
;; ;; For more information about any functions or macros provided, as well as
;; ;; additional example use, refer to any function's docstring with
;; ;; `M-x describe-function`.

;; ;;; Code:

;; (provide 'prettify-utils)

;; ;;;###autoload
;; (defun prettify-utils--list (l &optional glue)
;;   "Takes two lists and interleaves the (optional) second between each element of
;; the first.  Used to create multi-character sequences for use with the minor mode
;; `prettify-symbols-mode'.  If not supplied, GLUE defaults to '(Br . Bl).  For more
;; information about GLUE, refer to the documentation for the `compose-region'
;; function and the `reference-point-alist' variable.

;; This function is used by `prettify-utils-string' to create the lists given to
;; `prettify-symbols-alist'.  Calling prettify-utils--list directly is probably not
;; what you want, check the documentation for `prettify-utils-string' and
;; `prettify-utils-generate' instead.

;; Example use:
;; (prettify-utils--list (string-to-list \"hello\") '(Br . Bl))
;; "

;;   (let ((glue (or glue '(Br . Bl)))
;;                 (head (car l))
;;                 (tail (cdr l)))
;;         (cond
;;          ((not (consp l))    '())
;;          ((not (consp tail))  (list head))
;;          (t (cons head
;;                           (cons glue
;;                                         (prettify-utils--list tail glue)))))))

;; ;;;###autoload
;; (defun prettify-utils-string (s &optional glue)
;;   "Takes a string and an optional list, and returns a list of the string's
;; characters with GLUE interleaved between each character, for use with
;; `prettify-symbols-mode'.  If no GLUE is supplied, uses the
;; `prettify-utils--list' default.  For more information about GLUE, refer to the
;; documentation for the `compose-region' function and the `reference-point-alist'
;; variable.

;; This function can be used to simplify multiple-character replacements when
;; manually constructing a `prettify-symbols-alist'.  For something more high-level,
;; consider using `prettify-utils-generate' to create the entire alist instead.

;; Example:

;; (prettify-utils-string \"example\" '(Br . Bl))
;; "
;;   (prettify-utils--list (append s nil) glue))

;; ;; Was used during macro creation then removed
;; (defun prettify-utils-create-pair (old new &optional glue)
;;   "Takes two strings, OLD and NEW, and an optional GLUE list, and creates an
;; alist pair for use when creating a `prettify-symbols-alist'.  For more information
;; about GLUE, refer to the documentation for the `compose-region' function and the
;; `reference-point-alist' variable.

;; This function is primarily for use by the user-friendly `prettify-utils-generate'
;; macro, but may be useful if manual alist creation is desired for some reason.

;; Example:

;; (setq prettify-symbols-alist `((\">=\" ?≥)
;;           ,(prettify-utils-create-pair \"foo\" \"bar\" '(Br . Bl))))
;; "
;;   (cons old (prettify-utils-string new glue)))

;; ;;;###autoload
;; (defmacro prettify-utils-generate (&rest lists)
;;   "Generates an alist for use when setting `prettify-symbols-alist'.  Takes one or
;; more LISTS, each consisting of two strings and an optional GLUE list to be
;; interleaved between characters in the replacement list.  If the optional GLUE
;; list is not supplied, uses the `prettify-utils--list' default of '(Br . Bl).  For more
;; information about GLUE, refer to the documentation for the `compose-region'
;; function and the `reference-point-alist' variable.

;; Example #1:

;; (setq prettify-symbols-alist
;;           (prettify-utils-generate (\"foo\" \"bar\")
;;                                                            (\">=\" \"≥\" (Br . Bl))
;;                                                            (\"->\"     \"→ \")))

;; Example #2:

;; (setq prettify-symbols-alist
;;           (prettify-utils-generate
;;            (\"lambda\"  \"λ\")
;;            (\"|>\"      \"▷\")
;;            (\"<|\"      \"◁\")
;;            (\"->>\"     \"↠  \")
;;            (\"->\"      \"→ \")
;;            (\"<-\"      \"← \")
;;            (\"=>\"      \"⇒\")
;;            (\"<=\"      \"≤\")
;;            (\">=\"      \"≥\")))
;; "
;;   (let* ((head       (car   lists))
;;          (tail       (cdr   lists))
;;          (old-string (car   head))
;;                  (new-string (cadr  head))
;;                  (glue-list  (caddr head)))
;;         (if (not (consp head))
;;                 '()
;;        `(cons (quote ,(prettify-utils-create-pair old-string new-string glue-list))
;;                          (prettify-utils-generate ,@tail)))))


;; ;;;###autoload
;; (defun prettify-utils-generate-f (&rest lists)
;;   "Generates an alist for use when setting `prettify-symbols-alist'.  Takes one or
;; more LISTS, each consisting of two strings and an optional GLUE list to be
;; interleaved between characters in the replacement list.  If the optional GLUE
;; list is not supplied, uses the `prettify-utils--list' default of '(Br . Bl).  For more
;; information about GLUE, refer to the documentation for the `compose-region'
;; function and the `reference-point-alist' variable.

;; This is a function equivalent of the `prettify-utils-generate' macro.  Unless
;; you specifically need a function, such as for use with a higher-order function,
;; you should use the `prettify-utils-generate' macro instead.

;; Example:

;; (prettify-utils-generate-f '(\"foo\" \"bar\")
;;                                            '(\">=\" \"≥\" (Br . Bl))
;;                                                    '(\"->\"     \"→ \"))
;; "
;;   (let* ((head       (car   lists))
;;          (tail       (cdr   lists))
;;          (old-string (car   head))
;;                  (new-string (cadr  head))
;;                  (glue-list  (caddr head)))
;;   (if (not (consp head))
;;           '()
;;       (cons (prettify-utils-create-pair old-string new-string glue-list)
;;                    (apply 'prettify-utils-generate-f tail)))))

;; ;; Macro based on a suggestion and example code by reddit user /u/Kungsgeten
;; ;; Github @ https://github.com/Kungsgeten/
;; ;;;###autoload
;; ;; (defmacro prettify-utils-add-hook (mode &rest lists)
;; ;;   "Convenience macro for the most likely use case of prettify-utils: using
;; ;; `add-hook' to add LISTS to MODE. LISTS consists of one or more lists of
;; ;; replacements, defined as expected by `prettify-utils-generate'.

;; ;; Example:

;; ;; ;; Replace org-mode checkboxes with appropriate unicode boxes
;; ;; (prettify-utils-add-hook org-mode
;; ;;                          (\"[ ]\" \"☐\")
;; ;;                          (\"[X]\" \"☑\")
;; ;;                          (\"[-]\" \"❍\"))

;; ;; "
;; ;;   `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
;; ;;              (lambda ()
;; ;;                (setq prettify-symbols-alist
;; ;;                      (prettify-utils-generate ,@lists))
;; ;;                (prettify-symbols-mode 1))))

;; ;; ;;; prettify-utils.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-ispell-fuzzy-limit 4)
 '(ac-ispell-requires 4)
 '(counsel-mode t)
 '(custom-safe-themes
   (quote
    ("3f5f69bfa958dcf04066ab2661eb2698252c0e40b8e61104e3162e341cee1eb9" "672bb062b9c92e62d7c370897b131729c3f7fd8e8de71fc00d70c5081c80048c" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" default)))
 '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
 '(help-at-pt-timer-delay 0.5)
 '(ivy-mode t)
 ;; '(js-auto-format-command "prettier")
 ;; '(js-auto-format-command-args "--write --single-quote --no-semi")
 '(package-selected-packages
   (quote
    (exwm tide rjsx-mode scss-mode js-format htmlize smooth-scroll smooth-scrolling slack neotree ts org-mime oauth2 auctex-lua request auctex srefactor emojify-logos call-graph indium wiki-summary simple-call-tree google-this undo-tree smartparens swoop tree-mode magit swiper-helm counsel-tramp tramp-term use-package)))
 '(tab-width 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
