(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)
(toggle-frame-fullscreen)
(auto-fill-mode)
(which-key-mode)

;; File for custom-set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq-default inhibit-startup-screen t
              inhibit-splash-screen t
              initial-scratch-message "")

(defun load-org (f)

  (org-babel-load-file (concat user-emacs-directory f)))


(load-org "pauls_functions.org")

(load-org "functions.org")

(load-org "appearance.org")

;; (setq frame-title-format
;; '("emacs: " (:eval (if (buffer-file-name)
;;            (abbreviate-file-name (buffer-file-name)) "%b"))))

(show-paren-mode t)
(setq-default show-paren-style 'parenthesis) ; highlight brackets only

(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in
the echo area. Has no effect if the character before point is not
of the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
   (matching-text (and cb
           (char-equal (char-syntax cb) ?\) )
           (blink-matching-open))))
    (when matching-text (message matching-text))))

;; (savehist-mode 1)

;; ;; Lazy prompting. Change "yes or no" to "y or n"
;; ;; http://dl.dropboxusercontent.com/u/3968124/sacha-emacs.html
(fset 'yes-or-no-p 'y-or-n-p)

(setq-default
fill-column 79
standard-indent 2

 ;;  ;; comment for now
;;  ;; tab-always-indent 'complete
;;  ;; indent-tabs-mode nil

;;  sentence-end-double-space nil
;;  indicate-empty-lines t

;;  european-calendar-style t
;;  calendar-date-style 'european
;;  calendar-week-start-day 1
;;  diary-file "~/.diary"

;;  display-time-24hr-format t
;;  display-time-day-and-date t
;;  display-time-string-forms
;;  '((if (and (not display-time-format) display-time-day-and-date)
;;  (format-time-string "%a %b %e " now) "")
;;    (format-time-string (or display-time-format
;;          (if display-time-24hr-format "%H:%M" "%-I:%M%p"))
;;      now))
;;  calendar-time-display-form
;;  '(24-hours ":" minutes
;;       (if time-zone " (") time-zone (if time-zone ")"))


;;  delete-old-versions t
;;  vc-make-backup-files t
;;  backup-directory-alist '((".*" . "~/.emacs.d/emacs-backups"))

;;  mail-user-agent 'gnus-user-agent
    visible-bell t
;;  ps-paper-type 'a4

;;  ediff-split-window-function 'split-window-horizontally
;;  ediff-window-setup-function 'ediff-setup-windows-plain

 enable-recursive-minibuffers t
;;  debug-on-error nil

;;  compile-command "remake install"

;;  aurel-download-directory "~/code/src/aur"

;;  async-shell-command-buffer 'new-buffer ; Don't ask.
;;  display-buffer-alist '(("*Async Shell Command*" . (display-buffer-no-window)))
;;  ibuffer-use-other-window nil
;;  Man-width 80
;;  Man-notify-method 'aggressive

;;  eshell-prompt-function #'(lambda () "$ ")
 )

;; ;; (add-to-list 'ibuffer-never-show-predicates "^\*Async")

;; (dolist (mode '(scroll-bar-mode))
;;   (if (fboundp mode) (funcall mode -1)))

;; ;; Edit from  chrome
;; ;; (edit-server-start)

;; ;; Save point position between sessions
;; ;; (use-package saveplace
;; ;;   :ensure t
;; ;;   :config
;; ;;   (setq-default save-place t)
;; ;;   (setq save-place-file (expand-file-name ".places" user-emacs-directory)))

;; (define-minor-mode atend-mode
;;   "The sole purpose is to move to the end of the buffer as soon
;; as the file is visited. This is useful for log files and such
;; where we are immediately more interested in the bottom than the
;; top."
;;   ;; The initial value.
;;   nil
;;   ;; The indicator for the mode line.
;;   ""
;;   ;; The minor mode bindings.
;;   '()
;;   :group 'atend
;;   (goto-char (point-max)))

;; (global-set-key (kbd "C-c 3") 'browse-url-at-point)
(global-set-key (kbd "C-c b") 'bury-buffer)
;; (global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c t") 'tramp-cleanup-this-connection)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k") `keybase-open-chat)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))
;; (use-package expand-region
;;   :bind ("C-=" . er/expand-region))

(load-org "files.org")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; ;; Full path of buffer in mode-line
(setq uniquify-buffer-name-style 'forward)

(load-org "windows.org")

;; (load-org "packages.org")
(load-org "python.org")
(load-org "js.org")
(load-org "etc.org")
(load-org "org.org")
;; (load-org "email.org")
(load-org "latex.org")
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode)

(load-org "pauls_functions.org")

;; (load-org "personal.org")

;; (load-org "custom_commands.org")

;; (defun go-to-function (identifier)
;;   "Go to function (re-generate file tags file everytime"
;;   (interactive (list (xref--read-identifier "Find definitions of: ")))
;;   (shell-command "find ~/projects/ -name '*py' | xargs etags -o ~/check_tags")
;;   (setq tags-file-name "~/check_tags")
;;   (xref-find-definitions identifier))
;;   ;; ;; (interactive)
;;   ;; (go-to-function identifier nil))

;;   (global-set-key (kbd "M-.") `go-to-function)

;; (load-org "pauls_functions.org")
;; (load-org "personal.org")
;;   ;;     (load-org "js.org")
;;   (use-package tramp
;;     :init (setq tramp-ssh-controlmaster-options
;;     "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no"))
;;     ;; (defvar TeX-command-default "LaTeX"
;;     ;;   "The default command for `TeX-command' in the current major mode.")
;;     ;; ;; (setq TeX-command-default )

;;     ;; (defun TeX-command-master (&optional override-confirm)
;;     ;;       "Run command on the current document.

;;     ;;     If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
;;     ;;     depend on it being positive instead of the entry in `TeX-command-list'."
;;     ;;       (interactive "P")
;;     ;;       ;; (save-buffer); edits won't automatically show otherwise
;;     ;;       (TeX-command TeX-command-default              ;
;;     ;;                    'TeX-master-file nil))
;;     (load-org "general.org")

(setq debug-on-error nil)

(load-org "ssh.org")

(load-org "pim.org")

(defun jupyter-insert-token ()
  (interactive)
  (let ((output (shell-command-to-string "jupyter notebook list")))
        (insert (first (split-string (second (split-string output "token=")) " " )))
))

(which-function-mode)

(setq explicit-shell-file-name "/bin/zsh")

(load-org "drag-stuff.org")
(load-org "jeroen.org")
