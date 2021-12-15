(use-package exec-path-from-shell
  :straight t
  )

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq debug-on-error nil)

(setq-default inhibit-startup-screen t
              inhibit-splash-screen t
              initial-scratch-message "")

(toggle-frame-fullscreen)
(toggle-frame-fullscreen)

;; File for custom-set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(defun jpj ()
  (interactive)
  (find-file "~/.emacs.d/jpj.org")
  )

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'super)

(setq visible-bell t)

(setq next-line-add-newlines t)

(defun define-named-lambda (name lambd args)
  (defalias (intern name) `(lambda () (interactive) (apply ,lambd ',args))))

(defun define-custom-function (name func)
  (define-named-lambda name (lambda () (funcall func))))

(bind-key* "s-u"  'revert-buffer)

(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

(global-set-key (kbd "C-`") 'push-mark-no-activate)

(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

;; (defun exchange-point-and-mark-no-activate ()
;;   "Identical to \\[exchange-point-and-mark] but will not activate the region."
;;   (interactive)
;;   (exchange-point-and-mark)
;;   (deactivate-mark nil))
;; (define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(global-set-key "\C-ceu" 'unfill-paragraph)

(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(global-set-key (kbd "M-<backspace>") `backward-delete-word)

(defun kill-word (arg)
  "Kill characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun my-delete-surrounded-delimiters ()
  (interactive)
  ;; save where region begins & ends
  (let ((beginning (region-beginning))
        (end (region-end)))
        (save-excursion
        (goto-char end)
        (delete-char -1)
        (goto-char beginning)
        (delete-char 1))))

(defun my-move-region-inwards ()
  (interactive)
  ;; save where region begins & ends
  (let ((beginning (region-beginning))
        (end (region-end)))
    (save-excursion
      (goto-char (+ beginning 1))
      (set-mark)
      (goto-char (- end 1))
      (goto-char (+ beginning 1))
          ;; (delete-char -1)

          ;; (goto-char beginning)
          ;; (delete-char 1)
          )))

(global-set-key (kbd "C-M-<backspace>") `my-delete-surrounded-delimiters)
(global-set-key (kbd "C-M-r") `my-move-region-inwards)

(defun regexp-orrify (&rest disjuncts)
  "Return the regexp disjunction of the given regexps"
  (cond ((null disjuncts) "")
        ((null (cdr disjuncts)) (car disjuncts))
        (t (concat (car disjuncts)
                   "\\|"
                   (apply #'regexp-orrify (cdr disjuncts))))))

(use-package s
  :straight t)

(defun eval-region-and-insert ()
  (interactive)
  (let ((currbuf (get-buffer (or (buffer-file-name) (buffer-name)))))
    (eval-region (region-beginning) (region-end) currbuf)
                        ))

;; Enable pretty syntax highlighting everywhere
(global-font-lock-mode t)

(use-package whitespace
  :ensure t
  :init (setq-default indicate-empty-lines t)
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face trailing lines-tail empty
                                indentation::space space-before-tab::tab))
  (global-whitespace-mode -1))

(setq-default indent-tabs-mode nil)

(defcustom do-whitespace-cleanup t
  "Perform whitespace-cleanup on save."
  :group 'whitespace)

(make-variable-buffer-local 'do-whitespace-cleanup)

(defun toggle-whitespace-cleanup ()
  "Turn the whitespace-cleanup hook on and off."
  (interactive)
  (setq do-whitespace-cleanup (not do-whitespace-cleanup))
  (message "do-whitespace-cleanup set to %s" do-whitespace-cleanup))

(add-hook 'before-save-hook
          (lambda ()
            (when do-whitespace-cleanup
              (whitespace-cleanup))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  do-whitespace-cleanup nil)))

(add-hook 'prog-mode-hook
          (lambda ()
            (whitespace-mode +1)
            ;; (setq show-trailing-whitespace t)
            ))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-all-mode 0)
(scroll-bar-mode 0)
(tooltip-mode )

(load-org "prettify.org")

;;; doom-palenight-theme.el --- inspired by Material-PaleNight -*- no-byte-compile: t; -*-
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(use-package doom-themes
  :ensure t
  :config (load-theme 'doom-palenight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(defun light ()
  (interactive)
  (load-theme 'doom-opera-light t))

(defun gray ()
  (interactive)
  (load-theme 'doom-nova t))

(defun dark ()
  (interactive)
  (load-theme 'doom-palenight t))

(dark)

(set-face-attribute 'region nil :background (doom-darken "#c792ea" 0.6) :foreground nil) ;; "#ffffff")

(global-hl-line-mode 0)
(set-face-background 'hl-line (doom-darken "#c792ea" 0.77))

;; (add-to-list 'load-path "~/.emacs.d/repos/elegant-emacs")
;; (require 'sanity)
;; ;; (require 'elegance)

(set-face-font 'default "DM Mono 13")

;; ;; (set-frame-parameter (selected-frame)
;; ;;                      'internal-border-width 24)
(setq default-frame-alist
      (append (list '(vertical-scroll-bars . nil)
                    ;; '(internal-border-width . 24)
                    '(font . "DM Mono 13"))))


;; ;; Line spacing, can be 0 for code and 1 or 2 for text
;; (setq-default line-spacing 0)

;; ;; Underline line at descent position, not baseline position
;; (setq x-underline-at-descent-line t)

;; ;; No ugly button for checkboxes
;; (setq widget-image-enable nil)

;; ;; No sound
;; (setq visible-bell t)
;; (setq ring-bell-function 'ignore)

;; ;; Paren mode is part of the theme
;; (show-paren-mode t)

;; ;; this is a purposefully long line that I hope will show some things in the fringe
;; ;; (fringe-mode '(10 . 10))
;; (defface fallback '((t :family "Fira Code Light"
;;                        :inherit 'face-faded)) "Fallback")
;; (set-display-table-slot standard-display-table 'truncation
;;                         (make-glyph-code ?… 'fallback))
;; (set-display-table-slot standard-display-table 'wrap
;;                         (make-glyph-code ?↩ 'fallback))

;; ;; Vertical window divider
;; (setq window-divider-default-right-width 3)
;; (setq window-divider-default-places 'right-only)
;; (window-divider-mode)

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

(defun wrap-region-in-text (prefix suffix)
  (interactive)
  (save-excursion
    (goto-char (region-beginning))
    (insert prefix))
  (save-excursion
    (goto-char (region-end))
    (insert suffix)))

(defun wrap-region-in-component (component)
  (wrap-region-in-text (s-concat "<" component ">") (s-concat "</" component ">")))

(defun wrap-region-in-trans ()
  (interactive)
  (wrap-region-in-component "Trans"))

(add-to-list 'load-path "~/.emacs.d/repos/protesilaos/dotfiles/emacs/.emacs.d/straight/repos/prot-lisp/")

(use-package prot-orderless
  :demand
  :config
  (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-literal
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-flex))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp)))

(use-package orderless
  :ensure t
  :demand
  :after prot-orderless
  :config
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher))
  ;; SPC should never complete: use it for `orderless' groups.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))

(use-package selectrum
  :straight t
  :config
  (set-face-attribute 'selectrum-current-candidate nil :background (doom-darken "#c792ea" 0.6))
  (selectrum-mode +1)
)

;; to make sorting and filtering more intelligent
(straight-use-package 'selectrum-prescient)
(selectrum-prescient-mode +1)

(use-package embark
  ;; Note that this gets only the main library.  That repo contains
  ;; other packages as well (which are small *.el files that are
  ;; distributed separately).
  :straight (embark :host github
                    :repo "oantolin/embark"
                    :branch "master"
                    :files ("embark.el"))
  :demand
  :diminish embark-collect-zebra-minor-mode
  :after prot-minibuffer
  :config
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))
  (setq embark-collect-live-update-delay 0.5)
  (setq embark-collect-live-initial-delay 0.8)

  ;; Please don't read too much into the names of those faces.  Just
  ;; green and yellow.
  (setq embark-action-indicator (propertize "Act" 'face 'success))
  (setq embark-become-indicator (propertize "Become" 'face 'warning))

  ;; ;; NOTE: I keep this around for when I do videos, otherwise I do not
  ;; ;; use it.  It requires `which-key' to display key hints.
  ;; (setq embark-action-indicator
  ;;       (lambda (map)
  ;;         (which-key--show-keymap "Embark" map nil nil 'no-paging)
  ;;         #'which-key--hide-popup-ignore-command)
  ;;       embark-become-indicator embark-action-indicator)
  :hook ((minibuffer-setup-hook . embark-collect-completions-after-input)
         (embark-post-action-hook . embark-collect--update-linked)
         (embark-collect-mode-hook . prot-embark-completions-cursor))
  :bind (("C-r" . embark-act)
         :map minibuffer-local-completion-map
         ("C-r" . embark-act)
         ("C-." . embark-act-noexit)
         ("C->" . embark-become)
         ("M-q" . embark-collect-toggle-view) ; parallel of `fill-paragraph'
         :map embark-collect-mode-map
         ("C-r" . embark-act)
         ("C-." . embark-act-noexit)
         ("r" . embark-act)
         ("." . embark-act-noexit)
         ("M-q" . embark-collect-toggle-view)
         :map embark-symbol-map
         ("." . embark-find-definition)
         ("k" . describe-keymap)))

;; Integration with Consult.  Note that the package is `embark-consult',
;; but because it comes from the same repo as Embark I prefer to use
;; this straight.el directive (check the main embark package above).
(use-package embark-consult
  :straight (embark-consult :host github
                            :repo "oantolin/embark"
                            :branch "master"
                            :files ("embark-consult.el"))
  :demand
  :after (embark consult)
  ;; ;; Use the hook, or check `prot-embark-consult-preview-toggle'.
  ;; :hook (embark-collect-mode-hook . embark-consult-preview-minor-mode)
  :bind (:map embark-collect-mode-map
         ("C-j" . embark-consult-preview-at-point)))

(use-package prot-embark
  :straight (:type built-in)
  :demand
  :after embark
  :hook ((minibuffer-exit-hook . prot-embark-clear-live-buffers)
         (embark-collect-post-revert-hook . prot-embark-collect-fit-window)
         (embark-collect-mode-hook . prot-embark-hl-line)
         (embark-collect-mode-hook . prot-embark-display-line-numbers))
  ;; NOTE: to switch to the live collection buffer, I also use
  ;; `prot-minibuffer-focus-mini-or-completions' which is bound to
  ;; "s-v".
  :bind (:map embark-collect-mode-map
         ("h" . prot-simple-describe-symbol)  ; from `prot-simple.el'
         ("C-g" . prot-embark-keyboard-quit)
         ("C-k" . prot-embark-collection-kill-line)
         ("C-M-n" . prot-embark-completions-act-next)
         ("C-M-p" . prot-embark-completions-act-previous)
         ("C-M-j" . prot-embark-completions-act-current)
         ("C-M-v" . prot-embark-consult-preview-toggle) ; "view", "visualise" mnemonic
         ("C-n" . prot-embark-next-line-or-mini)
         ("C-p" . prot-embark-previous-line-or-mini)
         ("M-F" . prot-embark-collection-flush-lines) ; M-S-f like M-S-5 (M-%)
         ("M-K" . prot-embark-collection-keep-lines)  ; same principle as right above
         :map minibuffer-local-completion-map
         ("C-n" . prot-embark-switch-to-completions-top)
         ("C-p" . prot-embark-switch-to-completions-bottom)
         ("C-l" . prot-embark-completions-toggle)))

(use-package marginalia
  :straight (:host github :repo "minad/marginalia" :branch "main")
  :demand
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))

(use-package consult
  :straight t
  :demand
  :config
  (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-min-input 3)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")

  ;; configure a function which returns the project root directory
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)

  ;; NOTE: check `embark-consult' for previews that can be used with the
  ;; default minibuffer and Embark collections.
  :bind (("C-x M-:" . consult-complex-command)
         ("C-x M-m" . consult-minor-mode-menu)
         ("C-x M-k" . consult-kmacro)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-X" . consult-mode-command)
         ("M-K" . consult-keep-lines)  ; M-S-k is similar to M-S-5 (M-%)
         ("M-s f" . consult-find)
         ("M-s g" . counsel-rg) ; THIS IS COUNSEL! NOT CONSULT
         ("M-s m" . consult-mark)
         ("C-j" . consult-buffer)
         ("C-x b" . consult-buffer)
         :map consult-narrow-map
         ("?" . consult-narrow-help)))

;; enforce the switch-buffer binding
(bind-key* "C-j" 'consult-buffer)

(use-package prot-consult
  ;; :after (consult prot-pulse)
  :after (consult)
  :config
  (setq consult-project-root-function #'prot-consult-project-root)
  (setq prot-consult-add-advice-set-hooks t)
  (setq prot-consult-command-centre-list
        '(consult-line
          prot-consult-line
          consult-mark))
  (setq prot-consult-command-top-list
        '(consult-outline
          consult-imenu
          prot-consult-outline
          prot-consult-imenu))
  (prot-consult-set-up-hooks-mode 1)
  :bind (("M-s i" . prot-consult-imenu)
         ("M-s s" . prot-consult-outline)    ; M-s o is `occur'
         ("M-y" . prot-consult-yank)
         ("M-s l" . prot-consult-line)))

(use-package prot-minibuffer
  :demand
  :bind (("s-v" . prot-minibuffer-focus-mini-or-completions)
         :map completion-list-mode-map
         ("M-v" . prot-minibuffer-focus-mini)
         ("h" . prot-simple-describe-symbol) ; from `prot-simple.el'
         ;; Those are DE FACTO DEPRECATED generic actions for the
         ;; "*Completions*" buffer.  I normally use `embark' and its own
         ;; buffers.
         ("w" . prot-minibuffer-completions-kill-symbol-at-point)
         ("i" . prot-minibuffer-completions-insert-symbol-at-point)
         ("j" . prot-minibuffer-completions-insert-symbol-at-point-exit))
  :hook (minibuffer-setup-hook . prot-minibuffer-mini-cursor))

(use-package minibuffer
  :demand
  :after prot-minibuffer
  :config
  (setq completion-styles '(orderless partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-auto-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.  Note that I actually do not use that buffer,
  ;; because I rely on Embark's version of it.
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  :bind (("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         :map minibuffer-local-completion-map
         ("C-j" . exit-minibuffer)
         ("<tab>" . minibuffer-force-complete)
         ;; De facto deprecated as I use Embark and its own completions'
         ;; buffer.
         :map completion-list-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)))

(defvar ctl-x-map-transient nil
  "Transient keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")
;; (define-prefix-command  (kbd "C-x")  ctl-x-map-transient)



(setq ctl-x-map-transient (let ((map (make-sparse-keymap)))
                            (define-key map "p" `move-windows)
                            (define-key map "n" `move-windows)
                            (define-key map "g" `move-windows)
                            (define-key map "0" 'delete-window)
                            (define-key map "q" 'delete-window)
                            (define-key map "1" 'delete-other-windows)
                            (define-key map "2" 'split-window-below)
                            (define-key map "3" 'split-window-right)
                            map))


(defun move-windows ()
  (interactive)
  (let* ((base (event-basic-type last-command-event))
         (step (pcase base
                 (?p -1)
                 (?n 1)
                 (?g 0))))
    (if (not (= step 0))
        (progn
          (message "Use p and n to move back and forwards between windows, g to quit")
          (other-window step)
          (set-transient-map ctl-x-map-transient)
          ))))
(global-set-key (kbd "C-x p")  `move-windows)
(global-set-key (kbd "C-x n")  `move-windows)

(defun resize-window (inc)
  (interactive "p")
  (let* ((base (event-basic-type last-command-event))
           (step (pcase base
                   ((or ?f ?n) inc)
                   ((or ?b ?p) (- inc))))
           (horizontal (pcase base
                         ((or ?f ?b) t))))
      (enlarge-window step horizontal))
    (message "Use f,b,n,p to adjust window size")
    (set-transient-map (let ((map (make-sparse-keymap)))
    (define-key map "f" 'resize-window);;(lambda () (interactive "p") (resize-window 1)))
    (define-key map "b" 'resize-window)
    (define-key map "n" 'resize-window)
    (define-key map "p" 'resize-window)
    map)))

(global-set-key (kbd "C-x w f") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w b") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w n") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w p") (lambda () (interactive) (resize-window 1)))

(defun local ()
  (interactive)
  (setq default-directory "/Users/jeroen"))

(defun reset-default-directory ()
  (interactive)
  (setq-local default-directory (file-name-directory (buffer-file-name))))

(defun open-shell-in-directory (directory &optional buffername)
  (interactive)
  (message "osid")
  (message directory)
  (message buffername)
  (with-temp-buffer
    (setq default-directory directory)
    (shell buffername)
    )
)

(setq ssh-config '(
                   ("fxr" "/ssh:jeroen@168.119.165.84:")
                   ("lisa-dl" "/ssh:lgpu0348@lisa.surfsara.nl:")
                  ))

(dolist (elt ssh-config)
  (define-named-lambda
    (nth 0 elt)
    (lambda (directory)
      (interactive)
      (setq default-directory directory))
    '((nth 1 elt))
    )
  (define-named-lambda
    (s-concat "shell-" (nth 0 elt))
    (lambda (name directory)
      (interactive)
      (open-shell-in-directory directory (s-concat "*shell-" name "*")))
    elt
    )
)

;; (global-set-key (kbd "C-§") (kbd "_"))
;; (defun exec-underscore () (interactive) (execute-kbd-macro (kbd "C-§")))
;; (defun exec-hyphen () (interactive) (execute-kbd-macro (kbd "-")))
;; (global-set-key (kbd "-") 'exec-underscore)
;; (global-set-key (kbd "_") 'exec-hyphen)

(bind-key* "M-<left>"  'windmove-left)
(bind-key* "M-<right>" 'windmove-right)
(bind-key* "M-<up>"    'windmove-up)
(bind-key* "M-<down>"  'windmove-down)

(bind-key* "s-s" 'shell)

(auto-fill-mode 1)

(which-key-mode 1)

(which-function-mode)

(setq ibuffer-use-other-window nil)
(global-set-key (kbd "C-x C-b") `ibuffer)

(defvar ibuffer-mode-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-2)] 'ibuffer-mouse-filter-by-mode)
    (define-key map (kbd "f") 'ibuffer-interactive-filter-by-mode)
    (define-key map (kbd "RET") 'ibuffer-visit-buffer)
    map))

(use-package swiper
  :ensure t
  :config (global-set-key (kbd "C-s") 'swiper))

;; (use-package counsel
;;   :ensure t
;;   :config
;;   ;; (global-set-key (kbd "M-x")         'counsel-M-x)
;;   ;; (global-set-key (kbd "C-x C-f")     'counsel-find-file)
;;   (global-set-key (kbd "C-h S")       'counsel-info-lookup-symbol)
;;   (global-set-key (kbd "C-c 8 <ret>") 'counsel-unicode-char)

;;   (global-set-key (kbd "C-c g") 'counsel-git)
;;   (global-set-key (kbd "C-c j") 'counsel-git-grep)
;;   ;; (global-set-key (kbd "C-c k") 'counsel-rg)
;;   (global-set-key (kbd "C-x l") 'counsel-locate)

;;   (setq counsel-find-file-ignore-regexp "\\*.fasl$"))

;; (use-package ivy
;;   :ensure t
;;   :config
;;   (ivy-mode 1)
;;   (setq ivy-use-virtual-buffers t
;;         ivy-count-format "%d/%d "
;;         ivy-wrasp t
;;         ivy-magic-tilde nil)
;;   (global-set-key (kbd "C-c C-r") 'ivy-resume))

(require 'dired-x)

(setq dired-dwim-target t)

(setq dired-omit-files
      (concat dired-omit-files
              "\\|"
              (regexp-orrify "\\.orgx$"
                             "^\.DS_Store$"
                             "^__MACOSX$"
                             "\\.pyc$"
                             "\\.pyo$"
                             "^__pycache__"
                             ".ipynb_checkpoints"
                             "\\.lprof$"
                             "\\.bak$"
                             "^ltximg$"
                             "^\\.~lock\\."
                             "^!.*pdf$"
                             "\\.~.*#")))

(add-hook 'dired-mode-hook
          #'(lambda ()
              (setq dired-guess-shell-alist-user
                    '(("\\.e?ps$" "gv" "lpr")
                      ("\\.pdf$" "mupdf" "xpdf" "lp" "acroread")
                      ("\\.mobi$" "ebook-viewer")
                      ("\\.epub$" "ebook-viewer")
                      ("\\.djvu$" "ebook-viewer")
                      ("\\.csv$" "loffice" "gnumeric")
                      ("\\.docx?$" "loffice")
                      ("\\.xlsx?$" "loffice" "gnumeric")
                      ("\\.pptx?$" "loffice")
                      ("\\.od[spt]$" "loffice")
                      ("\\.divx$" "mplayer")
                      ("\\.flv$" "mplayer")
                      ("\\.avi$" "mplayer")
                      ("\\.mpg$" "mplayer")
                      ("\\.mp4$" "mplayer")
                      ("\\.wmv$" "mplayer")
                      ("\\.mkv$" "mplayer")
                      ("\\.mov$" "mplayer")
                      ("\\.webm$" "mplayer")
                      ("Flash......$" "mplayer")
                      ("mplay......$" "mplayer")
                      ("\\.p[bgpn]m$" "geeqie" "display")
                      ("\\.gif$" "geeqie" "display")
                      ("\\.tif$" "geeqie" "display")
                      ("\\.png$" "geeqie" "display")
                      ("\\.jpe?g$" "geeqie" "display")
                      ("\\.svg$" "geeqie" "display")
                      ("\\.e?ps.g?z$" "gunzip -qc * | gv -"
                       (concat
                        "gunzip"
                        (if dired-guess-shell-gzip-quiet " -q")))
                      ("\\.e?ps.Z$" "zcat * | gv -"
                       (concat "znew"
                               (if dired-guess-shell-gzip-quiet " -q")
                               " " dired-guess-shell-znew-switches))
                      ("viewapp.asp" "xpdf"))
                    dired-listing-switches "-alh")
              (local-set-key "\C-c\C-zf" 'browse-url-of-dired-file)
              (dired-omit-mode)
              (whitespace-mode -1)))

  ;; Auto complete with ignore case
  (setq-default read-buffer-completion-ignore-case t)
  (setq-default read-file-name-completion-ignore-case t)

(setq dired-listing-switches "-alh")

(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; (global-set-key (kbd "C-c 3") 'browse-url-at-point)
(global-set-key (kbd "C-c b") 'bury-buffer)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))

(defun find-pattern-in-dir ()
  (interactive)
  (let* ((dir-name (read-directory-name "Directory to look in: "))
         (extensions (seq-map 'file-name-extension (directory-files dir-name)))
         (file-pattern (read-from-minibuffer "Files to match: " "*.org"))
         (grep-pattern (read-from-minibuffer "Grep pattern: ")))
    (shell-command (format "find %s -name  '%s' | xargs grep %s" dir-name file-pattern grep-pattern))))

(defun replace-string-defun ()
  (interactive)
  (save-window-excursion
    (narrow-to-defun)
    (mark-whole-buffer)
    (call-interactively 'replace-string)
    (widen)))

(defun comment-dwim-or-line ()
  (interactive)
  "Comments if region selected, else comment line"
   (if (use-region-p)
   (save-excursion (call-interactively 'comment-dwim))
   (save-excursion (call-interactively 'comment-line)))

)
(global-set-key (kbd "M-;") 'comment-dwim-or-line)

(use-package wrap-region
  :diminish)
(wrap-region-global-mode)
(wrap-region-add-wrapper "`" "`")
(wrap-region-add-wrapper "'" "'")
(wrap-region-add-wrapper "|" "|")
;; (wrap-region-add-wrapper "=" "=")
(wrap-region-add-wrapper "$" "$")
(wrap-region-global-mode t)

(defun format-arg ()
  (interactive)
  (if (use-region-p)
      (save-excursion (goto-char (region-beginning))
                      (insert "(format \"%s\" ")
                      (goto-char (region-end))
                      (insert  ")")
                      ;; (let ((arg (extract-rectangle (region-beginning) (region-end))))
                      ;;   (insert (concat "(format \"%s\" " (format "%s)" arg))))
                      )))

;; (format "%s" fdsf
;; (format "%s" fdf

(defun print-arg-python ()
  (interactive)
  (if (use-region-p)
      (let ((msg (read-from-minibuffer "Message to print with: ")))
      (save-excursion (goto-char (region-beginning))
                      (insert "print(f\"")
                      (insert (format "%s: {" msg))
                      (goto-char (region-end))
                      (insert  "}\")")
                      ;; (let ((arg (extract-rectangle (region-beginning) (region-end))))
                      ;;   (insert (concat "(format \"%s\" " (format "%s)" arg))))
                      ))))

(defun print-arg ()
  (interactive)
    (cond
     ((string-match-p (regexp-quote "emacs-lisp") (symbol-name major-mode))
      (call-interactively 'print-arg-elisp))
      ((string-match-p (regexp-quote "python") (symbol-name major-mode))
      (call-interactively 'print-arg-python))
))



(defun print-arg-elisp ()
  (interactive)
  (if (use-region-p)
      (save-excursion (goto-char (region-beginning))
                      (insert "(message (format \"%s\" ")
                      (goto-char (region-end))
                      (insert  "))")
                      ;; (let ((arg (extract-rectangle (region-beginning) (region-end))))
                      ;;   (insert (concat "(format \"%s\" " (format "%s)" arg))))
                      )))

(defalias  'farg 'format-arg)
(defalias  'parg 'print-arg)

(defun copy-and-comment ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (call-interactively 'kill-ring-save)
        (call-interactively (lambda () (interactive) (comment-region beg end)))
      )))
(global-set-key (kbd "C-M-;") 'copy-and-comment)

(setq indent-rigidly-map
      (let ((map (make-sparse-keymap)))
        (define-key map [left]  'indent-rigidly-left)
        (define-key map (kbd "C-M-b")  'indent-rigidly-left)
        (define-key map [right] 'indent-rigidly-right)
        (define-key map (kbd "C-M-f") 'indent-rigidly-right)
        (define-key map [S-right] 'indent-rigidly-right-to-tab-stop)
        map)
      )

(defun set-default-dir-of ()
  (interactive)
  (let ((b (read-buffer "Select buffer: " (buffer-list))))
    (setq default-directory (buffer-file-name (get-buffer (read-buffer "check" nil nil (lambda (b) (not (null (buffer-file-name (cdr b)))))))))
    ))

(load-org "drag-stuff.org")

(require 'package)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'org-mode-hook 'org-indent-mode)

;; (defun move-line-up ()
;;   "Move up the current line."
;;   (interactive)
;;   (transpose-lines 1)
;;   (forward-line -2)
;;   (indent-according-to-mode))

;; (defun move-line-down ()
;;   "Move down the current line."
;;   (interactive)
;;   (forward-line 1)
;;   (transpose-lines 1)
;;   (forward-line -1)
;;   (indent-according-to-mode))

;; (global-set-key [(meta up)]  'move-line-up)
;; (global-set-key [(meta down)]  'move-line-down)

(defun duplicate-line()
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank)
)
(global-set-key (kbd "C-c C-d") 'duplicate-line)


(defun python-gitignore  ()
  (interactive)
  (let ((fullpath (string-join (list default-directory ".gitignore"))))
    (if (file-exists-p fullpath)
        (progn
          (shell-command-to-string "curl 'https://raw.githubusercontent.com/github/gitignore/master/Python.gitignore' >> .gitignore")
          (message (format "Added python-gitignore in %s" default-directory))))))

(defun emacs-gitignore  ()
  (interactive)
  (let ((fullpath (string-join (list default-directory ".gitignore"))))
    (if (file-exists-p fullpath)
        (progn
          (shell-command-to-string "curl 'https://raw.githubusercontent.com/github/gitignore/master/Global/Emacs.gitignore' >> .gitignore")
          (message (format "Added emacs-gitignore in %s" default-directory))))))

  ;; (yank)

  ;;     (shell-command-to-string "curl 'https://raw.githubusercontent.com/github/gitignore/master/Global/Emacs.gitignore' >> .gitignore")
  ;; (message (format "Added gitignore in %s" default-directory))
;; )

;;  (require 'hydra)
;; (defhydra hydra-zoom (global-map "M")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;  ("l" text-scale-decrease "out"))
;; (drag-stuff-mode)
;; (defhydra hydra-zoom (global-map "<f2>")
;;   "zoom"
;;   ("g" text-scale-increase "in")
;;   ("l" text-scale-decrease "out"))
(define-key indent-rigidly-map (drag-stuff--kbd 'left) 'drag-stuff-left)
(define-key indent-rigidly-map (kbd "C-p") 'drag-stuff-up)
(define-key indent-rigidly-map (kbd "C-n") 'drag-stuff-down)
(define-key indent-rigidly-map (kbd "C-f") 'drag-stuff-right)
(define-key indent-rigidly-map (kbd "C-b") 'drag-stuff-left)
(drag-stuff-mode)

(defun occur-selection ()
  (interactive)
  (when (region-active-p)
    (let (deactivate-mark)
      (occur (regexp-quote (buffer-substring (region-beginning) (region-end)))))))
(global-set-key [(meta o)] 'occur-selection)

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

(defun kill-line (&optional arg)
  "Kill the rest of the current line; if no nonblanks there, kill thru newline.
With prefix argument ARG, kill that many lines from point.
Negative arguments kill lines backward.
With zero argument, kills the text before point on the current line.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg.

To kill a whole line, when point is not at the beginning, type \
\\[move-beginning-of-line] \\[kill-line] \\[kill-line].

If `show-trailing-whitespace' is non-nil, this command will just
kill the rest of the current line, even if there are no nonblanks
there.

If option `kill-whole-line' is non-nil, then this command kills the whole line
including its terminating newline, when used at the beginning of a line
with no argument.  As a consequence, you can always kill a whole line
by typing \\[move-beginning-of-line] \\[kill-line].

If you want to append the killed line to the last killed text,
use \\[append-next-kill] before \\[kill-line].

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive "P")
  (delete-region (point)
         ;; It is better to move point to the other end of the kill
         ;; before killing.  That way, in a read-only buffer, point
         ;; moves across the text that is copied to the kill ring.
         ;; The choice has no effect on undo now that undo records
         ;; the value of point from before the command was run.
               (progn
                 (if arg
         (forward-visible-line (prefix-numeric-value arg))
       (if (eobp)
           (signal 'end-of-buffer nil))
       (let ((end
        (save-excursion
          (end-of-visible-line) (point))))
         (if (or (save-excursion
             ;; If trailing whitespace is visible,
             ;; don't treat it as nothing.
             (unless show-trailing-whitespace
         (skip-chars-forward " \t" end))
             (= (point) end))
           (and kill-whole-line (bolp)))
       (forward-visible-line 1)
           (goto-char end))))
     (point))))

(use-package magit
  :ensure t
  :bind (
         ("C-x g" . magit-status)
         :map magit-mode-map
         ("C-j" . consult-buffer))
  :diminish magit-minor-mode)

;; (add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode
;;           'fundamental-mode)))
;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

;; (add-hook 'yas-minor-mode-hook
;;           (lambda ()
;;             (yas-activate-extra-mode 'fundamental-mode)))
(use-package yasnippet
  :straight t)
(yas-global-mode 1)

(bind-keys* ((kbd "C-.") . mc/mark-next-like-this)
            ((kbd "C-,") . mc/mark-previous-like-this)
            ((kbd "C-M-.") . mc/unmark-next-like-this)
            ((kbd "C-M-,") . mc/unmark-previous-like-this)
             ((kbd "C-c C-,") . mc/mark-all-like-this))

(use-package projectile
  :diminish
  :ensure t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(use-package inheritenv
  :straight (
             :host github :repo "purcell/inheritenv"
             :branch "main" :files ("inheritenv.el")
             )
  )

(use-package format-all
  :straight t
  :config
  (add-hook 'ruby-mode-hook 'format-all-mode)
  (add-hook 'yaml-mode-hook 'format-all-mode)
  (add-hook 'emacs-lisp-mode 'format-all-mode)
  )

(use-package web-mode
  :ensure t
  :mode
  ("\\.ejs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t))

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

(use-package restclient
  :straight t)

(autoload #'tramp-register-crypt-file-name-handler "tramp-crypt")
(use-package tramp
  :straight t
  :config
    (setq tramp-message-show-message "show-message")
    (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
    (setq tramp-verbose 1)
    (global-set-key (kbd "C-c t") 'tramp-cleanup-this-connection)
  )

(use-package polymode
  :straight t)

(use-package poly-markdown
  :straight t)

(use-package escr
  :straight (:host github :repo "atykhonov/escr"))

(global-set-key (kbd "C-x j r") 'escr-region-screenshot)
(global-set-key (kbd "C-x j f") 'escr-frame-screenshot)
(global-set-key (kbd "C-x j w") 'escr-window-screenshot)

(defun insert-screenshot (file-name)
  "Save screenshot to FILE-NAME and insert an Org link at point.

This calls the `import' from ImageMagick to take the screenshot,
and `optipng' to reduce the file size if the program is present."
  (interactive "FSave to file: ")
  ;; Get absolute path
  (let ((file (expand-file-name file-name)))
    ;; Create the directory if necessary
    (make-directory (file-name-directory file) 'parents)
    ;; Still, make sure to signal if the screenshot was in fact not created
    (unless (= 0 (call-process "import" nil nil nil file))
      (user-error "`import' failed to create screenshot %s" file))
    (if (executable-find "optipng")
        (start-process "optipng" nil "optipng" file))
    (insert
     ;; A link relative to the buffer where it is inserted is more portable
     (format "[[file:%s]]"
             (file-relative-name file
                                 (file-name-directory buffer-file-name))))
    (when (eq major-mode 'org-mode)
      (org-redisplay-inline-images))))

;; (setq ruby-insert-encoding-magic-comment nil)

(use-package projectile-rails
  :ensure t)
(projectile-rails-global-mode)
(define-key projectile-rails-mode-map (kbd "C-c e") 'projectile-rails-command-map)

(use-package emmet-mode
  :straight t
  :config (setq emmet-expand-jsx-className? t))

(defun web-mode-init-emmet-hook ()
  (emmet-mode))

(add-hook 'web-mode-hook  'web-mode-init-emmet-hook)

(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

(defun web-mode-init-hook ()
  "Hooks for Web mode.  Adjust indent."
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(setq prettier-js-args (list "--no-semi" "--single-quote"))

(defun web-mode-init-prettier-hook ()
  (add-node-modules-path)
  (prettier-js-mode))

(add-hook 'web-mode-hook  'web-mode-init-prettier-hook)

;; (defcustom flymake-eslint-executable-name "eslint"
;;   "Name of executable to run when checker is called.  Must be present in variable `exec-path'."
;;   :type 'string
;;   :group 'flymake-eslint)

;; (add-hook 'web-mode-hook
;;   (lambda ()
;;     (flymake-eslint-enable)))

(use-package flymake-json
  :straight t
  :config
  (global-set-key (kbd "C-c j v") 'flymake-json-load)
  )

;; (use-package org-trello
;;   :straight t)

;; (custom-set-variables '(org-trello-files (directory-files "~/trello/" nil "\\.org$")))

(use-package deft
         :straight t
         :commands (deft)
         :config (setq deft-directory "~/notes"
                       deft-extensions '("org" "md" "txt")
                       deft-default-extension "org"
                       deft-auto-save-interval 300
                       deft-use-filter-string-for-filename t))



(bind-key* (kbd "C-c C-;") 'deft)

(use-package pasp-mode
  :straight t)

(defun eval-region-pasp ()
  (interactive)
  (let ((code (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (org-element-property :value (org-element-at-point))))
        (this-buf (current-buffer)))
    (save-window-excursion (switch-to-buffer-other-window (concat "*"
                                                                  (file-name-base)
                                                                  "*"))
                           (end-of-buffer)
                           (insert (format "print_answer_sets(\"\"\"%s\"\"\")"
                                           code))
                           (comint-send-input)
                           (switch-to-buffer this-buf))))

;; (with-temp-buffer )
(defun save-and-run-pasp ()
  (interactive)
  (progn
    (save-buffer)
    (pasp-run-buffer)))
(define-key pasp-mode-map (kbd "C-c C-c") #'save-and-run-pasp)

;; patch this function to add quotes around filepath
(defun pasp-generate-command (encoding &optional instance)
  "Generate Clingo call with some ASP input file.

   Argument ENCODING The current buffer which holds the problem encoding.
   Optional argument INSTANCE The problem instance which is solved by the encoding.
     If no instance it is assumed to be also in the encoding file."
     (if 'instance
         (concat pasp-clingo-path " " pasp-clingo-options " '" encoding "' " instance)
       (concat pasp-clingo-path " " pasp-clingo-options " '" encoding "'")))

(setq pasp-clingo-options "-n 0")

(defun pasp-gvis ()
  (interactive)
  (let ((answerset (buffer-substring (region-beginning)
                                                  (region-end))))
    (shell-command (format "python /Users/jeroen/code/UvA/msc/krr/gvis.py '%s'"
                                           answerset))))

(eval-after-load 'pasp-compilation-mode
                 '(define-key pasp-compilation-mode-map (kbd "C-c C-v") 'pasp-gvis))

(use-package ess
  :straight t
  :config
  (add-hook `inferior-ess-mode-hook (lambda () (setq comint-input-ring-size 1500)))
  (setq ess-eval-visibly 'nowait) ;; don't hang buffer when exec-ing code
  (setq ess-fancy-comments nil) ;; don't indent comments
  )

(eval-after-load "comint"
   '(progn
      (define-key comint-mode-map [up]
        'comint-previous-matching-input-from-input)
      (define-key comint-mode-map [down]
        'comint-next-matching-input-from-input)

      ;; also recommended for ESS use --
      (setq comint-move-point-for-output 'others)
      ;; somewhat extreme, almost disabling writing in *R*, *shell* buffers above prompt:
      ;; (setq comint-scroll-to-bottom-on-input 'this)
      ))

(defun load-ess-mode-maps ()
  (dolist (m (list ess-r-mode-map inferior-ess-mode-map))
    (bind-keys :map m
               ("M-i" . ess-insert-assign)
               )))
(add-hook `inferior-ess-mode-hook 'load-ess-mode-maps)

(use-package ess-R-data-view
  :straight t
  :config
  (bind-key* "C-x w" 'ess-R-dv-ctable)
   )



;; (use-package ess-view
;;   :straight t)

;; (setq ess-view--spreadsheet-program "/Applications/Numbers.app")

(use-package poly-R
  :straight t)

;; (add-to-list 'load-path "~/code/paulodder/canvas-utils/")
;; (require 'canvas-utils)
;; (setq canvas-baseurl "https://canvas.uva.nl") ; url you visit to go to the
                                        ; canvas instance of your institution
                                        ; (e.g. https://canvas.uva.nl)
;; (setq canvas-token "10392~0HN7MJHY2C0MA2XcZlNvra3OScZR8crUs7xxbjT6yl6rb1YEPYYgb9yzlSgdTETW") ; when logged in generate an access
                                        ; token under Account > Settings
                                        ; > Approved integrations

(when (eq window-system 'w32)
  (setq putty-directory "C:/Program Files/PuTTY")
  (setq tramp-default-method "plink")
  (when (and (not (string-match putty-directory (getenv "PATH")))
       (file-directory-p putty-directory))
    (setenv "PATH" (concat putty-directory ";" (getenv "PATH")))
    (add-to-list 'exec-path putty-directory)))

(load-file "~/code/matthewlmcclure/tramp-virtualenv/tramp-virtualenv.el")

(setq my-keybase-username "jpj8")
(use-package keybase-chat
  :straight (keybase-chat
             :host github
             :repo "paulodder/keybase-chat"
             )
)

(bind-key (kbd "C-c k") 'keybase-join-channel)

(global-set-key (kbd "C-;") 'avy-goto-char-2)

(plist-put org-format-latex-options :scale 1.8)

(defun exec-source-block ()
  "Copies and pastes the current source block to
  the active python session and executes it."
  (interactive)
  (let* ((this-window (selected-window))
         (sb-content (if (region-active-p)
                         (substring-no-properties (buffer-string)
                                                  (- (region-beginning)
                                                     1)
                                                  (- (region-end)
                                                     1))
                       (string-trim (org-element-property :value (org-element-at-point)))))
         (sb-info (org-babel-get-src-block-info))
         (maybe-cpaste-content (if (string= "python"
                                            (first sb-info))
                                   (concat "\n%cpaste\n" sb-content "\n--")
                                 sb-content)))
    (save-excursion
      (org-babel-switch-to-session)
      (end-of-buffer)
      (insert maybe-cpaste-content)
      (comint-send-input)
      (select-window this-window))))


(defun org-src-exec-blocks-up-until ()
  "applies exec-source-block to all source blocks up until current point"
  (interactive)
  (let ((max-point (point)))
    (save-excursion
      (beginning-of-buffer)
      (org-babel-next-src-block)
      (while (<= (point) max-point)
        (progn
          (org-babel-next-src-block)
          (exec-source-block))))))

(define-key org-mode-map (kbd "C-c C-c") 'exec-source-block)
(define-key org-mode-map (kbd "C-c c") 'org-ctrl-c-ctrl-c)

(bind-keys* :map org-mode-map
            ((kbd "M-n") . org-babel-next-src-block)
            ((kbd "M-p") . org-babel-previous-src-block)
            )

(defun org-toggle-execution-on-export ()
  (interactive)
  (setq org-export-babel-evaluate (not org-export-babel-evaluate))
  (message "org-export-babel-evaluate turned %s" (if org-export-babel-evaluate "on" "off")))

(defun org-execute-code-in-shell  (&optional arg _info)
  "Copy current src block's contents and execute it in code shell buffer."
  (interactive)
  (save-window-excursion
    (org-babel-switch-to-session arg (org-babel-get-src-block-info))
    (end-of-buffer)
    (yank)
    (comint-send-input)
    (comint-send-input)
    (comint-send-input)
    ))

(defun org-execute-code-in-shell  (&optional arg _info)
  "Copy current src block's contents and execute it in code shell buffer."
  (interactive "P")
  (let ((this-window (selected-window))
        (info (org-babel-get-src-block-info)))
    (org-babel-switch-to-session arg info)
    (end-of-buffer)
    (yank)
    (comint-send-input)
    (comint-send-input)
    (comint-send-input)
    (select-window this-window)
    ))

(org-defkey org-mode-map "\C-c\C-c" `org-execute-code-in-shell)
(org-defkey org-mode-map "\C-c\c" 'org-ctrl-c-ctrl-c)

(fset 'org-yank-src-block-into-session
   (kmacro-lambda-form [?\C-c ?\' ?\C-x ?h ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\C-c ?\' ?\C-c ?\C-v ?\C-z ?\C-a ?\C-  ?\C-e backspace ?\C-e ?  ?\C-\M-y ?\M-o] 0 "%d"))

(define-key org-mode-map (kbd "C-c y") 'org-yank-src-block-into-session)

(fset 'org-copy-src-block
   (kmacro-lambda-form [?\C-c ?\' ?\C-x ?h ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\C-c ?\'] 0 "%d"))


(define-key org-mode-map (kbd "C-M-w") 'org-copy-src-block)

(defun exec-source-block ()
  "Copies and pastes the current source block to
  the active python session and executes it."
  (interactive)
  (let* ((this-window (selected-window))
         (sb-content (if (region-active-p)
                         (substring-no-properties (buffer-string)
                                                  (- (region-beginning)
                                                     1)
                                                  (- (region-end)
                                                     1))
                       (string-trim (org-element-property :value (org-element-at-point)))))
         (sb-info (org-babel-get-src-block-info))
         (maybe-cpaste-content (if (string= "python"
                                            (first sb-info))
                                   (concat "\n%cpaste\n" sb-content "\n--")
                                 sb-content)))
    (save-excursion
      (org-babel-switch-to-session)
      (end-of-buffer)
      (insert maybe-cpaste-content)
      (comint-send-input)
      (select-window this-window))))

(use-package poporg
      :bind (("C-c /" . poporg-dwim)))

;; (add-to-list 'load-path "~/.emacs.d/github/ox-ipynb")
;; (require 'ox-ipynb)

(setq org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(load-library "org")
(push "/home/paul/org-mode/lisp" load-path)
(define-key org-mode-map (kbd "C-c o") 'org-open-at-point)
(define-key global-map (kbd "C-C l") 'org-store-link)

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WIP" . "yellow")
        ("CANCELED" . (:foreground "blue" :weight bold))
        ("DONE" . "green")))
(use-package org-bullets
  ;; :hook (org-mode org-bullets-mode)
  :init (progn
          (setq org-ellipsis "⤵")
          ;; (add-hook org-mode-hook org-bullets-mode)
          ))

(add-hook `org-mode-hook (lambda () (setq inhibit-read-only 1)
                           ;; (auto-insert-mode)
                           (wrap-region-add-wrapper "=" "=")
                           (wrap-region-add-wrapper "~" "~")
                           (wrap-region-add-wrapper "+" "+")
                           (wrap-region-add-wrapper "/" "/")
                           (wrap-region-add-wrapper "*" "*")
                           (wrap-region-add-wrapper "_" "_")
                           (wrap-region-add-wrapper "|" "|")
                           ;; (modify-syntax-entry ?= "\"")
                           (modify-syntax-entry ?* "\"")
                           ;; (modify-syntax-entry ?_ "\"")
                           (modify-syntax-entry ?| "\"")
                           (org-bullets-mode)
                           ))

;; (defadvice syntax-table (before in-src activate)
;;   (if (org-in-src-block-p)
;;       (message "HEERE")
;;     (setq-local syntax-table python-mode-syntax-table)
;;     (setq-local syntax-table (syntax-table))))

;; (define-key org-mode-map "\C-\M-f" '(lambda ()
;;                                      (interactive)
;;                                      (if (org-in-src-block-p)
;;                                          (with-syntax-table
;;                                              python-mode-syntax-table
;;                                            (forward-sexp))
;;                                        (forward-sexp)
;;                                            )))

;; (setq prettify-symbols-alist
;; (prettify-utils-generate (("\\mathbb{N}" "ℕ"))))
;; (add-hook `org-mode-hook (lambda ()
;; (setq prettify-symbols-alist '(("\\models" .  "⊧") ("\\vdash" .  "⊢")
;;                                ("\\bar{a}"  . "a̅") ("\\underbrace"  . "a⃗")
;;                                ("\\subseteq"  . "⊆")))
;; (prettify-symbols-mode)))
;; (add-hook 'global-prettify-symbols-mode (lambda ()
;; (setq prettify-symbols-alist '(("\\models" .  "⊧") ("\\vdash" .  "⊢")
;; ("\\bar{a}"  . "a̅") ("\\underbrace"  . "a⃗")
;; ("\\subseteq"  . "⊆")))

;; ))
;; (add-hook 'org-mode-hook (org-bullets-mode))

;; (add-hook 'org-mode-hook org-bullets-mode)


;; Org babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 (mapcar (lambda (m) (cons m t))
         '(;; C calc dot
           emacs-lisp ;; gnuplot java js latex
           ;; lisp
           python ipython
           latex
           ;; R racket  not necessary for my purposes
           ;; ruby scheme
           shell sqlite ;; haskell
           sql)))
(defun my-org-confirm-babel-evaluate (lang body)
  (not (member lang '("ipython" "python" "emacs-lisp" "sh"))))

(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
;; (use-package ox-latex
;;   :config (push '(racket "Racket") org-latex-listings-langs))

;; (push '("https" . "i.reddituploads.com") org-html-inline-image-rules)
;;   (require 'ob-ipython)
;; (use-package ob-ipython :ensure t)
(require 'package)
;; (add-to-list 'package-archives
;;                  '("melpa" . "https://melpa.org/packages/"))

(defun org--get-syntax-table-for-src () (interactive)
       (let* ((lang (first (org-babel-get-src-block-info)))
              )
       (cond ((string-equal "emacs-lisp" lang)
              emacs-lisp-mode-syntax-table))
))

;; (package-initialize)
;;   (require 'ob-sql-mode)

(defun sql-beautify-region (beg end)
  "Beautify SQL in region between beg and END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "anbt-sql-formatter" nil t)))
    ;; change sqlbeautify to anbt-sql-formatter if you
    ;;ended up using the ruby gem

(defun sql-beautify-buffer ()
 "Beautify SQL in buffer."
 (interactive)
 (sql-beautify-region (point-min) (point-max)))

(define-advice org-edit-src-exit (:before (&rest _args) format-sql)
  "Run `blacken-buffer' when leaving an org-mode Python source block."
  (when (eq major-mode 'sql-mode)
    (sql-beautify-buffer)))

(defun org-export-filter-timestamp-remove-brackets (timestamp backend info)
  "removes relevant brackets from a timestamp"
  (cond
   ((org-export-derived-backend-p backend 'latex)
    (replace-regexp-in-string "[<>]\\|[][]" "" timestamp))
   ((org-export-derived-backend-p backend 'html)
    (replace-regexp-in-string "&[lg]t;\\|[][]" "" timestamp))))

(eval-after-load 'ox '(add-to-list
                       'org-export-filter-timestamp-functions
                       'org-export-filter-timestamp-remove-brackets))

(add-to-list 'org-src-lang-modes '("latex-macros" . latex))

(defvar org-babel-default-header-args:latex-macros
  '((:results . "raw")
    (:exports . "results")))

(defun prefix-all-lines (pre body)
  (with-temp-buffer
    (insert body)
    (string-insert-rectangle (point-min) (point-max) pre)
    (buffer-string)))

(defun org-babel-execute:latex-macros (body _params)
  (concat
   (prefix-all-lines "#+LATEX_HEADER: " body)
   "\n#+HTML_HEAD_EXTRA: <div style=\"display: none\"> \\(\n"
   (prefix-all-lines "#+HTML_HEAD_EXTRA: " body)
   "\n#+HTML_HEAD_EXTRA: \\)</div>\n"))

(setenv "IPY_TEST_SIMPLE_PROMPT" "1")
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt -i --colors=Linux --profile=default")
;; python-shell-interpreter-args "--simple-prompt -i")
;; "--colors=Linux --profile=default"
(setq org-babel-python-command "ipython")
(defun org-py3.6 ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq org-babel-python-command "python3.6"))
;; (concat default-directory "~/.envs/py3.6")))
(defun org-quanti ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq org-babel-python-command "/home/paul/.virtualenvs/quanti/bin/python"))
;; (concat default-directory "~/.envs/py3.6"))
(defun org-nhbc-poc ()
  (interactive)
  (setq org-babel-python-command "/home/paul/.virtualenvs/nhbc-poc/bin/python"))

(defun org-ogi_products ()
  (interactive)
  (setq org-babel-python-command "/home/paul/.virtualenvs/ogi_products/bin/python")
  (setq python-shell-interpreter "/home/paul/.virtualenvs/ogi_products/bin/ipython"))

(defun org-ogi-db ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq org-babel-python-command "/home/paul/.virtualenvs/ogi-database/bin/python"))

(defun org-ogi-quotes ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq org-babel-python-command "/home/paul/.virtualenvs/ogi-quotes/bin/python"))

(defun org-local-py3.7 ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq org-babel-python-command "/usr/bin/python3.7"))
;; (concat default-directory "~/.envs/py3.6")))



(defun py-nhbc-poc ()
  (interactive)
  ;; (setq python-shell-virtualenv-root
  (setq python-shell-interpreter "/home/paul/.virtualenvs/nhbc-poc/bin/python"))
;; (concat default-directory "~/.envs/py3.6")))

(defun execute-in-shell (&optional arg _info)
    "Switch to code buffer and display sessiona and execute code there."
    (interactive "P")
    (let ((swap-windows
           (lambda ()
             (let ((other-window-buffer (window-buffer (next-window))))
               (set-window-buffer (next-window) (current-buffer))
               (set-window-buffer (selected-window) other-window-buffer))
             (other-window 1)))
          (info (org-babel-get-src-block-info))
          (org-src-window-setup 'reorganize-frame))
      (save-window-excursion
        (org-babel-switch-to-session arg info))
        (org-edit-src-code)
        (copy-to-buffer  (point-min) (point-max))
        (funcall swap-windows)
        (org-edit-src-exit)
      ))

;; (require 'ob-async)

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(setq org-src-fontify-natively t)

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    (latex . t)))

(setq org-latex-inline-image-rules '(("file" . "\\.\\(pdf\\|jpeg\\|jpg\\|png\\|ps\\|eps\\|tikz\\|pgf\\|svg\\|gif\\)\\'")))

;; installation
;; (let ((packages (list 'org-plus-contrib 'org-ref)))
;;   ; refresh if needed.
;;   (unless (cl-every #'package-installed-p packages)
;;     (package-refresh-contents))

;;   (dolist (package packages)
;;     (unless (package-installed-p package)
;;       (package-install package))))

(setq org-ref-notes-directory "~/Documents/literature/notes"
      org-ref-pdf-directory "~/Documents/literature/bibtex-pdfs/"
      org-ref-default-bibliography '("~/Documents/literature/references.bib")
      )

(setq bibtex-completion-bibliography
      '(org-ref-default-bibliography
        ))
(setq bibtex-completion-notes-path org-ref-notes-directory)

(unless (file-exists-p org-ref-pdf-directory)
  (make-directory org-ref-pdf-directory t))

;; Some org-mode customization
(setq org-src-fontify-natively t
      org-confirm-babel-evaluate nil
      org-src-preserve-indentation t)

(org-babel-do-load-languages
 'org-babel-load-languages '((python . t)))

(setq org-latex-pdf-process
      '("pdflatex -interaction nonstopmode -output-directory %o %f"
  "bibtex %b"
  "pdflatex -interaction nonstopmode -output-directory %o %f"
  "pdflatex -interaction nonstopmode -output-directory %o %f"))

(setq bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(use-package dash)
(setq org-latex-default-packages-alist
      (-remove-item
       '("" "hyperref" nil)
       org-latex-default-packages-alist))

;; Append new packages
(add-to-list 'org-latex-default-packages-alist '("" "natbib" "") t)
(add-to-list 'org-latex-default-packages-alist
       '("linktocpage,pdfstartview=FitH,colorlinks,
linkcolor=blue,anchorcolor=blue,
citecolor=blue,filecolor=blue,menucolor=blue,urlcolor=blue"
         "hyperref" nil)
       t)

;; some requires for basic org-ref usage
;; (require 'org-plus-contrib)
(require 'org-ref)
(require 'org-ref-pdf)
(require 'org-ref-url-utils)

;; setup org-ref
(setq org-ref-notes-function
      (lambda (thekey)
  (let ((bibtex-completion-bibliography (org-ref-find-bibliography)))
    (bibtex-completion-edit-notes
     (list (car (org-ref-get-bibtex-key-and-file thekey)))))))

;; (use-package org-plus-contrib
;;   :straight t)
;; (use-package org-ref
;;   :straight t)
;; (use-package org-ref-pdf
;;   :straight t)
;; (use-package org-ref-url-utils
;;   :straight t)

(use-package scratch
  :straight t)

;; (require 'auctex)
(require 'mode-local)
  (use-package auctex
    :defer t
    :ensure t
    :hook (LaTeX-mode-hook .
    electric-quote-local-mode)
    :config
    ;; Some LaTeX packages need to run a shell command, e.g. minted needs
    ;; pygmentize
    (push '("LaTeX-shell"
            "%`%l -shell-escape %(mode)%' %t"
            TeX-run-TeX
            nil
            (latex-mode doctex-mode)
            :help "Run LaTeX allowing shell escape")
          TeX-command-list)
    (setq font-latex-fontify-script nil)
    (setq-mode-local latex-mode region-extract-function latex--region-extract-function)
    )


  ;; configs below need to be outside the use-package since org-mode may need
  ;; them directly.

  (setq TeX-view-program-list
        '(("mupdf" "/bin/mupdf %s.pdf"))
        TeX-view-program-selection
        '((output-pdf "PDF Tools")
          ;; (output-pdf "mupdf")
          ((output-dvi style-pstricks) "dvips and gv")
          (output-dvi "xdvi")
          (output-html "xdg-open"))
        TeX-source-correlate-start-server t
        LaTeX-electric-left-right-brace t)

  (setq bibtex-completion-bibliography
        (mapcar (lambda (f)
                  (concat
                   "/path/to/media/data/tie/path/to/media/writing/bib/"
                   f
                   ".bib"))
                '("bon" "counting" "dsls" "etc")))

  ;; Update PDF buffers after successful LaTeX runs
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)

(defun has-face-at (pos face-in-question &optional object)
  (let ((face-result-at-pos (get-text-property pos 'face object)))
    (if (eq (type-of face-result-at-pos) 'cons)
        (seq-contains face-result-at-pos face-in-question)
      (eq face-result-at-pos face-in-question))
    ))
;; (face-at-post ))


(defun latex--add-math-delimiters (substr)
  "add $ if necessary"
  (if (<= (length substr) 1)
      substr
    ;; (message (format "%s" (has-face-at  0 'font-latex-math-face substr)))
    (let*
        (;; (face-result-at-start (get-text-property 0 'face substr))
         ;; (face-at-start (if (eq (type-of face-result-at-start) 'cons) (first face-result-at-start) face-result-at-start))
         (substr-prefixed (if (and (not (string-prefix-p "$" substr))
                                   (has-face-at  0 'font-latex-math-face substr)
                                   )
                              (concat "$" substr)
                            substr))
         (substr-suffixed
          (if (and (not (string-suffix-p "$" substr-prefixed))
                   ;; (has-face-at  (- (length substr-prefixed) 1) 'font-latex-math-face substr-prefixed)
                   (has-face-at (- (length substr-prefixed) 1)
                   ;; (has-face-at (length substr-prefixed)
                                'font-latex-math-face substr-prefixed)
                   )
              (concat  substr-prefixed "$")
            substr-prefixed))
         )
      substr-suffixed

      )

    )
  )


;; (defun latex--add-math-delimiters (str)
;;   (let ((max-pos (length str)))
;;     (message "AAA")
;;     str
;; ))

(defvar latex--region-extract-function
  (lambda (method)
    (when (region-beginning)
      (cond
       ((eq method 'bounds)
        (list (cons (region-beginning) (region-end))))
       ((eq method 'delete-only)
        (delete-region (region-beginning) (region-end)))
       (t
        (latex--add-math-delimiters (filter-buffer-substring (region-beginning) (region-end) method))
        ;; (message out)
        ;; (message (latex--add-math-delimiters out))
        ))))
  "Function to get the region's content.
Called with one argument METHOD which can be:
- nil: return the content as a string (list of strings for
  non-contiguous regions).
- `delete-only': delete the region; the return value is undefined.
- `bounds': return the boundaries of the region as a list of one
  or more cons cells of the form (START . END).
- anything else: delete the region and return its content
  as a string (or list of strings for non-contiguous regions),
  after filtering it with `filter-buffer-substring', which
c  is called, for each contiguous sub-region, with METHOD as its
  3rd argument.")
(setq-mode-local latex-mode region-extract-function latex--region-extract-function)

(defun has-face (face-in-question face-seq)
  )

(defun latex--possibly-remove-math-delimiters (face start end)
  ;; (message (format "%s" (seq-contains face 'font-latex-math-face)))
  ;; (message (format "%s" (get-text-property (+ start 1) :foreground)))
  (let* ((substr (buffer-substring start end))
         ;; (face-after (get-text-property (+ end 1) 'face ))
         ;; (face-before (get-text-property (- start 1) 'face ))
         (has-face-after (has-face-at (+ end 1) 'font-latex-math-face);; (get-text-property (+ end 1) 'face )
                         )
         (has-face-before (has-face-at (- start 1) 'font-latex-math-face);; (get-text-property (+ end 1) 'face )
                          )
         ;; (face-before (get-text-property (- start 1) 'face ))
         )
    (if (and (string-equal "$" substr)
             has-face-after has-face-before)
        (delete-region start end))))


    ;; (progn (message (format "%s"  substr ))
    ;;        (message (format "%s"  (get-text-property start 'face )))
    ;;        (message (format "face after %s"  (get-text-property (+ end 1) 'face )))
    ;;        (message (format "face before %s"  ))
    ;;        ;; (message (format "%s"  (null face)))
    ;;        (message (format "%s" face))
    ;; ;; (message (format "%s" (call-interactively 'delete-char)))
    ;; (if (and
    ;;      (eq substr "$")
    ;;      (null (get-text-property start 'face ))
    ;;      (not (null face)))
    ;;     (call-interactively delete-char)))
;; ))
;; (message "CALLED")
;;   (message (format "%s" face))
;;   (message (format "%s" start))
;;   (message (format "%s" end))
;;   (message (buffer-substring start end))
;; ;; (message (format "%s" (get-text-property start 'face )))
;;   ;; (message (format "%s" (get-text-property (+ 1 start) :face )))
;;   ;; (message (format "%s" (get-text-property (+ 1 start) 'face )))
;;   (message (format "%s" (get-text-property start 'face )))
;;   (cond ((and (seq-contains face 'font-latex-math-face)
;;              (not (seq-contains  (get-text-property (+ 1 start) 'face ) 'font-latex-math-face)))
;;          (message "HERE")
;;         (message (format "%s" face)))
;;         )
;; (cond ((
;;   (progn (message (format "%s" face))
;;   (message (format "%s" start))
;;   (message (format "%s" end)))
;; )))
;; )



(defvar-mode-local latex-mode yank-handled-properties
  '((font-lock-face . yank-handle-font-lock-face-property)
    (face . latex--possibly-remove-math-delimiters)
    ;; ('foreground . latex--possibly-add-math-delimiters)
    (category . yank-handle-category-property)))

;; (modify-syntax-entry ?^ " " tex-mode-syntax-table)
;; (modify-syntax-entry ?^ "" )
;; (modify-syntax-entry ?^ " " LaTeX-mode-syntax-table)

;; (defun latex--filter-buffer-substring (beg end &optional delete)
;;   "Return the buffer substring between BEG and END, after filtering.
;; If DELETE is non-nil, delete the text between BEG and END from the buffer.

;; This calls the function that `filter-buffer-substring-function' specifies
;; \(passing the same three arguments that it received) to do the work,
;; and returns whatever it does.  The default function does no filtering,
;; unless a hook has been set.

;; Use `filter-buffer-substring' instead of `buffer-substring',
;; `buffer-substring-no-properties', or `delete-and-extract-region' when
;; you want to allow filtering to take place.  For example, major or minor
;; modes can use `filter-buffer-substring-function' to exclude text properties
;; that are special to a buffer, and should not be copied into other buffers."
;;   (let* ((substr (funcall filter-buffer-substring-function beg end delete)))
;;     (if (<= (length substr) 1)
;;         substr
;;       (let*
;;           ((substr-prefixed (if (and (not (string-prefix-p "$" substr))
;;                                      (eq (get-text-property 0 'face substr) 'font-latex-math-face)
;;                                      )
;;                                 (concat "$" substr)
;;                               substr))
;;            (substr-suffixed (if (and (not (string-suffix-p "$" substr-prefixed))
;;                                      (eq (get-text-property (length substr-prefixed)
;;                                                             'face substr-prefixed) 'font-latex-math-face)
;;                                      )
;;                                 (concat  substr-prefixed "$")
;;                               substr-prefixed))))
;;       )
;;     )
;;   )




;; (defvar latex--region-extract-function
;;   (lambda (method)
;;     (when (region-beginning)
;;       (cond
;;        ((eq method 'bounds)
;;         (list (cons (region-beginning) (region-end))))
;;        ((eq method 'delete-only)
;;         (delete-region (region-beginning) (region-end)))
;;        (t
;;         (latex--filter-buffer-substring (region-beginning) (region-end) method)))))
;;   "Function to get the region's content.
;; Called with one argument METHOD which can be:
;; - nil: return the content as a string (list of strings for
;;   non-contiguous regions).
;; - `delete-only': delete the region; the return value is undefined.
;; - `bounds': return the boundaries of the region as a list of one
;;   or more cons cells of the form (START . END).
;; - anything else: delete the region and return its content
;;   as a string (or list of strings for non-contiguous regions),
;;   after filtering it with `filter-buffer-substring', which
;;   is called, for each contiguous sub-region, with METHOD as its
;;   3rd argument.")
;; (require 'mode-local)
;; (setq-mode-local latex-mode region-extract-function latex--region-extract-function)
;; (add-hook 'latex-mode-hook
;;           (lambda ()
;;             (setq-mode-local latex-mode region-extract-function latex--region-extract-function)))

(defun ans-copy ()
  (interactive)
  (let ((tmp-file (make-temp-file "ans_" nil nil
                                  (substring-no-properties
                                   (buffer-string) (region-beginning) (region-end)))))
    (kill-new (shell-command-to-string
                  (format "~/.virtualenvs/py3.8/bin/python ~/src/latex2ans.py %s" tmp-file))
                 nil)
))


  ;; (while (< current end)
  ;;   (cond ((and (has-face-at current font-latex-math-face)
  ;;               (string-equal (substring-no-properties current (+ current 1) "$")))
  ;;          (
  ;;          )

(require 'unicode-fonts)
(unicode-fonts-setup)

(require 'latex-unicode-math-mode)
;; Enable latex-unicode-math-mode automatically for all LaTeX files.
;; This converts LaTeX to Unicode inside math environments.
(add-hook 'LaTeX-mode-hook (lambda () (modify-syntax-entry ?^ " ")))

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

(defun set-bash () (interactive) (setq explicit-shell-file-name "/bin/bash"))
(defun set-zsh () (interactive) (setq explicit-shell-file-name "/bin/zsh"))

;; set zsh as default
(set-zsh)

(define-key shell-mode-map (kbd "SPC") 'comint-magic-space)

;; (use-package vterm
;;   :straight t)

(push "~/.node_modules_global/bin" exec-path)
(load-file "~/.emacs.d/prettier-emacs/prettier-js.el")
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(eval-after-load 'js-mode (lambda ()
                            (define-key js-mode-map (kbd "C-c C-c")  'indium-reload)))



(add-hook 'js-mode-hook (lambda ()
  (web-mode-set-content-type "jsx")))
(require 'js-format)
  (eval-after-load 'js-mode
    (add-hook 'js-mode-hook
              (lambda()
                (js-format-setup "prettier"))))
(custom-set-variables
  '(js-auto-format-command "prettier")
  '(js-auto-format-command-args "--write --single-quote --no-semi"))
  ;; using "jsbeautify-css" as css formatter
  (eval-after-load 'css-mode
    (add-hook 'css-mode-hook
              (lambda()
                (js-format-setup "jsb-css"))))


(add-hook `js2-mode-hook (lambda ()
                           (global-set-key (kbd "C-c C-v") #'js-format-buffer)))

(load-file "~/.emacs.d/prettier-emacs/prettier-js.el")
(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

;; (use-package tide
;;    :hook (web-mode . my/activate-tide-mode)
;;    :ensure t)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

;; (define-key dump-jump-mode-map (kbd "C-M-p") nil)
;; python-indent-dedent-line-backspace
      (use-package python
        :ensure t
        :mode ("\\.py\\'" . python-mode)
        :interpreter ("python" . python-mode)
        :config
        (setq python-shell-interpreter "ipython"
        ;; python-shell-interpreter-args "--simple-prompt -i --colors=Linux --profile=default")
          python-shell-interpreter-args "")
              (push '("\\.ipynb$" . js2-mode) auto-mode-alist)
        :hook
        (python-mode . (lambda ()
                         "No eldoc for remote files"
                         (let ((name (buffer-file-name)))
                           (when (and name
                                      (> (length name) 5)
                                      (string= "/ssh:" (substring name 0 5)))
                             (eldoc-mode -1))))))

  (setq py-shell-name "ipython")

  (setq python-shell-interpreter-args "-c exec('__import__(\\'readline\\')') -i")

              ;; python-shell-interpreter-args "--simple-prompt -i")
    ;; (defcustom py-shell-name
    ;;   (if (eq system-type 'windows-nt)
    ;;       "C:/Python27/python"
    ;;     ;; "python"
    ;;     "ipython")

    ;; ;;   "A PATH/TO/EXECUTABLE or default value `py-shell' may look for.

    ;; ;; If no shell is specified by command.

    ;; ;; On Windows default is C:/Python27/python
    ;; ;; --there is no garantee it exists, please check your system--

    ;; ;; Else python"
    ;; ;;   :type 'string
    ;; ;;   :tag "py-shell-name"
    ;; ;;   :group 'python-mode)

(use-package blacken
  :diminish)
(setq blacken-line-length 79)
(setq blacken-executable "/Users/jeroen/.virtualenvs/py3.8/bin/black")
(add-hook 'python-mode-hook 'blacken-mode 'too-long-lines-mode)

(define-advice org-edit-src-exit (:before (&rest _args) format-python)
  "Run `blacken-buffer' when leaving an org-mode Python source block."
  (when (eq major-mode 'python-mode)
    (blacken-buffer)))

;; (use-package blacken
;;   :ensure t
;;   :bind (:map python-mode-map ("C-c C-b" . blacken-buffer))
;;   :config
;;   (setq blacken-line-length 80)
;;   (add-hook 'python-mode-hook 'blacken-mode))

;; (setq jedi:server-command '("pip3" "/Users/jeroen/.emacs.d/.python-environments/default/bin/jediepcserver"))

;;; Python Jedi
;;; From emacs-jedi readme
;; Type:
;;     M-x jedi:install-server RET
;; Then open Python file.
;; (use-package jedi
;;   :ensure t
;;   ;; :disabled nil
;;   :defer 3
;;   :config
;;   ;; Standard Jedi.el setting
;;   (add-hook 'python-mode-hook 'jedi:setup)
;;   (setq jedi:complete-on-dot t))

;;   (use-package elpy
;;     :ensure t
;;     :diminish elpy-mode
;;     :config (elpy-enable))

;;     ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

;;     (custom-set-variables
;;      ;; sudo dnf install python-jedi python3-jedi -y
;;      ;; '(elpy-rpc-backend "jedi")
;;      '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
;;      '(help-at-pt-timer-delay 0.9)
;;      '(tab-width 2))

;;     ;; Do not highlight indentation
;;     ;; (delete 'elpy-module-highlight-indentation elpy-modules)
;;     ;; )
;; (setq elpy-eldoc-show-current-function nil)

(push '("\\.xsh$" . python-mode) auto-mode-alist)

(push '("/Pipfile$" . conf-mode) auto-mode-alist)
(push '("/Pipfile.lock$" . js2-mode) auto-mode-alist)

;; Fix Python loading bug in emacs25
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))

(setq
 ;; python-shell-interpreter "python"
 ;; python-shell-interpreter-args "--colors=Linux --profile=default"
 python-shell-interpreter-interactive-arg "--simple-prompt -i"
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
;; (setq python-shell-interpreter "ipython"
;;       python-shell-interpreter-args "--simple-prompt"
;;       py-python-command-args '("--matplotlib")
;;       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;       python-shell-completion-setup-code
;;       "from IPython.core.completerlib import module_completion"
;;       python-shell-completion-module-string-code
;;       "';'.join(module_completion('''%s'''))\n"
;;       python-shell-completion-string-code
;;       "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"

;;       python-check-command "pylint"
;;       python-indent-offset 2
;;       py-autopep8-options '("--max-line-length=80")
;;       )

;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell)             ;; if you want eshell support
  ;; note that setting `venv-location` is not necessary if you
  ;; use the default location (`~/.virtualenvs`), or if the
  ;; the environment variable `WORKON_HOME` points to the right place
  (setq venv-location "~/.virtualenvs/"))

(require 'info-look)

(info-lookup-add-help
 :mode 'python-mode
 :regexp "[[:alnum:]_]+"
 :doc-spec '(("(python)Index" nil "")))

;;     (load-file "/home/paul/.emacs.d/too-long-lines-mode/too-long-lines-mode.el")
;; (add-hook `inferior-python-mode-hook (too-long-lines-mode))

(define-key python-mode-map (kbd "DEL") nil)

(fset 'ipython-cpaste
   (kmacro-lambda-form [?\M-> ?% ?c ?p ?a ?s ?t ?e return ?\M-x ?y ?a ?n ?k ?\C-m return ?- ?- return] 0 "%d"))

(define-key inferior-python-mode-map (kbd "C-M-y") 'ipython-cpaste)
(define-key shell-mode-map (kbd "C-M-y") 'ipython-cpaste)

(setenv "WORKON_HOME" "/usr/local/Caskroom/miniconda/base/envs")

;; (defun django-shell ()
;;   (interactive)
;;   (let ((python-shell-interpreter "python")
;;     (python-shell-interpreter-args "-i /home/paul/projects/plekje/api/manage.py shell_plus"))
;;     (run-python)))
;; # (setq python-shell-interpreter "python"
;; #       python-shell-interpreter-args "-i /home/paul/projects/plekje/api/manage.py shell_plus")

(defun jupyter-insert-token ()
  (interactive)
  (let ((output (shell-command-to-string "jupyter notebook list")))
        (insert (first (split-string (second (split-string output "token=")) " " )))
))

(defun pip-venv-mode () (interactive)
(setq venv-location "~/.virtualenvs/"))

(defun conda-venv-mode () (interactive)
(setq venv-location "/usr/local/Caskroom/miniconda/base/envs"))

;; (use-package pygen
;;   :ensure t)
;; (add-hook 'python-mode-hook 'pygen-mode)

(use-package python-docstring
  :straight t)

(add-hook 'python-mode-hook 'python-docstring-mode)

;; (load "~/.emacs.d/icicles-install")
;; (customize-variable' "icicle-download-dir" "~/.emacs.d/icicles")


;; (add-to-list 'load-path "~/.emacs.d/icicles")
;; (require 'icicles)

;; (icy-mode 1)

(delete 'flycheck-disabled-checkers "json-jsonlist")
(use-package flycheck
  :ensure t)
(flycheck-add-mode 'json-jsonlint 'json-mode)
(add-hook 'json-mode-hook 'flycheck-mode)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(bind-key* "C-x M-e" 'eval-and-replace)

(use-package ace-window
  :ensure t)
(global-set-key (kbd "M-o") 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
(defvar aw-dispatch-alist
  '((?x aw-delete-window "Delete Window")
  (?m aw-swap-window "Swap Windows")
  (?M aw-move-window "Move Window")
  (?c aw-copy-window "Copy Window")
  (?j aw-switch-buffer-in-window "Select Buffer")
  (?n aw-flip-window)
  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
  (?c aw-split-window-fair "Split Fair Window")
  (?v aw-split-window-vert "Split Vert Window")
  (?b aw-split-window-horz "Split Horz Window")
  (?o delete-other-windows "Delete Other Windows")
  (?? aw-show-dispatch-help))
  "List of actions for `aw-dispatch-default'.")

(defun dashboard-remember (list-size)
  (insert "remember:
    [M--] negative argument
    [C-M-v & M-- C-M-v] scroll other window down & up
    [M-m] move to indentation
    [C-c C-j in python] imenu
    [M-{ & M-}] traverse paragraph-type blocks
"))
(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 6)
                            (projects . 8)
                            (remember . 1)))
    (setq dashboard-banner-logo-title "jpjmacs")
    (setq dashboard-startup-banner "~/.emacs.d/araki_banner.png")
    (setq dashboard-set-navigator t)
    (setq dashboard-set-footer nil)
    (setq dashboard-navigator-buttons
      `(;; line1
        (
         (nil "shell-fxr" "open shell on fxr" (lambda (&rest _) (shell-fxr)))
         (nil "shell-lisa" "open shell on lisa-dl" (lambda (&rest _) (shell-lisa-dl)))
        )
       )
      )
    )
  :config
  (dashboard-setup-startup-hook))
(add-to-list 'dashboard-item-generators  '(remember . dashboard-remember))

(use-package olivetti
  :ensure t
  :diminish
  :config
  (setq olivetti-body-width 0.7)
  (setq olivetti-minimum-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))

(defun focus ()
  (interactive)
  (delete-other-windows)
  (olivetti-mode))

(defun defocus ()
  (interactive)
  (olivetti-mode))

(defun org-copy-section-as-tex ()
  (interactive)
  (save-excursion
    (call-interactively 'org-previous-visible-heading)
    (call-interactively 'org-mark-element)
    (next-line)
    (tex2ans (buffer-substring (region-beginning)
                               (region-end)))))

(defun tex2ans (s)
  (let ((tmp-file (make-temp-file "ans_" nil nil s)))
    (kill-new (shell-command-to-string (format "~/.virtualenvs/py3.8/bin/python ~/.emacs.d/scripts/latex2ans.py %s"
                                               tmp-file))
              nil)))

(define-key org-mode-map (kbd "C-c C-8") 'org-copy-section-as-tex)

(defun shell-on-windows ()
  (let ((explicit-shell-file-name "C:/Windows/System32/bash.exe"))
    (shell)))

;; (use-package diminish
;;   :straight t
;;   :config
;;   (diminish 'projectile-mode)
;;   (diminish 'auto-fill-mode))

(use-package rich-minority
  :straight t
  :config
  ;; (setq rm-blacklist "Projectile.*")
  (setq rm-whitelist-regexps
        '(
          "mc:*"
          " Def"
          ))
  (setq rm-whitelist (mapconcat 'identity rm-whitelist-regexps "\\|"))
  ;; (setq rm-whitelist
  ;;     (format "^ \(%s\)$"
  ;;             (mapconcat #'identity
  ;;                        rm-whitelist-regexps
  ;;                        "\\|")))
  (rich-minority-mode 1))

;; status-icon to the left of filename
(defun render-mode-line-status-icon (read-only modified)
  (if read-only
      ""
    (if modified
        " ●"
        " ○"
      )))
(defun render-mode-line-remote (remote)
  (if remote
      " @"
    ""))


;; mode-line section on left of screen
(setq mode-line-left-section
      (list
       ;; day and time
       ;; '(:eval (propertize (format-time-string " %b %d %H:%M ")
       ;;                     'face 'font-lock-builtin-face))

       ;; buffer status icon (dot)
       '(:eval (render-mode-line-status-icon buffer-read-only (buffer-modified-p)))
       ;; (render-mode-line-remote mode-line-remote)

       ;; the buffer name; the file name as a tool tip
       '(:eval (propertize " %b "
                           'help-echo (buffer-file-name)))

       ;; relative position, size of file
       ;; " ["
       ;; (propertize "%p" 'face 'font-lock-constant-face) ;; % above top
       ;; "/"
       ;; (propertize "%I" 'face 'font-lock-constant-face) ;; size
       ;; "] "
       ))

;; mode-line section on right of screen
(setq mode-line-right-section
      (list
       ;; git branch
       '(:eval (if vc-mode
                   (propertize (substring vc-mode 5)
                           'face 'font-lock-comment-face)
                   ""))

       ;; line and column
       " [" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-comment-face) ":"
       (propertize "%02c" 'face 'font-lock-comment-face)
       "] "
       ;; (propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

       ;; the current major mode
       (propertize " %m" 'face 'font-lock-comment-face)
       " "
       ;; rich-minority minor modes
       rm--mode-line-construct
       "  "
       ))

(defun mode-line-render ()
  (append (append mode-line-left-section
                  (list
                   ;; function to right-justify part of modeline
                   ;; by filling center with spaces
                   '(:eval (s-repeat
                            (- (+ (window-total-width) 0)
                               (+
                                (length (format-mode-line mode-line-right-section))
                                (length (format-mode-line mode-line-left-section))))
                            " "))
                   )
                  mode-line-right-section)))


;; actually render the mode-line
;; (setq-default mode-line-format (mode-line-render))

;; move modeline to the top of the buffer
(setq-default header-line-format (mode-line-render))
;; (setq-default mode-line-format'(""))
;; hide empty mode-line
(setq-default mode-line-format nil)

;; reduce height of empty mode-line
(set-face-attribute 'mode-line nil :foreground "white" :background nil :box nil :overline "#1e212e")
(set-face-attribute 'mode-line-inactive nil :background nil :box nil :foreground "#232635" :overline "#1e212e")

(set-face-attribute 'vertical-border nil :background nil :foreground "#676E95")

;; decorate header-line
(set-face-attribute 'header-line nil
                    :background "#1c1f2b"
                    :foreground "#EEFFFF"
                    :box '(:line-width 5 :color "#1c1f2b")
                    :overline nil
                    :underline nil)

(bind-key* "M-/" 'hippie-expand)

(use-package powerthesaurus
  :straight t
  :config
  (bind-key* "s-p" 'powerthesaurus-lookup-word-dwim)
  )

(use-package google-translate
  :straight t
  :config
  (setq google-translate-default-source-language "nl")
  (setq google-translate-default-target-language "en")
  (bind-key* "C-c t" 'google-translate-smooth-translate)
  (setq google-translate-translation-directions-alist
      '(("nl" . "en") ("en" . "nl") ("en" . "es") ("es" . "en")))
  )

(bind-key* "C-c t" 'google-translate-smooth-translate)

(use-package prism
  :straight (:host github :repo "alphapapa/prism.el" :branch "master")
  :defer
  :config
  (setq prism-comments nil) ; non-nil distorts colours
  (setq prism-num-faces 8))

  ;; ;; NOTE: read the manual of the `modus-themes' for prism.el

  ;; ;; for 4 colours (the closest to the default)
  ;; (prism-set-colors
  ;;   :desaturations '(0) ; may lower the contrast ratio
  ;;   :lightens '(0)      ; same
  ;;   :colors (modus-themes-with-colors
  ;;             (list fg-main
  ;;                   cyan-alt-other
  ;;                   magenta-alt-other
  ;;                   magenta)))
  ;;
  ;; ;; for 16 colours
  ;; (prism-set-colors
  ;;   :desaturations '(0) ; may lower the contrast ratio
  ;;   :lightens '(0)      ; same
  ;;   :colors (modus-themes-with-colors
  ;;             (list fg-main
  ;;                   magenta
  ;;                   cyan-alt-other
  ;;                   magenta-alt-other
  ;;                   blue
  ;;                   magenta-alt
  ;;                   cyan-alt
  ;;                   red-alt-other
  ;;                   green
  ;;                   fg-main
  ;;                   cyan
  ;;                   yellow
  ;;                   blue-alt
  ;;                   red-alt
  ;;                   green-alt-other
  ;;                   fg-special-warm)))
  ;;
  ;; for 8 colours
  ;; (prism-set-colors
  ;;   :desaturations '(0) ; may lower the contrast ratio
  ;;   :lightens '(0)      ; same
  ;;   :colors (modus-themes-with-colors
  ;;             (list fg-special-cold
  ;;                   magenta
  ;;                   magenta-alt-other
  ;;                   cyan-alt-other
  ;;                   fg-main
  ;;                   blue-alt
  ;;                   red-alt-other
  ;;                   cyan))))

(defun maak-belachelijk ()
  (interactive)
  (let ((initial-end (region-end)))
  (if (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (while (<= (point) initial-end)
          (message (format "%s"(point)))
          (goto-char (+
                      (+ (floor (* 3 (/ (abs (random)) (float most-positive-fixnum)))) 1)
                      (point)))
          (if (<= (point) initial-end)
                  (call-interactively 'upcase-char)))))))

(use-package atomic-chrome
  :straight t)

(fset 'shell-last-python-script
   (kmacro-lambda-form [?\M-r ?p ?y ?t ?h ?o ?n ?  return] 0 "%d"))

(bind-keys* :map shell-mode-map
            ((kbd "C-M-r") . shell-last-python-script)
            )

(fset 'fxr-restart-fxrbot
   (kmacro-lambda-form [?c ?d return ?c ?d ?  ?k ?e ?y ?b ?a ?s ?e return ?k ?i ?l ?l ?  ?\C-a ?\C-k ?p ?s ?  ?- ?e ?f ?  ?| ?  ?g ?r ?e ?p ?  ?f ?x ?r ?b ?o ?t return ?\C-a ?\M-f ?\M-f ?\C-a ?\C-p ?\M-f ?\M-f ?\M-b ?\C-  ?\M-f ?\M-w ?\M-> ?k ?i ?l ?l ?  ?\C-y return ?c ?a ?t ?  ?s ?t ?a ?r ?t ?_ ?f ?x ?r ?b ?o ?t ?. ?s ?h return ?\C-p ?\C-p ?\C-a ?\C-  ?\C-n ?\C-e ?\M-w ?\M-> ?\C-y return ?\C-c ?\C-c ?n ?o ?h ?u ?p ?  ?p ?y ?t ?h ?o ?n ?  ?/ ?h ?o ?m ?e ?/ ?j ?e ?r ?o ?e ?n ?/ ?k ?e ?y ?b ?a ?s ?e ?/ ?f ?x ?r ?b ?o ?t ?. ?p ?y ?  ?& return return] 0 "%d"))
