(setq debug-on-error nil)

(setq-default inhibit-startup-screen t
              inhibit-splash-screen t
              initial-scratch-message "")

(toggle-frame-fullscreen)

;; File for custom-set variables
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

(setq visible-bell t)

(setq next-line-add-newlines t)

(defun define-named-lambda (name lambd args)
  (defalias (intern name) `(lambda () (interactive) (apply ,lambd ',args))))

(defun define-custom-function (name func)
  (define-named-lambda name (lambda () (funcall func))))

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

(require 's)

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

(set-face-font 'default "Roboto Mono 12")

;; ;; (set-frame-parameter (selected-frame)
;; ;;                      'internal-border-width 24)
(setq default-frame-alist
      (append (list '(vertical-scroll-bars . nil)
                    ;; '(internal-border-width . 24)
                    '(font . "Roboto Mono 12"))))


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

(defun slick-cut (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Cut line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-region :before #'slick-cut)

(defun slick-copy (beg end)
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(advice-add 'kill-ring-save :before #'slick-copy)

(bind-key* "M-<left>"  'windmove-left)
(bind-key* "M-<right>" 'windmove-right)
(bind-key* "M-<up>"    'windmove-up)
(bind-key* "M-<down>"  'windmove-down)

(bind-key* "C-j" 'ivy-switch-buffer)

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

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x")         'counsel-M-x)
  (global-set-key (kbd "C-x C-f")     'counsel-find-file)
  (global-set-key (kbd "C-h S")       'counsel-info-lookup-symbol)
  (global-set-key (kbd "C-c 8 <ret>") 'counsel-unicode-char)

  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  ;; (global-set-key (kbd "C-c k") 'counsel-rg)
  (global-set-key (kbd "C-x l") 'counsel-locate)

  (setq counsel-find-file-ignore-regexp "\\*.fasl$"))

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-wrasp t
        ivy-magic-tilde nil)
  (global-set-key (kbd "C-c C-r") 'ivy-resume))

;; (message "done one")

(require 'dired-x)

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
(global-set-key (kbd "C-c t") 'tramp-cleanup-this-connection)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k") 'keybase-open-chat)
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

(require 'wrap-region)
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
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode)

;; (add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode
;;           'fundamental-mode)))
;; (use-package yasnippet
;;   :config
;;   (yas-global-mode 1))

;; (add-hook 'yas-minor-mode-hook
;;           (lambda ()
;;             (yas-activate-extra-mode 'fundamental-mode)))
(require 'yasnippet)
(yas-global-mode 1)

(bind-keys* ((kbd "C-.") . mc/mark-next-like-this)
            ((kbd "C-,") . mc/mark-previous-like-this)
            ((kbd "C-M-,") . mc/unmark-next-like-this)
            ((kbd "C-M-.") . mc/unmark-previous-like-this)
             ((kbd "C-c C-,") . mc/mark-all-like-this))

(use-package projectile
  :ensure t)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(use-package projectile-rails
  :ensure t)
(projectile-rails-global-mode)
(define-key projectile-rails-mode-map (kbd "C-c e") 'projectile-rails-command-map)

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

;; (setq my-keybase-username "jpj8")
;; (add-to-list 'load-path "~/.emacs.d/keybase-chat/")
;; ;; (add-to-list 'load-path "~/.emacs.d/zone-matrix")
;; ;; (require 'zone-matrix)
;; (load-file "~/.emacs.d/keybase-chat/keybase-chat.el")
;; (load-file "~/.emacs.d/keybase-chat/keybase-markup.el")
;; (require 'keybase)

(global-set-key (kbd "C-;") 'avy-goto-char-2)

(plist-put org-format-latex-options :scale 1.8)

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

(fset 'org-copy-src-block
   (kmacro-lambda-form [?\C-c ?\' ?\C-x ?h ?\M-w ?\C-u ?\C-  ?\C-u ?\C-  ?\C-c ?\'] 0 "%d"))


(define-key org-mode-map (kbd "C-M-w") 'org-copy-src-block)

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

(use-package tide
   :hook (web-mode . my/activate-tide-mode)
   :ensure t)
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

(require 'blacken)
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
       '(:eval (propertize (substring vc-mode 5)
                           'face 'font-lock-comment-face))

       ;; line and column
       " [" ;; '%02' to set to 2 chars at least; prevents flickering
       (propertize "%02l" 'face 'font-lock-comment-face) ":"
       (propertize "%02c" 'face 'font-lock-comment-face)
       "] "
       ;; (propertize org-mode-line-string 'face '(:foreground "#5DD8FF"))

       ;; the current major mode
       (propertize " %m" 'face 'font-lock-comment-face)
       "  "
       ;;minor-mode-alist
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
(setq-default mode-line-format (mode-line-render))

;; move modeline to the top of the buffer
(setq-default header-line-format (mode-line-render))
(setq-default mode-line-format'(""))
;; hide empty mode-line
;; (setq-default mode-line-format nil)

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
