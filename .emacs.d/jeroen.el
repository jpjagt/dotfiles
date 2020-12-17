(setq visible-bell t)

(defun define-named-lambda (name lambd args)
  (defalias (intern name) `(lambda () (interactive) (apply ,lambd ',args))))

(defun define-custom-function (name func)
  (define-named-lambda name (lambda () (funcall func))))

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

(require 's)

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

;; drag-stuff


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

(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(projectile-rails-global-mode)
(define-key projectile-rails-mode-map (kbd "C-c e") 'projectile-rails-command-map)

(require 'web-mode)
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

(add-to-list 'load-path "~/code/paulodder/canvas-utils/")
(require 'canvas-utils)
(setq canvas-baseurl "https://canvas.uva.nl") ; url you visit to go to the
                                        ; canvas instance of your institution
                                        ; (e.g. https://canvas.uva.nl)
(setq canvas-token "10392~0HN7MJHY2C0MA2XcZlNvra3OScZR8crUs7xxbjT6yl6rb1YEPYYgb9yzlSgdTETW") ; when logged in generate an access
                                        ; token under Account > Settings
                                        ; > Approved integrations

(load-file "~/code/matthewlmcclure/tramp-virtualenv/tramp-virtualenv.el")

(global-set-key (kbd "C-;") 'avy-goto-char-2)

(plist-put org-format-latex-options :scale 1.8)

(use-package poporg
      :bind (("C-c /" . poporg-dwim)))

(add-to-list 'load-path "~/.emacs.d/github/ox-ipynb")
(require 'ox-ipynb)

(defun set-bash () (interactive) (setq explicit-shell-file-name "/bin/bash"))
(defun set-zsh () (interactive) (setq explicit-shell-file-name "/bin/zsh"))

;; (define-key dump-jump-mode-map (kbd "C-M-p") nil)
;; python-indent-dedent-line-backspace
      (use-package python
        :ensure t
        :mode ("\\.py\\'" . python-mode)
        :interpreter ("python" . python-mode)
        :config
        (setq python-shell-interpreter "python"
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

(use-package elpy
    :ensure t
    :diminish elpy-mode
    :config (elpy-enable))

    ;; (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

    (custom-set-variables
     ;; sudo dnf install python-jedi python3-jedi -y
     ;; '(elpy-rpc-backend "jedi")
     '(help-at-pt-display-when-idle (quote (flymake-overlay)) nil (help-at-pt))
     '(help-at-pt-timer-delay 0.9)
     '(tab-width 2))

    ;; Do not highlight indentation
    ;; (delete 'elpy-module-highlight-indentation elpy-modules)
    ;; )
(setq elpy-eldoc-show-current-function nil)

(push '("\\.xsh$" . python-mode) auto-mode-alist)

(push '("/Pipfile$" . conf-mode) auto-mode-alist)
(push '("/Pipfile.lock$" . js2-mode) auto-mode-alist)

;; Fix Python loading bug in emacs25
'(with-eval-after-load 'python
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

(require 'pygen)
(add-hook 'python-mode-hook 'pygen-mode)

(load-theme 'doom-palenight t)

;; (load "~/.emacs.d/icicles-install")
;; (customize-variable' "icicle-download-dir" "~/.emacs.d/icicles")


;; (add-to-list 'load-path "~/.emacs.d/icicles")
;; (require 'icicles)

;; (icy-mode 1)

(delete 'flycheck-disabled-checkers "json-jsonlist")
(require 'flycheck)
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

(require 'ace-window)
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

(use-package dashboard
  :ensure t
  :init
  (progn
    (setq dashboard-items '((recents . 6)
                            (projects . 8)))
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

(use-package olivetti
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
