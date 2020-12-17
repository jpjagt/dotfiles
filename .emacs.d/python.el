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

(setenv "WORKON_HOME" "/Users/jeroen/miniconda3/envs")

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
(setq venv-location "/Users/jeroen/miniconda3/envs"))
