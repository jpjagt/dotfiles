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
    ;; (select-window this-window)

  ;; (let ((this-window (selected-window))
  ;; ;;       (info ))


  ;;   ))


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

;; ("C-c C-c"  (lambda () (org-ctrl-c-ctrl-c))))

(defun shell (&optional buffer)
  "Run an inferior shell, with I/O through BUFFER (which defaults to `*shell*').
Interactively, a prefix arg means to prompt for BUFFER.
If `default-directory' is a remote file name, it is also prompted
to change if called with a prefix arg.

If BUFFER exists but shell process is not running, make new shell.
If BUFFER exists and shell process is running, just switch to BUFFER.
Program used comes from variable `explicit-shell-file-name',
 or (if that is nil) from the ESHELL environment variable,
 or (if that is nil) from `shell-file-name'.
If a file `~/.emacs_SHELLNAME' exists, or `~/.emacs.d/init_SHELLNAME.sh',
it is given as initial input (but this may be lost, due to a timing
error, if the shell discards input when it starts up).
The buffer is put in Shell mode, giving commands for sending input
and controlling the subjobs of the shell.  See `shell-mode'.
See also the variable `shell-prompt-pattern'.

To specify a coding system for converting non-ASCII characters
in the input and output to the shell, use \\[universal-coding-system-argument]
before \\[shell].  You can also specify this with \\[set-buffer-process-coding-system]
in the shell buffer, after you start the shell.
The default comes from `process-coding-system-alist' and
`default-process-coding-system'.

The shell file name (sans directories) is used to make a symbol name
such as `explicit-csh-args'.  If that symbol is a variable,
its value is used as a list of arguments when invoking the shell.
Otherwise, one argument `-i' is passed to the shell.

\(Type \\[describe-mode] in the shell buffer for a list of commands.)"
  (interactive
   (list
    (and current-prefix-arg
         (prog1
             (read-buffer "Shell buffer: "
                          ;; If the current buffer is an inactive
                          ;; shell buffer, use it as the default.
                          (if (and (eq major-mode 'shell-mode)
                                   (null (get-buffer-process (current-buffer))))
                              (buffer-name)
                            (generate-new-buffer-name "*shell*")))
           (if (file-remote-p default-directory)
               ;; It must be possible to declare a local default-directory.
               ;; FIXME: This can't be right: it changes the default-directory
               ;; of the current-buffer rather than of the *shell* buffer.
               (setq default-directory
                     (expand-file-name
                      (read-directory-name
                       "Default directory: " default-directory default-directory
                       t nil))))))))
  (setq buffer (if (or buffer (not (derived-mode-p 'shell-mode))
                       (comint-check-proc (current-buffer)))
                   (get-buffer-create (or buffer "*shell*"))
                 ;; If the current buffer is a dead shell buffer, use it.
                 (current-buffer)))

  ;; On remote hosts, the local `shell-file-name' might be useless.
  (if (and (called-interactively-p 'any)
           (file-remote-p default-directory)
           (null explicit-shell-file-name)
           (null (getenv "ESHELL")))
      (with-current-buffer buffer
        (set (make-local-variable 'explicit-shell-file-name)
             (file-remote-p
              (expand-file-name
               (read-file-name
                "Remote shell path: " default-directory shell-file-name
                t shell-file-name))
              'localname))))

  ;; The buffer's window must be correctly set when we call comint (so
  ;; that comint sets the COLUMNS env var properly).
  (switch-to-buffer buffer)
  (unless (comint-check-proc buffer)
    (let* ((prog (or explicit-shell-file-name
                     (getenv "ESHELL") shell-file-name))
           (name (file-name-nondirectory prog))
           (startfile (concat "~/.emacs_" name))
           (xargs-name (intern-soft (concat "explicit-" name "-args"))))
      (unless (file-exists-p startfile)
        (setq startfile (concat user-emacs-directory "init_" name ".sh")))
      (apply 'make-comint-in-buffer "shell" buffer prog
             (if (file-exists-p startfile) startfile)
             (if (and xargs-name (boundp xargs-name))
                 (symbol-value xargs-name)
               '("-i")))
      (shell-mode)))
  buffer)

(setq ibuffer-use-other-window nil)
(global-set-key (kbd "C-x C-b") `ibuffer)

(defvar ibuffer-mode-name-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(mouse-2)] 'ibuffer-mouse-filter-by-mode)
    (define-key map (kbd "f") 'ibuffer-interactive-filter-by-mode)
    (define-key map (kbd "RET") 'ibuffer-visit-buffer)
    map))

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

(defun message-this-window ()
(interactive)
(message "%s" (selected-window)))
(global-set-key (kbd "<C-return>") `message-this-window)

;; (defun ivy-done ()
;;   "Exit the minibuffer with the selected candidate."
;;   (interactive)
;;   (if (ivy--prompt-selected-p)
;;       (ivy-immediate-done)
;;     (setq ivy-current-prefix-arg current-prefix-arg)
;;     (delete-minibuffer-contents)
;;     (cond ((or (> ivy--length 0)
;;                ;; the action from `ivy-dispatching-done' may not need a
;;                ;; candidate at all
;;                (eq this-command 'ivy-dispatching-done))
;;            (ivy--done (ivy-state-current ivy-last)))
;;           ((memq (ivy-state-collection ivy-last)
;;                  '(read-file-name-internal internal-complete-buffer))
;;            (if (or (not (eq confirm-nonexistent-file-or-buffer t))
;;                    (equal " (confirm)" ivy--prompt-extra))
;;                (ivy--done ivy-text)
;;              (setq ivy--prompt-extra " (confirm)")
;;              (insert ivy-text)
;;              (ivy--exhibit)))
;;           ((memq (ivy-state-require-match ivy-last)
;;                  '(nil confirm confirm-after-completion))
;;            (ivy--done ivy-text))
;;           (t
;;            (setq ivy--prompt-extra " (match required)")
;;            (insert ivy-text)
;;            (ivy--exhibit)))))

(defvar TeX-command-default "LaTeX"
      "The default command for `TeX-command' in the current major mode.")
    ;; (setq TeX-command-default )

(defun TeX-command-master (&optional override-confirm)
      "Run command on the current document.

    If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
    depend on it being positive instead of the entry in `TeX-command-list'."
      (interactive "P")
      ;; (save-buffer); edits won't automatically show otherwise
      (TeX-command "LaTeX"
                   'TeX-master-file override-confirm))



;; (defun TeX-command-master (&optional override-confirm)
  ;;       "Run command on the current document.

  ;;     If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
  ;;     depend on it being positive instead of the entry in `TeX-command-list'."
  ;;       (interactive "P")
  ;;       ;; (save-buffer); edits won't automatically show otherwise
  ;;       (TeX-command TeX-command-default              ;
  ;;                    'TeX-master-file nil))

;; (defun TeX-command-master (&optional override-confirm)
;;         "Run command on the current document.

;;       If a prefix argument OVERRIDE-CONFIRM is given, confirmation will
;;       depend on it being positive instead of the entry in `TeX-command-list'."
;;         (interactive "P")
;;         ;; (save-buffer); edits won't automatically show otherwise
;;         (TeX-command TeX-command-default              ;
;;                      'TeX-master-file nil))

(defun courses  ()
(interactive)
(setq default-directory "/home/paul/courses/s2/")
)
(defun projects  ()
(interactive)
(setq default-directory "/home/paul/projects/")
)
(defun quanti  ()
(interactive)
(setq default-directory "/home/paul/projects/quanti")
)

(defun modal  ()
(interactive)
(setq default-directory "/home/paul/courses/s1_modal_logic")
)
(defun lolaco  ()
(interactive)
(setq default-directory "/home/paul/Dropbox/courses/s1_logic_language_computation/")
)
(defun gcvenn  ()
(interactive)
(setq default-directory "/home/paul/projectjes/gcvenn/")
)

(defun it  ()
(interactive)
(setq default-directory "/home/paul/courses//s1_information_theory/")
)

(defun coco-site () (interactive)
       (eww "https://staff.fnwi.uva.nl/r.dehaan/complexity2020/"))
(defun coco-mode ()
  (interactive)
  (let* (
         (hw-dir "/home/paul/courses/s2/coco/hw/")
         (file-dir "/home/paul/courses/s2/coco/files/")
         (hw-files (seq-filter (lambda (f) (and (string-prefix-p "hw" f)
                                                (string-suffix-p "solutions.tex" f)))
                               (directory-files hw-dir)))
         (hw-solutions-file-tex "take-home.tex")
         (current-hw-number (string-to-number (substring hw-solutions-file-tex 2 3)))
         (hw-questions-file "take-home-exam.pdf")
         (hw-solutions-file-pdf (concat (file-name-sans-extension
                                         hw-solutions-file-tex) ".pdf")))
    (message (concat hw-dir hw-solutions-file-tex))
    (message (concat file-dir hw-questions-file))
    (call-interactively 'delete-other-windows)
    (call-interactively 'split-window-right)
    (find-file (concat hw-dir hw-solutions-file-tex))
    (find-file-other-window (concat file-dir hw-questions-file))
    (pdf-view-fit-height-to-window)
    (pdf-view-enlarge 1.90)
    (call-interactively 'split-window-below)
    (find-file-other-window (concat hw-dir hw-solutions-file-pdf))
    (pdf-view-fit-height-to-window)
    (pdf-view-enlarge 1.90)
    (call-interactively 'other-window)))
;; (defun coco-mode ()
;;   (interactive)
;;   (let* (
;;          (hw-dir "/home/paul/courses/s2/coco/hw/")
;;          (file-dir "/home/paul/courses/s2/coco/files/")
;;          (hw-files (seq-filter (lambda (f) (and (string-prefix-p "hw" f)
;;                                                 (string-suffix-p "solutions.tex" f)))
;;                                (directory-files hw-dir)))
;;          (hw-solutions-file-tex (-max-by (lambda (a b) (> (string-to-number(substring a 2 3)) (string-to-number(substring b 2 3)))) hw-files))
;;          (current-hw-number (string-to-number (substring hw-solutions-file-tex 2 3)))
;;          (hw-questions-file (format "hw%s.pdf"  current-hw-number))
;;          (hw-solutions-file-pdf (concat (file-name-sans-extension hw-solutions-file-tex) ".pdf"))
;;          )
;;     (message (concat hw-dir hw-solutions-file-tex))
;;     (message (concat file-dir hw-questions-file))
;;     (call-interactively 'delete-other-windows)
;;     (call-interactively 'split-window-right)
;;     (find-file (concat hw-dir hw-solutions-file-tex))
;;     (find-file-other-window (concat file-dir hw-questions-file))
;;     (pdf-view-fit-height-to-window)
;;     (pdf-view-enlarge 1.90)
;;     (call-interactively 'split-window-below)
;;     (find-file-other-window (concat hw-dir hw-solutions-file-pdf))
;;     (pdf-view-fit-height-to-window)
;;     (pdf-view-enlarge 1.90)
;;     (call-interactively 'other-window)

;;     )
;;   )



  ;;   (-max-by (lambda (a b) (= (string-to-number(substring a 3 4)) (string-to-number(substring b 3 4)))) hw-files)
  ;; (find-file "/home/paul/projects/gcvenn/genes/static/js/base.js")
  ;; (shell)
  ;; (end-of-buffer)
  ;; (sit-for 1)
  ;; (comint-send-input)
  ;; (insert "cd /home/paul/projects/gcvenn")
  ;; (comint-send-input)
  ;; (sit-for 1)
  ;; (insert "python manage.py runserver")
  ;; (comint-send-input)
  ;; (indium-launch)
  ;; )
(defun coco-site () (interactive)
(eww "https://staff.fnwi.uva.nl/r.dehaan/complexity2020/"))

(defun tda-site ()
  (interactive)
(eww "https://www2.cs.duke.edu/courses/fall06/cps296.1/"))

(defun diremacs ()
(interactive)
(setq default-directory "/home/paul/.emacs.d/")
)

;; (defun org-venv-brug ()
;;     (interactive)
;;     (setq org-babel-python-command "/home/paul/.virtualenvs/bruggeman/bin/python"))

(defun paul ()
      (interactive)
      (setq default-directory "/ssh:paul@31.20.232.17:"))

(defun fp-unfill-paragraph (&optional justify region)
  (interactive (progn
         (barf-if-buffer-read-only)
         (list (if current-prefix-arg 'full) t)))
  (interactive)
  (let ((fill-column 100000))
    (fill-paragraph justify region)))

(global-set-key "\C-ceu" 'fp-unfill-paragraph)

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

;; (eq (string-to-syntax "(") 4)

;;       ;; (= (first (string-to-syntax "("))  4)
;;   (char-syntax (string-to-char "$"))
;;     ;; (syntax-code "{")
;; (kbd "C-M-<Backspace>")
;; (kbd "C-DEL")
(not (seq-contains `(40 36) (char-syntax (string-to-char "{"))))

(defun coco-load-files () (interactive)
       (let ((default-directory "/home/paul/courses/s2/coco/"))
             (async-shell-command "bash ~/courses/s2/coco/get_files.sh")))

(defun gcvenn-mode ()
  (interactive)
  (venv-workon "gcvenn")
  (find-file "/home/paul/projects/gcvenn/genes/static/js/base.js")
  (shell)
  (end-of-buffer)
  (sit-for 1)
  (comint-send-input)
  (insert "cd /home/paul/projects/gcvenn")
  (comint-send-input)
  (sit-for 1)
  (insert "python manage.py runserver")
  (comint-send-input)
  (indium-launch)
  )

(defun transip ()
  (interactive)
  (setq default-directory "/ssh:paul@149.210.193.148:"))
(defun plekje-transip ()
  (interactive)
  (setq default-directory "/ssh:paul@37.97.129.188:"))
(defun plekje-sfs ()
  (interactive)
  (setq default-directory "/ssh:paul@37.97.145.128:"))
(defun su-plekje-sfs ()
  (interactive)
  (setq default-directory "/ssh:paul@37.97.145.128|sudo:37.97.145.128:"))
(defun su-transip ()
  (interactive)
(setq default-directory "/ssh:paul@149.210.193.148|sudo:149.210.193.148:")
)
  ;; (setq default-directory "/ssh:paul@149.210.193.148/"))
(defun su-plekje-transip ()
  (interactive)
  (setq default-directory "/ssh:paul@37.97.129.188|sudo:37.97.129.188:"))

(defun phone ()
  (interactive)
  (setq default-directory "/ssh:paul@10.14.18.66:"))

(defun matrix-cookbook () (interactive) (eww "https://www.math.uwaterloo.ca/~hwolkowi/matrixcookbook.pdf")
       )
