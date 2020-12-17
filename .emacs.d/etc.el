;; ;; (global-set-key (kbd "C-h i") 'helm-info)

;; (with-eval-after-load 'info
;;   (info-initialize)
;;   (add-to-list 'Info-directory-list
;;                "~/.emacs.d/site-lisp/use-package/")
;;   ;; Link all info files to my info dir and update the dir file there.
;;   (add-to-list 'Info-directory-list
;;                "/path/to/media/info"))

;; ;; (autoload 'stumpwm-mode "stumpwm-mode" nil t)

;; (use-package auto-complete              ;
;;   :ensure t
;;   :diminish auto-complete-mode
;;   :config	(ac-config-default))

;; (use-package ac-ispell
;;   :ensure t
;;   :diminish ac-ispell-mode
;;   :config
;;   (custom-set-variables
;;    '(ac-ispell-requires 4)
;;    '(ac-ispell-fuzzy-limit 4))

;;   ;; (eval-after-load "auto-complete"
;;   ;;   '(progn (ac-ispell-setup)))
;;   )

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

;; -(use-package insert-shebang
;;   :custom
;;   (insert-shebang-env-path "/bin/env" "env location on Arch")
;;   (insert-shebang-file-types
;;    '(("py" . "python") ("rkt" . "racket") ("sh" . "bash")))
;;   (insert-shebang-custom-headers
;;    '(("rkt" . "#lang racket")))
;;   :hook
;;   (after-save-hook #'make-this-file-executable))

;; (advice-add
;;  'ansi-color-apply-on-region
;;  :before 'ora-ansi-color-apply-on-region)

;; (defun ora-ansi-color-apply-on-region (begin end)
;;   "Fix progress bars for e.g. apt(8).
;;   Display progress in the mode line instead."
;;   (let ((end-marker (copy-marker end))
;;         mb)
;;     (save-excursion
;;       (goto-char (copy-marker begin))
;;       (while (re-search-forward "\0337" end-marker t)
;;         (setq mb (match-beginning 0))
;;         (when (re-search-forward "\0338" end-marker t)
;;           (ora-apt-progress-message
;;            (substring-no-properties
;;             (delete-and-extract-region mb (point))
;;             2 -2)))))))

;; (defun ora-apt-progress-message (progress)
;;   (setq mode-line-process
;;         (if (string-match
;;              "Progress: \\[ *\\([0-9]+\\)%\\]" progress)
;;             (list
;;              (concat ":%s "
;;                      (match-string 1 progress)
;;                      "%%%% "))
;;           '(":%s")))
;;   (force-mode-line-update))

(add-to-list 'display-buffer-alist
             '("^\\*shell\\*$" . (display-buffer-same-window)))

(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

(defun xpath-docs () (interactive)
(eww "https://www.guru99.com/xpath-selenium.html"))

(defun regex () (interactive)
(eww "https://docs.python.org/3.8/library/re.html"))

(defun find-pattern-in-dir ()
  (interactive)
  (let* ((dir-name (read-directory-name "Directory to look in: "))
         (extensions (seq-map 'file-name-extension (directory-files dir-name)))
         (file-pattern (read-from-minibuffer "Files to match: " "*org"))
         (grep-pattern (read-from-minibuffer "Grep pattern: ")))
    (shell-command (format "find %s -name  '%s' | xargs grep %s" dir-name file-pattern grep-pattern))))

(global-set-key (kbd "M-C-g")
                (lambda ()
                  (interactive)
                  (shell-command
                   (format "find /home/paul/projects/plekje/app/src/ -name  '*jsx' | xargs grep %s "
                           (buffer-substring-no-properties (region-beginning) (region-end))))))

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

;; (defun vulture ()
;;   (interactive))

(defun duplicate-region ()
  (interactive)
  (if (use-region-p)
      (let*
          ((very-end (save-excursion (goto-char (region-end)) (line-end-position)))
           (very-beginning (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
           (string-to-dup (buffer-substring
                           very-beginning very-end)
                          ))
        (goto-char very-end)
        (newline)
        (insert string-to-dup))
    (let ((string-to-dup (buffer-substring (line-beginning-position) (line-end-position))))
      (goto-char (line-end-position))
      (newline)
      (insert string-to-dup))))

(require 'wrap-region)
(wrap-region-global-mode)
(wrap-region-add-wrapper "`" "`")
(wrap-region-add-wrapper "'" "'")
(wrap-region-add-wrapper "|" "|")
;; (wrap-region-add-wrapper "=" "=")
(wrap-region-add-wrapper "$" "$")
(wrap-region-global-mode t)

(defun uva-print ()
  (interactive)
  (let ((filename (read-file-name "Choose file to print: ")))
    (message filename)
    (async-shell-command (format "/home/paul/.virtualenvs/scrape/bin/python /home/paul/projects/scrape/uva-print/uvaprint.py %s" filename))
))

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

;; (define-key image-mode-map "R" (lambda () (interactive) (find-file (buffer-file-name))))

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

(defun occur-selection ()
  (interactive)
  (when (region-active-p)
    (let (deactivate-mark)
      (occur (regexp-quote (buffer-substring (region-beginning) (region-end)))))))
(global-set-key [(meta o)] 'occur-selection)

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

;; (load-org "app-web.org")
