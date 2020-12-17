(load-file "~/.emacs.d/prettify-utils.el")
  (defun read-lines (filePath)
    "Return a list of lines of a file at filePath."
    (with-temp-buffer
      (insert-file-contents filePath)
      (split-string (buffer-string) "\n" t)))

  (defun strip-text-properties (txt)
    (set-text-properties 0 (length txt) nil txt)
    txt)


  (defun re-seq (regexp string)
    "Get a list of all regexp matches in a string"
    (save-match-data
      (let ((pos 0)
            matches)
        (while (string-match regexp string pos)
          (push (match-string 0 string) matches)
          (setq pos (match-end 0)))
        matches)))

  (defun replace-in-string (what with in)
    (replace-regexp-in-string (regexp-quote what) with in nil 'literal))


  (defun get-prettify-list-for-buffer ()
    (setq tex-expr "[\\][0-9a-z{}\\]+")
    (setq unicode-command "~/.virtualenvs/py3.8/bin/python ~/.emacs.d/write_unicode_for_commands.py")
    ;; (setq tex-expr "\\[\w|\{\}]+")

    (setq commands (seq-map (lambda (x) (strip-text-properties x)) (seq-filter
                                                                    (lambda (s)
                                                                      (string-prefix-p
                                                                       "\\" s))
                                                                    (delete-dups
                                                                     (re-seq
                                                                      tex-expr
                                                                      (buffer-string))))))
    (setq comma-sep-commands  (mapconcat 'identity  commands ","))
    (setq command-to-exec (string-join (list unicode-command comma-sep-commands) " "))
    (shell-command (replace-in-string "\\" "\\\\" command-to-exec))
    (message (replace-in-string "\\" "\\\\" command-to-exec))

    (setq command-symbol-pairs
          (seq-map (lambda (x) (split-string
                                x ",")) (read-lines "/tmp/symbols")))
    (setq out (seq-map (lambda (el) (first (prettify-utils-generate-f
                                            el))) command-symbol-pairs))
    (seq-map (lambda (x) (if (= (length x) 2) (apply #'cons x) x)) out))

;; (add-hook `prettify-symbols-mode-hook (lambda ()
;;                                         (setq prettify-symbols-alist
;;                                         (get-prettify-list-for-buffer))))
