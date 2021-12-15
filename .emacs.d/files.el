(defun remove-ssh-prefix (filename)
  "Removes ssh-prefix of filenames taken from remote locations"
  (first (last (split-string filename ":")))
  )

(defun touch ()
  (interactive)
  (let* ((to-touch (read-file-name "Filename to touch: " ))
         (to-touch (remove-ssh-prefix to-touch))
         (command (format "touch %s" to-touch)))
    (if (string-prefix-p "/ssh:" default-directory)
        (progn (message "AA")
               (tramp-handle-shell-command command))
      (shell-command command))))

(defun open-file-region ()
  (interactive)
  (let* ((path-selected (buffer-substring (region-beginning) (region-end)))
         (url-p (<= (length (first (last (split-string path-selected "\\.")))) 3))
         )
    ;; (message  (split-string path-selected "\\." ))
    (cond ((file-exists-p path-selected) (find-file-other-window path-selected))
          (url-p  (shell-command (format "google-chrome %s" path-selected)))
          (t (error (format "File %s does not exist" path-selected)))
          )
    )
  )


    ;; (if (and  (not url-p))

    ;;   )))
;; (global-set-key (kbd "C-M-o") 'open-file-region)
(global-set-key (kbd "C-M-o") 'ffap)



(defun insdate-insert-current-date (&optional omit-day-of-week-p)
  "Insert today's date using the current locale.
  With a prefix argument, the date is inserted without the day of
  the week."
  (interactive "P*")
  (calendar-date-string (calendar-current-date) nil
                        omit-day-of-week-p))

(defun my/org-template ()
  (let ((session-name (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
    (message session-name)
    (insert
     (format  "#+TITLE: %s
#+BIND: org-export-use-babel nil
#+AUTHOR: jeroen jagt
#+EMAIL: <jpjagt@pm.me>
#+DATE: %s
#+LATEX: \\setlength\\parindent{0pt}
#+LATEX_HEADER: \\usepackage{minted}
#+LATEX_HEADER: \\usepackage[margin=1.2in]{geometry}
#+LATEX_HEADER: \\usepackage{mathpazo}
#+LATEX_HEADER: \\usepackage{adjustbox}
#+LATEX_HEADER_EXTRA:  \\usepackage{mdframed}
#+LATEX_HEADER_EXTRA: \\BeforeBeginEnvironment{minted}{\\begin{mdframed}}
#+LATEX_HEADER_EXTRA: \\AfterEndEnvironment{minted}{\\end{mdframed}}
#+LATEX_HEADER_EXTRA: \\BeforeBeginEnvironment{tabular}{\\begin{adjustbox}{center}}
#+LATEX_HEADER_EXTRA: \\AfterEndEnvironment{tabular}{\\end{adjustbox}}
#+MACRO: NEWLINE @@latex:\\\\@@ @@html:<br>@@
#+PROPERTY: header-args :exports both :session %s :cache :results value
#+OPTIONS: ^:nil
#+LATEX_COMPILER: pdflatex" session-name (insdate-insert-current-date t) session-name)
     ;; (org-mode-restart)
     )))
(define-auto-insert "\\.org$" #'my/org-template)
  ;; ))

;; doesn't work because if a shell already exists, doesn't change dir
;; accordingly
(defun shell-here () (interactive)
       (let ((default-directory (file-name-directory (buffer-file-name))))
         (message (file-name-directory (buffer-file-name)))
         (shell)))

;; (defun shell-dir (name dir)
;;   (interactive "sShell name: \nDDirectory: ")
;;   (let ((default-directory dir))
;;     (shell name)))

(defun get-current-fpath ()
  (let* ((filename-raw (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
         (filename (if (string-prefix-p "/ssh:" filename-raw) (remove-ssh-prefix filename-raw) filename-raw)))
    filename))

(defun cpath ()
  "Copy the current buffer full path to the clipboard."
  (interactive)
  (let* ((filename (get-current-fpath)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))


(defun cdir ()
  "Copy the current buffer full path to the clipboard."
  (interactive)
  (let* ((filename-raw (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
         (filename (file-name-directory (if (string-prefix-p "/ssh:" filename-raw) (remove-ssh-prefix filename-raw) filename-raw))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun cfile ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let* ((filepath-raw (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name)))
         (filename (file-name-nondirectory filepath-raw)))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))
