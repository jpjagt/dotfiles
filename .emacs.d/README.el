(defun load-org (f)
  (org-babel-load-file (concat user-emacs-directory f)))

(load-org "files.org")
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-auto-unix)

;; ;; Full path of buffer in mode-line
(setq uniquify-buffer-name-style 'forward)

(load-org "jpj.org")
