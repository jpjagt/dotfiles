(defun local ()
  (interactive)
  (setq default-directory "/home/paul"))

(defun reset-default-directory ()
  (interactive)
  (setq-local  default-directory (file-name-directory (buffer-file-name))))
