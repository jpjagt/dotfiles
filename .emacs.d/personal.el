;; My identity
(setq user-full-name "Paul Lodder"
      user-mail-address "paul_lodder@live.nl")

;; (setq user-init-file "/home/paul/.emacs.d/init.el")
;; (setenv "HOME" "/mnt/mlp/home/paul/")
;; (setq home-directory "/mnt/mlp/home/paul/em")



(defun local ()
  (interactive)
  (setq default-directory "/home/paul"))

(defun lisa ()
  (interactive)
  (setq default-directory "/ssh:lodderp@lisa.surfsara.nl:"))


(global-set-key (kbd "C-x n") 'other-window) ; Alt+a
