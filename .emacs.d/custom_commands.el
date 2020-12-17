(global-set-key (kbd "C-x C-b") 'buffer-menu)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))
