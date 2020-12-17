(defvar ctl-x-map-transient nil
  "Transient keymap for C-x commands.
The normal global definition of the character C-x indirects to this keymap.")
;; (define-prefix-command  (kbd "C-x")  ctl-x-map-transient)



(setq ctl-x-map-transient (let ((map (make-sparse-keymap)))
                            (define-key map "p" `move-windows)
                            (define-key map "n" `move-windows)
                            (define-key map "g" `move-windows)
                            (define-key map "0" 'delete-window)
                            (define-key map "q" 'delete-window)
                            (define-key map "1" 'delete-other-windows)
                            (define-key map "2" 'split-window-below)
                            (define-key map "3" 'split-window-right)
                            map))


(defun move-windows ()
  (interactive)
  (let* ((base (event-basic-type last-command-event))
         (step (pcase base
                 ;; (?p -1)
                 (?n 1)
                 (?g 0))))
    (if (not (= step 0))
        (progn
          (message "Use p and n to move back and forwards between windows, g to quit")
          (other-window step)
          (set-transient-map ctl-x-map-transient)
          ))))
;; (global-set-key (kbd "C-x p")  `move-windows)
(global-set-key (kbd "C-x n")  `move-windows)

(defun resize-window (inc)
  (interactive "p")
  (let* ((base (event-basic-type last-command-event))
           (step (pcase base
                   ((or ?f ?n) inc)
                   ((or ?b ?p) (- inc))))
           (horizontal (pcase base
                         ((or ?f ?b) t))))
      (enlarge-window step horizontal))
    (message "Use f,b,n,p to adjust window size")
    (set-transient-map (let ((map (make-sparse-keymap)))
    (define-key map "f" 'resize-window);;(lambda () (interactive "p") (resize-window 1)))
    (define-key map "b" 'resize-window)
    (define-key map "n" 'resize-window)
    (define-key map "p" 'resize-window)
    map)))

(global-set-key (kbd "C-x w f") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w b") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w n") (lambda () (interactive) (resize-window 1)))
(global-set-key (kbd "C-x w p") (lambda () (interactive) (resize-window 1)))

(follow-mode)
(global-set-key (kbd "C-x w u") 'winner-undo)
(global-set-key (kbd "C-x w r") 'winner-redo)
