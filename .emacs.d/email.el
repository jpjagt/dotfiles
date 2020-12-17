(add-to-list `load-path "/usr/local/share/emacs/site-lisp/mu4e")
   (require `mu4e)
   (setq mu4e-maildir "~/.mails/live")
   (setq mu4e-contexts
         `( ,(make-mu4e-context
      :name "live"
      :match-func (lambda (msg) (when msg
        (string-prefix-p "/live" (mu4e-message-field msg :maildir))))
      :vars '(
        (mu4e-trash-folder . "/live/[live].Trash")
        (mu4e-refile-folder . "/Gmail/[live].Archive")
        ))
            ,(make-mu4e-context
              :name "plekje"
              :match-func (lambda (msg) (when msg
        (string-prefix-p "/plekje" (mu4e-message-field msg :maildir))))
      :vars '(
        (mu4e-trash-folder . "/plekje/[live].Trash")
        (mu4e-refile-folder . "/plekjex/[live].Archive")
        ))
    ))

(setq mu4e-sent-folder "/sent"
        ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
        mu4e-drafts-folder "/drafts"
        user-mail-address "paul_lodder@live.nl"
        smtpmail-default-smtp-server "smtp-mail.outlook.com"
        smtpmail-smtp-server "smtp-mail.outlook.com"
        smtpmail-smtp-service 587)
(defvar my-mu4e-account-alist
  '(("live"
     (mu4e-sent-folder "/sent")
     (user-mail-address "paul_lodder@live.nl")
     (smtpmail-smtp-user "paul_lodder@live.nl")
     (smtpmail-local-domain "outlook.com")
     (smtpmail-default-smtp-server "xosmtp-mail.outlook.com")
     (smtpmail-smtp-server "smtp-mail.outlook.com")
     (smtpmail-smtp-service 587)
     )
    ("plekje"
     (mu4e-sent-folder "/sent")
     (user-mail-address "info@plekje.nu")
     (smtpmail-smtp-user "info@plekje.nu")
     (smtpmail-local-domain "plekje.nu")
     (smtpmail-default-smtp-server "smtp.transip.email")
     (smtpmail-smtp-server "smtp.transip.email")
     (smtpmail-smtp-service 465)
     )
     ;; Include any other accounts here ...
    ))
    (setq mu4e-update-interval 60

    mu4e-get-mail-command "offlineimap -a live -f Inbox,Spam,Sent")
(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
          (if mu4e-compose-parent-message
              (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                (string-match "/\\(.*?\\)/" maildir)
                (match-string 1 maildir))
            (completing-read (format "Compose with account: (%s) "
                                     (mapconcat #'(lambda (var) (car var))
                                                my-mu4e-account-alist "/"))
                             (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
                             nil t nil nil (caar my-mu4e-account-alist))))
         (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
        (mapc #'(lambda (var)
                  (set (car var) (cadr var)))
              account-vars)
      (error "No email account found"))))
;; (add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

(defun my-render-html-message ()
  (let ((dom (libxml-parse-html-region (point-min) (point-max))))
    (erase-buffer)
    (shr-insert-document dom)
    (goto-char (point-min))))

    ;; (setq mu4e-view-show-images t)

    ;; use imagemagick, if available
    ;; (when (fboundp 'imagemagick-register-types)
    ;; (imagemagick-register-types))
(setq mu4e-html2text-command 'my-render-html-message)
  (setq mu4e-hide-index-message 1)
  (setq mu4e-hide-index-messages 1)
  (setf  mu4e-display-index-messages   nil)
(eval-after-load 'mu4e-utils
  '(defun mu4e-info-handler (info)))
;; (add-hook `after-change-major-mode-hook `my-after-change-major-mode-prog-mode)
;; (defun my-after-change-major-mode-prog-mode ()
;;   "Custom `after-change-major-mode-hook' behaviours."
;;   ;; (when (derived-mode-p `mu4e-compose-mode-hook)
;;   ;;   (lambda ()
;;   ;;                                   (message "JAWEL here")
;;   ;;                                   (auto-fill-mode -1)
;;   ;;                                   (visual-line-mode)))
;;   )
  (add-hook 'mu4e-compose-mode-hook (lambda ()
                                                      ;;  `auto-fill-function t)
                                    (auto-fill-mode -1)
                                    (visual-line-mode)))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
    (concat
     "flag:unread maildir:/live/Inbox "
     "OR "
     "flag:unread maildir:/ml/INBOX"
     ))
  (mu4e-alert-enable-mode-line-display)

  ;; (defun gjstein-refresh-mu4e-alert-mode-line ()
  ;;   (interactive)
  ;;   (mu4e~proc-kill)
  ;;   (mu4e-alert-enable-mode-line-display)
  ;;   )
  ;; (run-with-timer 0 3 'gjstein-refresh-mu4e-alert-mode-line)
  )

    ;; (use-package mu4e-alert
    ;;   :ensure t
    ;;   :after mu4e
    ;;   :init
    ;;   (setq mu4e-alert-interesting-mail-query
    ;;     (concat
    ;;      "flag:unread maildir:/live/INBOX"
    ;;      ))
    ;;   (mu4e-alert-enable-mode-line-display)
    ;;   (defun gjstein-refresh-mu4e-alert-mode-line ()
    ;;     (interactive)
    ;;     (mu4e~proc-kill)
    ;;     (mu4e-alert-enable-mode-line-display)
    ;;     )
    ;;   (run-with-timer 0 60 'gjstein-refresh-mu4e-alert-mode-line)
    ;;   )

  ;; (defun my-mu4e-html2text (msg)
  ;;   "My html2text function; shows short message inline, show
  ;; long messages in some external browser (see `browse-url-generic-program')."
  ;;   (let ((html (or (mu4e-message-field msg :body-html) "")))
  ;;     (if (> (length html) 20000)
  ;;       (progn
  ;;   (mu4e-action-view-in-browser msg)
  ;;   "[Viewing message in external browser]")
  ;;       (mu4e-shr2text msg))))

  ;; (setq mu4e-html2text-command 'my-mu4e-html2text)
