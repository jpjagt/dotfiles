;; ;; (add-to-list 'load-path "~/.emacs.d/js")
;; (require 'js-comint)

;; ;; (defun js-send-region-or-buffer
;; ;; "Send current region, if not active send whole buffer"
;; ;;   (interactive)
;; ;;   (let* ((str (if (region-active-p)
;; ;;                   (buffer-substring-no-properties (region-beginning) (region-end))
;; ;;                 (buffer-string))))
;; ;;     (js-comint-send-string str)))
;; (defun js-comint-send-region-or-buffer ()
;;   "Send the current region to the inferior Javascript process.
;; If no region selected, you could manually input javascript expression."
;;   (interactive)
;;   (let* ((str (if (region-active-p)
;;                   (buffer-substring-no-properties (region-beginning) (region-end))
;;                 (buffer-string))))
;;     (message "str=%s" str)
;;     (js-comint-send-string str)))

;; (add-hook 'js-mode-hook
;; (lambda ()
;; (local-set-key (kbd "C-c C-p") 'js-comint-repl)
;; (local-set-key (kbd "C-x C-e") 'js-send-last-sexp)
;; ;; (local-set-key (kbd "C-c C-c") 'js-comint-send-region-or-buffer)))

;;   ;; (defcustom jsshell-profile
;;   ;;   (list "c:\\dev\\js\\json2.js" )
;;   ;;   "List of filenames, each one a Javascript source file. This module
;;   ;; will load each JS module into the jsshell as the jsshell starts."
;;   ;;   :group 'jsshell)
;; ;; "List of modules paths which could be used by NodeJS to search modules.")
;;   ;; (defvar js-comint-module-paths '("/home/paul/projectjes/gcvenn/node_modules/jquery/src/" "/home/paul/node_modules)
;;   ;;   "List of modules paths which could be used by NodeJS to search modules.")
;; (defvar js-comint-module-paths '()
;; "List of modules paths which could be used by NodeJS to search modules.")

;; (add-to-list
;;          'comint-preoutput-filter-functions
;;          (lambda (output)
;;            (replace-regexp-in-string "\\[[0-9a-zA-Z]+[GK]" "" output)))

;; (setq indium-chrome-executable "/usr/bin/chromium")
;; ;; (defun indium-chrome--default-executable ()
;; ;;   "Return a default executable based on the OS."
;; ;;   (cond ((string-equal system-type "darwin")
;; ;;          "/Applications/Google Chrome.app/Contents/MacOS/Google Chrome")
;; ;;         ((string-equal system-type "windows-nt")
;; ;;          "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")
;; ;;         (t "/usr/bin/chromium")))

;; (add-to-list 'exec-path "/usr/bin/")
;; (add-hook 'js2-mode-hook (lambda ()
;;                               (global-set-key (kbd "C-c C-c") (indium-reload))))
;; (add-hook 'indium-interaction-mode-hook (lambda ()
;;                               (global-set-key (kbd "C-c C-c") (indium-reload))))

;;  ;; (defun indium-start ()
;;  ;;   (interactive)
;;  ;;   (if (get-buffer "*Shell Command Output*") ""
;;  ;;     (shell-command "chromium-browser --remote-debugging-port=9222 http:localhost:8000"))
;;  ;;   (indium-launch))

(push "~/.node_modules_global/bin" exec-path)
(load-file "~/.emacs.d/prettier-emacs/prettier-js.el")
(add-hook 'js-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook 'prettier-js-mode)
(eval-after-load 'js-mode (lambda ()
                            (define-key js-mode-map (kbd "C-c C-c")  'indium-reload)))



(add-hook 'js-mode-hook (lambda ()
  (web-mode-set-content-type "jsx")))
(require 'js-format)
  (eval-after-load 'js-mode
    (add-hook 'js-mode-hook
              (lambda()
                (js-format-setup "prettier"))))
(custom-set-variables
  '(js-auto-format-command "prettier")
  '(js-auto-format-command-args "--write --single-quote --no-semi"))
  ;; using "jsbeautify-css" as css formatter
  (eval-after-load 'css-mode
    (add-hook 'css-mode-hook
              (lambda()
                (js-format-setup "jsb-css"))))
(defun format-buffer ()
  "Sync org file to Raspberry Pi with external script."
  (when (eq major-mode 'js2-mode)
    (lambda () nil)))

(add-hook 'after-save-hook #'format-buffer)

  (add-hook `js2-mode-hook (lambda ()
  (global-set-key (kbd "C-c C-v") #'js-format-buffer)))


  (add-hook 'js2-mode-hoook (lambda ()
                              (make-variable-buffer-local 'after-save-hook)
                              (add-hook 'after-save-hook 'special-hook)))

(defun quick-replace-no-memory (search repl)
  (goto-char (point-min))
  (while (search-forward search nil t)
    (replace-match repl)
  ;; repeat for other string pairs
  ))
(defun comment-all-logs ()
  (interactive)
  (goto-char (point-min))
  (save-excursion
    (while (search-forward "console." nil t)
         (if (null (nth 4 (syntax-ppss)))
       (save-excursion

         (replace-match "console_tmp.")
                       (call-interactively 'backward-sexp)
                    (call-interactively 'mark-sexp)
                    (call-interactively 'mark-sexp)
                    (call-interactively 'mark-sexp)
                    (call-interactively 'comment-region)
                    ))
    )))

;; (js2-mode-toggle-warnings-and-errors)

(load-file "~/.emacs.d/prettier-emacs/prettier-js.el")
(defun my/activate-tide-mode ()
  "Use hl-identifier-mode only on js or ts buffers."
  (when (and (stringp buffer-file-name)
             (string-match "\\.[tj]sx?\\'" buffer-file-name))
    (tide-setup)
    (tide-hl-identifier-mode)))

(use-package web-mode
  :ensure t
  :mode
  ("\\.ejs\\'" "\\.hbs\\'" "\\.html\\'" "\\.php\\'" "\\.[jt]sx?\\'")
  :config
  (setq web-mode-content-types-alist '(("jsx" . "\\.[jt]sx?\\'")))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-script-padding 2)
  (setq web-mode-block-padding 2)
  (setq web-mode-style-padding 2))
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-current-element-highlight t)

(use-package tide
   :hook (web-mode . my/activate-tide-mode)
   :ensure t)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)

(add-hook 'rjsx-mode-hook (lambda ()
                            (progn (prettier-js-mode)
                                   ;; (setq js2-mode-show-strict-warnings nil)
                                   ;; (setq-local forward-sexp-function `nil)
                                   )))
