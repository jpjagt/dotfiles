(wrap-region-global-mode)
(wrap-region-add-wrapper "`" "`")
(wrap-region-add-wrapper "'" "'")
(wrap-region-add-wrapper "|" "|")
;; (wrap-region-add-wrapper "=" "=")
(wrap-region-add-wrapper "$" "$")
(wrap-region-global-mode t)

;; (modify-syntax-entry ?' "'")
;; (modify-syntax-entry ?^ "('")
;; (modify-syntax-entry ?$ ")'")
;;   ;; (modify-syntax-entry ?' "'")
;;   ;;  (modify-syntax-entry ?' "^'")

;; (add-hook 'commint-mode-hook
;;           (lambda () (setenv "PAGER" "cat")))
;;   (add-to-list
;;            'comint-preoutput-filter-functions
;;            (lambda (output)
;;              (replace-regexp-in-string "\\[[0-9]+[GK]" "" output)))
