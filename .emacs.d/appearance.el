;; Enable pretty syntax highlighting everywhere
(global-font-lock-mode t)

(use-package whitespace
  :ensure t
  :init (setq-default indicate-empty-lines t)
  :config
  (setq whitespace-line-column 80
        whitespace-style '(face trailing lines-tail empty
                                indentation::space space-before-tab::tab))
  (global-whitespace-mode -1))

(setq-default indent-tabs-mode nil)

(defcustom do-whitespace-cleanup t
  "Perform whitespace-cleanup on save."
  :group 'whitespace)

(make-variable-buffer-local 'do-whitespace-cleanup)

(defun toggle-whitespace-cleanup ()
  "Turn the whitespace-cleanup hook on and off."
  (interactive)
  (setq do-whitespace-cleanup (not do-whitespace-cleanup))
  (message "do-whitespace-cleanup set to %s" do-whitespace-cleanup))

(add-hook 'before-save-hook
          (lambda ()
            (when do-whitespace-cleanup
              (whitespace-cleanup))))

(add-hook 'makefile-mode-hook
          (lambda ()
            (setq indent-tabs-mode t
                  do-whitespace-cleanup nil)))

(add-hook 'prog-mode-hook
          (lambda ()
            (whitespace-mode +1)
            ;; (setq show-trailing-whitespace t)
            ))

;; ;; make the fringe stand out from the background
;; (setq solarized-distinct-fringe-background t)

;; ;; Don't change the font for some headings and titles
;; (setq solarized-use-variable-pitch nil)

;; ;; make the modeline high contrast
;; (setq solarized-high-contrast-mode-line t)

;; ;; Use less bolding
;; (setq solarized-use-less-bold t)

;; ;; Use more italics
;; (setq solarized-use-more-italic t)

;; ;; Use less colors for indicators such as git:gutter, flycheck and similar
;; (setq solarized-emphasize-indicators nil)

;; ;; Don't change size of org-mode headlines (but keep other size-changes)
;; (setq solarized-scale-org-headlines nil)

;; ;; Avoid all font-size changes
;; (setq solarized-height-minus-1 1.0)
;; (setq solarized-height-plus-1 1.0)
;; (setq solarized-height-plus-2 1.0)
;; (setq solarized-height-plus-3 1.0)
;; (setq solarized-height-plus-4 1.0)
(setq solarized-high-contrast-mode-line t)
(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark-high-contrast t))

(defun light ()
  (interactive)
  (load-theme 'solarized-light-high-contrast t))

(defun light-low-contrast ()
  (interactive)
  (load-theme 'solarized-light t))



(defun dark-low-contrast ()
  (interactive)
  (load-theme 'solarized-dark t))

(defun dark ()
  (interactive)
  (load-theme 'solarized-dark-high-contrast t))
(setq solarized-high-contrast-mode-line t)



;; (set-frame-font "Inconsolata-12")
;; (set-frame-font "Source Code Pro 10")
;; (set-frame-font "Iosevka 12")

;; (set-frame-font "-*-Iosevka-normal-normal-normal-*-*-*-*-*-d-0-iso10646-1")
;; (set-face-attribute 'default (selected-frame) :height 110)
;; "Alte DIN 1451 Mittelschrift gepraegt"
;; (font-family-list)

;; (add-to-list
;;  'default-frame-alist
;;  '(font . "-*-Iosevka-normal-normal-normal-*-16-*-*-*-d-0-iso10646-1"))e

(setq line-number-mode t
      column-number-mode t)

(setq battery-mode-line-format "[%b%t]")
(display-battery-mode)

(tool-bar-mode 0)
  (menu-bar-mode 0)
  (scroll-all-mode 0)
  (scroll-bar-mode 0)
  (tooltip-mode )
  ;; (if window-system
  ;;     (progn
  ;; )

(load-org "prettify.org")
