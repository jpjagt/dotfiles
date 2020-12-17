;; Magit
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status)
  :diminish magit-minor-mode)

;; Git Time Machine
;; (use-package git-timemachine
;;   :ensure t
;;   :defer 5
;;   :diminish git-timemachine-minor-mode)

(setq magit-diff-hide-trailing-cr-characters t)
(push '("/\\.gitmodules\\'" . conf-mode) auto-mode-alist)

;; https://chris.beams.io/posts/git-commit/
(setq  git-commit-summary-max-length 50
       git-commit-fill-column 72)

;; (use-package vc-fossil)
