;;; init-helm.el --- Summary
;;; Commentary:
;; helm config

;;; Code:
(require 'helm-config)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(helm-autoresize-mode 1)
(setq-default helm-recentf-fuzzy-match t
              helm-buffers-fuzzy-matching t
              helm-M-x-fuzzy-match t
              helm-semantic-fuzzy-match t
              helm-imenu-fuzzy-match t
              helm-apropos-fuzzy-match t
              helm-lisp-fuzzy-completion t
              helm-locate-fuzzy-match t)

(helm-mode 1)

;; Use helm for projectile
(require 'helm-projectile)
(helm-projectile-on)

(provide 'init-helm)
;;; init-helm.el ends here
