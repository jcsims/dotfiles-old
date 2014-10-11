;;; init-helm.el --- Summary
;;; Commentary:
;; helm config

;;; Code:
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(helm-mode 1)

;; Use helm for projectile
(require 'helm-projectile)
(helm-projectile-on)

(provide 'init-helm)
;;; init-helm.el ends here
