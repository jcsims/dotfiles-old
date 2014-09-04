;;; init-helm.el --- Summary
;;; Commentary:
;; helm config

;;; Code:
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)

(provide 'init-helm)
;;; init-helm.el ends here
