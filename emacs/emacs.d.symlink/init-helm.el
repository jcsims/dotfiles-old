;;; init-helm --- Configure smartparens
;;; Commentary:

;;; Code:
(require 'helm-config)
(setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
(setq helm-ag-command-option "--all-text")
(setq helm-ag-thing-at-point 'symbol)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(helm-mode 1)
(provide 'init-helm)
;;; init-helm.el ends here
