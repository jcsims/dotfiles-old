;;; Auto-complete
(use-package auto-complete :ensure t)

(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'nrepl-mode)

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
