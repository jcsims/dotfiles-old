;;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-to-list 'ac-modes 'nrepl-mode)
(add-to-list 'ac-sources 'ac-source-ghc-mod)
(setq ac-use-fuzzy t)
(ac-flyspell-workaround)

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here