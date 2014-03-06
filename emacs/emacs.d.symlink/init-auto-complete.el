;;; Auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)

(add-hook 'cider-repl-mode-hook 'ac-nrepl-setup)
(add-hook 'cider-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'cider-repl-mode))

(eval-after-load "cider"
  '(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))
(setq ac-use-fuzzy t)
(ac-flyspell-workaround)

(provide 'init-auto-complete)
;;; init-auto-complete.el ends here
