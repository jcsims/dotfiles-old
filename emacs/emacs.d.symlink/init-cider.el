;;; Cider
(use-package cider
  :ensure t
  :init
  (progn
    (add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
    (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)))

(provide 'init-cider)
;;; init-cider.el ends here
