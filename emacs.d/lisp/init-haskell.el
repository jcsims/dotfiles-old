;;; Haskell --- Summary
;;; Commentary:
;;; Code:

(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook #'hindent-mode)

(setq-default haskell-tags-on-save t
              haskell-process-suggest-remove-import-lines t
              haskell-process-auto-import-loaded-modules t
              haskell-process-log t)

(provide 'init-haskell)
;;; init-haskell.el ends here
