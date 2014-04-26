;;; init-haskell.el --- Init haskell goodness

;;; Commentary:
;; Various hooks for super Haskell power

;;; Code:
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

(define-key haskell-mode-map (kbd "C-c n") 'haskell-mode-stylish-buffer)

(provide 'init-haskell)
;;; init-haskell.el ends here
