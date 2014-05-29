;;; init-haskell.el --- Init haskell goodness

;;; Commentary:
;; Various hooks for super Haskell power

;;; Code:
(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;; Add some easy align regexp's
;; (add-to-list 'align-rules-list
;;              '(haskell-types
;;                (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-assignment
;;                (regexp . "\\(\\s-+\\)=\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-arrows
;;                (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))
;; (add-to-list 'align-rules-list
;;              '(haskell-left-arrows
;;                (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
;;                (modes quote (haskell-mode literate-haskell-mode))))


;;(speedbar-add-supported-extension ".hs")

(define-key haskell-mode-map (kbd "C-c n") 'haskell-mode-stylish-buffer)

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

(provide 'init-haskell)
;;; init-haskell.el ends here
