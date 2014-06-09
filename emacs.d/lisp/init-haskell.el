;;; init-haskell.el --- Init haskell goodness

;;; Commentary:
;; Various hooks for super Haskell power

;;; Code:
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-unicode-input-method)
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

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-<") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C->") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-compile)

     ;; new haskell-interactive-mode
     (define-key haskell-mode-map (kbd "C-x C-d") nil)
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c M-.") nil)
     (define-key haskell-mode-map (kbd "C-c C-d") nil)))

(eval-after-load "haskell-cabal"
  '(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-compile))

;; Set up ghc-mod
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(provide 'init-haskell)
;;; init-haskell.el ends here
