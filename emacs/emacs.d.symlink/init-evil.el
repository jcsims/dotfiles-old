;;; Evil mode
;;; Commentary:

;;; Code:

;; Be evil!
(setq evil-want-C-u-scroll 1)
(evil-mode 1)
(evil-set-initial-state 'cider-repl-mode 'insert)
(evil-set-initial-state 'project-explorer-mode 'emacs)

;; Ensure that Esc quits pretty much everywhere
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'init-evil)
;;; init-evil.el ends here
