;;; init-evil.el --- Summary
;;; Commentary:
;; Let the evil flow

;;; Code:
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)
(setq evil-want-C-u-scroll t)
(require 'evil)

;; Set some initial states
(evil-set-initial-state 'inferior-emacs-lisp-mode 'emacs)
(evil-set-initial-state 'cider-repl-mode 'emacs)
(evil-set-initial-state 'comint-mode 'normal)
(evil-set-initial-state 'shell-mode 'insert)
(evil-set-initial-state 'git-commit-mode 'insert)
(evil-set-initial-state 'git-rebase-mode 'emacs)
(evil-set-initial-state 'term-mode 'emacs)
(evil-set-initial-state 'help-mode 'emacs)
(evil-set-initial-state 'helm-grep-mode 'emacs)
(evil-set-initial-state 'grep-mode 'emacs)
(evil-set-initial-state 'bc-menu-mode 'emacs)
(evil-set-initial-state 'magit 'emacs)
(evil-set-initial-state 'dired-mode 'emacs)
(evil-set-initial-state 'project-explorer-mode 'emacs)
(evil-set-initial-state 'wdired-mode 'normal)
(evil-set-initial-state 'inferior-ess-mode 'emacs)

;;; Key bindings
;; 'q' is used to close many buffers in Emacs, so we'll use the
;; default F3-F4 for macros instead
(define-key evil-normal-state-map (kbd "q") nil)

(evil-mode t)

(provide 'init-evil)
;;; init-evil.el ends here
