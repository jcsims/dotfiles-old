;;; Evil mode
;;; Commentary:

;;; Code:

;; Be evil!
(setq evil-want-C-u-scroll 1)
(evil-mode 1)
(evil-set-initial-state 'cider-repl-mode 'insert)
(evil-set-initial-state 'project-explorer-mode 'emacs)

;; evil-paredit is fairly handicapped for now -
;; it just tells you when you can't do something
(add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode)
(add-hook 'clojure-mode-hook 'evil-paredit-mode)

(provide 'init-evil)
;;; init-evil.el ends here
