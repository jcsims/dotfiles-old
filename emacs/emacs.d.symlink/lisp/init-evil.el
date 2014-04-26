;;; Evil mode
;;; Commentary:

;;; Code:
(setq evil-want-C-u-scroll 1)
(require 'evil)

;; Be evil!
(global-evil-leader-mode)
(evil-mode 1)
(evil-set-initial-state 'cider-repl-mode 'insert)
(evil-set-initial-state 'project-explorer-mode 'emacs)

(defun edit-init-file ()
  "Edit user's init file"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun edit-todo-file ()
  "Edit user's todo file"
  (interactive)
  (find-file "~/org/todo.org"))

;; Transfer some common leader keybindings from vim
(evil-leader/set-key
  "ei" 'edit-init-file
  "et" 'edit-todo-file
  "z" 'previous-buffer
  "x" 'next-buffer)

;;; esc quits

(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'init-evil)
;;; init-evil.el ends here
