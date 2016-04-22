;;; Clojure --- Summary
;;; Commentary:
;;; Code:

(rename-modeline "clojure-mode" clojure-mode "Clj")

(require 'clojure-mode)
;;; Cider
;; Don't prompt for a symbol with `M-.`
(setq-default cider-prompt-for-symbol nil)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

;;(require 'ggtags)

;; (defun find-tag-without-ns ()
;;   "Un-namspace vars if needed, before trying to find them."
;;   (interactive)
;;   (ggtags-find-tag-dwim
;;    (first (last (split-string (symbol-name (symbol-at-point)) "/")))))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c r")
                               ;(define-key clojure-mode-map (kbd
                               ;"C-.") 'find-tag-without-ns)
                               ))


(setq nrepl-hide-special-buffers t)

;; Add some goodies from Emacs Live
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

;; Set up proper indentation for a few compojure functions
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (defroutes 'defun)
     (GET 2)
     (POST 2)
     (PUT 2)
     (DELETE 2)
     (HEAD 2)
     (ANY 2)
     (context 2)))

(require 'clj-refactor)
(setq-default cljr-suppress-middleware-warnings t)

(provide 'init-clojure)
;;; init-clojure.el ends here
