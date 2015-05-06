;;; Clojure --- Summary
;;; Commentary:
;;; Code:
(rename-modeline "clojure-mode" clojure-mode "Clj")

;;; Cider
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
;;(add-hook 'cider-repl-mode-hook 'enable-paredit-mode)
;;(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(setq nrepl-hide-special-buffers t)

;; Known endpoint for the workhorse machine
;;(setq cider-known-endpoints '(("workhorse" "10.0.1.10" "31415")))

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

(provide 'init-clojure)
;;; init-clojure.el ends here
