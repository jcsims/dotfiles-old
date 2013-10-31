;;; Clojure
(rename-modeline "clojure-mode" clojure-mode "Clj")

;;; Cider
(add-hook 'cider-interaction-mode-hook 'cider-turn-on-eldoc-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-strict-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)

(provide 'init-clojure)
;;; init-clojure.el ends here
