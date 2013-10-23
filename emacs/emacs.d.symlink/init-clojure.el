;;; Clojure
(use-package clojure-mode
  :ensure t
  :init (rename-modeline "clojure-mode" clojure-mode "Clj"))

(require 'init-cider)
;; (use-package nrepl
;;   :ensure t
;;   :init (add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode))

(use-package clojurescript-mode :ensure t)

(use-package clojure-test-mode :ensure t)

(provide 'init-clojure)
;;; init-clojure.el ends here
