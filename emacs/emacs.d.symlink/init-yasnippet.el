;;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (progn
    (yas-global-mode 1)
    (setq yas/root-directory '("~/.emacs.d/snippets"))
    (mapc 'yas/load-directory yas/root-directory)))

(use-package clojure-snippets :ensure t)
(use-package datomic-snippets :ensure t)
(use-package el-autoyas :ensure t)
(use-package r-autoyas :ensure t)


(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
