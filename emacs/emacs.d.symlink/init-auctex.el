;;; AUCTex settings
(use-package auctex
  :ensure t
  :init
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)))

(provide 'init-auctex)
;;; init-auctex.el ends here
