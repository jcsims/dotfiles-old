;;; AUCTex settings
;; If the latest is installed from source,
;; use that
(if (file-directory-p "~/code/auctex")
    (progn
      (setq TeX-data-directory "~/code/auctex/")
      (add-to-list 'load-path "~/code/auctex/")
      (add-to-list 'load-path "~/code/auctex/preview/")
      (add-to-list 'Info-directory-list "~/code/auctex/doc")
      (load "auctex.el" nil t t)
      (load "preview-latex.el" nil t t))
  (install-package 'auctex))



(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; I always use pdf
(setq TeX-PDF-mode t)

(provide 'init-auctex)
;;; init-auctex.el ends here
