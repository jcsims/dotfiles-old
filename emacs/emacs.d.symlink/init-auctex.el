;; AUCTex settings
; Since the latest is installed from source,
; need to make sure it's loaded
(setq TeX-data-directory "~/code/auctex/")
(add-to-list 'load-path "~/code/auctex/")
(add-to-list 'load-path "~/code/auctex/preview/")
(add-to-list 'Info-directory-list "~/code/auctex/doc")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
