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

(use-package latex-extra :ensure t)
(eval-after-load 'latex '(latex/setup-keybinds))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
;; I always use pdf
(setq TeX-PDF-mode t)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(setq reftex-plug-into-AUCTeX t)

;; Set up auto-complete sources
(require 'ac-math)

(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

(defun ac-latex-mode-setup ()         ; add ac-sources to default ac-sources
  (setq ac-sources
     (append '(ac-source-math-unicode 
               ac-source-math-latex
               ac-source-latex-commands)
               ac-sources)))

(add-hook 'latex-mode-hook 'ac-latex-mode-setup)

(provide 'init-auctex)
;;; init-auctex.el ends here
