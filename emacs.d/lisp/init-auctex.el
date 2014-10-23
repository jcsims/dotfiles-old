;;; AUCTeX --- Summary
;; AUCTeX config

;;; Commentary:

;;; Code:
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

;; Use latex-extra package
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(when (eq system-type 'darwin) ;; mac-specific settings
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-view-program-list
        '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq TeX-view-program-selection '((output-pdf "Skim"))))

(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-output-view-style
             '("^pdf$" "."
               "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))


(provide 'init-auctex)
;;; init-auctex.el ends here
