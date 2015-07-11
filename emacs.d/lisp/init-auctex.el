;;; AUCTeX --- Summary
;; AUCTeX config

;;; Commentary:

;;; Code:
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(setq-default TeX-auto-save t
              TeX-parse-self t
              TeX-master nil
              TeX-PDF-mode t
              reftex-plug-into-AUCTeX t)
(when (eq system-type 'darwin) ;; mac-specific settings
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq-default TeX-source-correlate-method 'synctex)
  (setq-default TeX-view-program-list
                '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  (setq-default TeX-view-program-selection '((output-pdf "Skim"))))
(add-hook 'TeX-mode-hook
          (lambda ()
            (add-to-list
             'TeX-output-view-style
             '("^pdf$" "."
               "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))


;; Use latex-extra package
(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

(provide 'init-auctex)
;;; init-auctex.el ends here
