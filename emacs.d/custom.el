(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "70b51a849b665f50a97a028c44cec36b398398357d8f7c19d558fe832b91980f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(js-bounce-indent-p t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (nlinum flycheck-joker paradox diff-hl ob-restclient alchemist go-mode multiple-cursors robe git-timemachine restclient projectile yaml-mode which-key sql-indent solarized-theme smex smart-mode-line rainbow-delimiters paredit-everywhere page-break-lines monokai-theme markdown-mode magit latex-extra js2-mode idomenu ido-ubiquitous idle-highlight-mode hi2 git-gutter flycheck flx-ido expand-region exec-path-from-shell elisp-slime-nav dumb-jump dockerfile-mode diminish csv-mode company-quickhelp company-auctex color-theme-sanityinc-tomorrow clj-refactor browse-kill-ring ag)))
 '(paradox-automatically-star t)
 '(paradox-github-token t))

(add-hook 'kill-emacs-query-functions
          'custom-prompt-customize-unsaved-options)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
