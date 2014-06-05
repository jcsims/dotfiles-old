;;; init-evil.el --- Summary
;;; Commentary:
;; Let the evil flow

;;; Code:
(setq evil-search-module 'evil-search)
(setq evil-magic 'very-magic)
(setq evil-want-C-u-scroll t)

(require 'evil)

(evil-mode t)

(provide 'init-evil)
;;; init-evil.el ends here
