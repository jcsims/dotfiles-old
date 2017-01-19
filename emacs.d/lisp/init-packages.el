;;; Packages --- Summary
;; Handle package management


;;; Commentary:
;; Start the package machinery, as well as keep a list of packages that
;; need to be installed.

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ;;("org"          . "https://orgmode.org/elpa/")
        ))

;; Install packages, if they're not already installed
(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)

(provide 'init-packages)
;;; init-packages.el ends here
