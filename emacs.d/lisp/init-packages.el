;;; Packages --- Summary
;; Handle package management


;;; Commentary:
;; Start the package machinery, as well as keep a list of packages that
;; need to be installed.

;;; Code:

(require 'package)

(when (>= emacs-major-version 24)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/"))))

(package-initialize)

;; Pin a few packages
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((cider        . "melpa-stable")
          (clj-refactor . "melpa-stable")
          (clojure-mode . "melpa-stable")
          (paradox      . "melpa-stable")
          (async        . "melpa-stable")
          (magit        . "melpa-stable"))))

;; Install packages, if they're not already installed
(defvar jcs-package-list
  '(ag
    async
    auctex
    ;;browse-kill-ring
    cider
    clj-refactor
    clojure-mode
    company
    company-auctex
    company-quickhelp
    csv-mode
    diminish
    elisp-slime-nav
    ;;ess
    exec-path-from-shell
    expand-region
    ;;flx-ido
    flycheck
    git-gutter
    helm
    helm-ag
    helm-projectile
    idle-highlight-mode
    ;;idomenu
    ;;ido-ubiquitous
    js2-mode
    latex-extra
    magit
    markdown-mode
    monokai-theme
    paradox
    paredit
    paredit-everywhere
    projectile
    rainbow-delimiters
    smart-mode-line
    ;;smex
    sql-indent)
  "Packages that should be installed.")

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package jcs-package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
