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
          (smartparens  . "melpa-stable"))))

(defvar jcs-package-list '() "Packages that should be installed.")
;; Install packages, if they're not already installed
(setq jcs-package-list
      '(auctex
        cider
        clj-refactor
        clojure-mode
        company
        company-auctex
	company-quickhelp
        csv-mode
        diminish
        elisp-slime-nav
        exec-path-from-shell
        expand-region
        flycheck
        git
        git-commit-mode
        git-gutter
        git-rebase-mode
        helm
        helm-ag
        helm-projectile
        idle-highlight-mode
        js2-mode
        latex-extra
        magit
        markdown-mode
        nginx-mode
        paradox
        smartparens
        projectile
        rainbow-delimiters
        slamhound
        smart-mode-line
        zenburn-theme))

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package jcs-package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
