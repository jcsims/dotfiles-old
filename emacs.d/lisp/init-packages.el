;;; Packages --- Summary
;; Handle package management

;;; Commentary:
;; Start the package machinery, as well keep a list of packages that
;; need to be installed.

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" .
                                 "http://stable.melpa.org/packages/"))
(package-initialize)

;; Pin a few packages
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clj-refactor . "melpa-stable") t)
(add-to-list 'package-pinned-packages '(clojure-mode . "melpa-stable") t)

;; Install packages, if they're not already installed
(defvar package-list
  '(auctex
    cider
    clj-refactor
    clojure-mode
    company
    company-auctex
    company-go
    csv-mode
    diminish
    elpy
    exec-path-from-shell
    expand-region
    flycheck
    git
    git-commit-mode
    git-gutter
    git-rebase-mode
    go-eldoc
    go-mode
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
    paredit
    paredit-everywhere
    projectile
    rainbow-delimiters
    slamhound
    smart-mode-line
    zenburn-theme))

(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (package package-list)
  (when (not (package-installed-p package))
    (package-install package)))

(provide 'init-packages)
;;; init-packages.el ends here
