;;; Packages --- Summary
;; Handle package management


;;; Commentary:
;; Start the package machinery, as well as keep a list of packages that
;; need to be installed.

;;; Code:

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

;; Pin a few packages
(setq package-pinned-packages
      '((cider        . "melpa-stable")
        (clj-refactor . "melpa-stable")
        (clojure-mode . "melpa-stable")
        (super-save   . "melpa-stable")))

(package-initialize)

;; Install packages, if they're not already installed
(setq package-selected-packages
  '(ag
    alchemist
    auctex
    browse-kill-ring
    cider
    clj-refactor
    clojure-mode
    color-theme-sanityinc-tomorrow
    company
    company-auctex
    company-quickhelp
    csv-mode
    diminish
    dockerfile-mode
    dumb-jump
    elisp-slime-nav
    exec-path-from-shell
    expand-region
    flx-ido
    flycheck
    git-gutter
    haskell-mode
    hi2
    idle-highlight-mode
    idomenu
    ido-ubiquitous
    js2-mode
    latex-extra
    magit
    markdown-mode
    monokai-theme
    page-break-lines
    paradox
    paredit
    paredit-everywhere
    projectile
    protobuf-mode
    rainbow-delimiters
    solarized-theme
    smart-mode-line
    smex
    sql-indent
    super-save
    wakatime-mode
    which-key
    yaml-mode))

(when (not package-archive-contents)
  (package-refresh-contents))
(package-install-selected-packages)

(provide 'init-packages)
;;; init-packages.el ends here
