;;; Take care of loading the packages I use

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Taken from Steve Purcell's .emacs.d
(defun install-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (install-package package min-version t)))))


(defvar my-packages '(yaml-mode
                      s
                      request
                      project-mode
                      pkg-info
                      markdown-mode
                      levenshtein
                      ido-ubiquitous
                      idle-highlight-mode
                      helm
                      gitconfig-mode
                      git-rebase-mode
                      git-commit-mode
                      find-file-in-project
                      f
                      expand-region
                      ess-smart-underscore
                      ess-R-object-popup
                      ess-R-data-view
                      ess
                      erlang
                      emacs-eclim
                      elisp-slime-nav
                      dash
                      ctable
                      better-defaults
                      base16-theme
                      ac-nrepl
                      ac-math))

(dolist (p my-packages)
  (install-package p))

(install-package 'use-package)
(require 'use-package)

(provide 'init-packages)
;;; init-packages.el ends here
