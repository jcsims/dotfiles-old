;;; Packages --- Summary
;; Handle package management

;;; Commentary:
;; Start the package machinery, as well as keep a list of packages that
;; need to be installed.

;;; Code:


;; Pin a few packages
(when (boundp 'package-pinned-packages)
  (setq package-pinned-packages
        '((cider        . "melpa-stable")
          (clj-refactor . "melpa-stable")
          (clojure-mode . "melpa-stable")
          (paradox      . "melpa-stable")
          (magit        . "melpa-stable"))))

;; Install packages, if they're not already installed
(defvar jcs-package-list
  '(cider
    clj-refactor
    clojure-mode
    company
    company-auctex
    company-quickhelp
    csv-mode
    diminish
    elisp-slime-nav
    ess
    expand-region
    git-gutter
    idle-highlight-mode
    idomenu
    magit
    markdown-mode
    paradox
    paredit
    paredit-everywhere
    projectile
    smex
    sql-indent)
  "Packages that should be installed.")

(use-package ag
  :ensure t
  :config (setq-default ag-highlight-search t
                        ag-reuse-buffers t))

;; Ensure that the PATH is set correctly
(use-package exec-path-from-shell
  :ensure t
  :config (exec-path-from-shell-initialize))

(use-package monokai-theme
  :ensure t
  :demand t
  :config (load-theme 'monokai t))

(use-package rainbow-delimiters
  :ensure t
  :commands (rainbow-delimiters-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package smart-mode-line
  :ensure t
  :demand t
  :config (sml/setup))

(use-package flycheck
  :ensure t
  :commands global-flycheck-mode
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

;; ido
(use-package ido
  :demand t
  :init
  (setq-default ido-enable-flex-matching t
                ido-use-filename-at-point nil
                ido-auto-merge-work-directories-length 0
                ido-use-virtual-buffers t
                ido-default-buffer-method 'selected-window)
  :config
  (ido-mode t)
  (ido-everywhere t)
  (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up]
                                    'previous-history-element))))

(use-package ido-ubiquitous
  :ensure t
  :demand t
  :config (ido-ubiquitous-mode t))

(use-package idomenu
  :ensure t)

(use-package ido-flx
  :ensure t
  :demand t
  :init (setq ido-use-faces nil)
  :config (flx-ido-mode t))

(use-package smex
  :ensure t
  :demand t
  :bind ([remap execute-extended-command] . smex)
  :config
  (setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory)))

(use-package async
  :ensure t)

(provide 'init-packages)
;;; init-packages.el ends here
