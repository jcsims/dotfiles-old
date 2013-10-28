(add-to-list 'load-path user-emacs-directory)

;; Package Management
(require 'init-packages)

(require 'funcs)
(require 'init-smartparens)
(require 'init-auctex)
(require 'init-org)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Be evil!
(setq evil-want-C-u-scroll 1)
(use-package evil
  :ensure t
  :init (evil-mode 1))
(use-package undo-tree :ensure t)
(use-package evil-indent-textobject :ensure t)

;; Aesthetics
(load-theme 'base16-eighties t)
(use-package rainbow-delimiters
  :ensure t
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
(use-package magit
  :ensure t
  :init (progn
          (set-face-foreground 'magit-diff-add "green4")
          (set-face-foreground 'magit-diff-del "red3")))
;; Set frame transparency
(add-to-list 'default-frame-alist '(alpha 90 80))

;; Load a few other packages
(require 'init-clojure)
(require 'init-yasnippet)
(require 'init-haskell)
(require 'init-auto-complete)

;; Enable whitespace mode for programming languages, and highlight when
;; lines are over 80 characters long
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Save backups to a central location
;; Taken from http://whattheemacsd.com/init.el-02.html
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Fill mode is pretty handy
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode 'turn-on-auto-fill)

;; smex
(use-package smex
  :ensure t
  :bind ("M-x" . smex)
  :init (setq smex-save-file (concat user-emacs-directory ".smex-items")))
(smex-initialize)

;; Flycheck mode
(use-package flycheck
  :ensure t
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)
