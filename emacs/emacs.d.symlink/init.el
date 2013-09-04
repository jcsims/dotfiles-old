;; Package Management
;; Make sure that all the packages I use are installed
;; (require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/pallet/lib")
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Load other files
(load "~/.emacs.d/funcs.el")

;; Be evil!
(setq evil-want-C-u-scroll 1)
(evil-mode 1)

;; Aesthetics
(load-theme 'base16-eighties t)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))

(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green4")
     (set-face-foreground 'magit-diff-del "red3")))

;; Enable whitespace mode for progamming languages, and highlight when
;; lines are over 80 characters long
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Clojure mode
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;; Save backups to a central location
;; Taken from http://whattheemacsd.com/init.el-02.html
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Clean up some noisy modes
(rename-modeline "clojure-mode" clojure-mode "Clj")

;; Globally enable yasnippets
(yas-global-mode 1)

;; Configure auto-complete
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode 1)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))

;; Bind expand-region
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

