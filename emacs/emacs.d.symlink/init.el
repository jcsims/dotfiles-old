;;; init --- configuration starting point

;;; Commentary:
;; 
;; Most of what is found in these files has
;; been pulled from the dotfiles of others.
;; Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:
(add-to-list 'load-path user-emacs-directory)

;; Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

(require 'funcs)
(require 'init-smartparens)
(require 'init-auctex)
(require 'init-org)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

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
;; Clean up the modeline a bit
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))


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
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Flycheck mode
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; eclipse integration
(require 'eclim)
(require 'eclimd)
(setq eclim-executable "/home/jcsims/apps/eclipse/eclim")
(setq help-at-pt-display-when-idle t)
(setq help-at-pt-timer-delay 0.1)
(help-at-pt-set-timer)
;; add the emacs-eclim source
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
(global-eclim-mode)


;;; init.el ends here
