;;; init --- configuration starting point

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:

;; Add custom to the start of the file in an attempt to avoid emacs
;; asking about smart-mode-line's theme every time on startup
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-backends
   (quote
    (company-elisp company-bbdb company-nxml company-css company-eclim company-semantic company-clang company-xcode company-ropemacs company-cmake company-capf
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-files company-dabbrev company-ispell)))
 '(custom-safe-themes
   (quote
    ("3a727bdc09a7a141e58925258b6e873c65ccf393b2240c51553098ca93957723" "756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" "6a37be365d1d95fad2f4d185e51928c789ef7a4ccf17e7ca13ad63a8bf5b922f" default)))
 '(paradox-github-token t))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Ensure that the PATH is set correctly
(exec-path-from-shell-initialize)

(require 'paredit)

;; External user config
(require 'init-funcs)
(require 'init-auctex)
(require 'init-org)
(require 'init-helm)
(require 'init-clojure)

;; Use paredit until smartparens gets a bit more stable
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)

;;  a subset of paredit can be handy in other languages as well
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ensure that when we go to a new line, it's indented properly
(electric-indent-mode)

 ;;; Aesthetics
(load-theme 'monokai t)
(require 'rainbow-delimiters)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; Clean up the modeline a bit
(sml/setup)
(when (memq window-system '(mac ns))
  (set-frame-font "Menlo 12"))
(global-prettify-symbols-mode 1)

;; The audible bell is obnoxious
(setq visible-bell t)

;; Use company mode for completion
(add-hook 'after-init-hook 'global-company-mode)

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
(add-hook 'markdown-mode 'turn-on-auto-fill)

;; Flycheck mode
;; We shouldn't need this require, but it isn't loaded without it
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(projectile-global-mode)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Auto-refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired buffers, but do it quietly
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Quick access to a few files
(global-set-key (kbd "C-c e i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e t")
                (lambda () (interactive) (find-file "~/org/todo.org")))

;; For some reason, zsh files are not opened in shell mode =/
(add-to-list 'auto-mode-alist '("\\*.zsh*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Highlight matching parens
(show-paren-mode 1)

;; Never indent with tabs (unless set in the local buffer,
;; e.g. Makefiles)
(setq-default indent-tabs-mode nil)
(setq gui-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places"))

;; Ensure that a server is running for quicker start times
(require 'server)
(unless (server-running-p)
  (server-start))

;; Use js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
