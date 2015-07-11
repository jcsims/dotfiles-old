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
    (company-elisp company-bbdb company-nxml company-css company-semantic company-clang company-xcode company-cmake company-capf
                   (company-dabbrev-code company-gtags company-etags company-keywords)
                   company-oddmuse company-files company-dabbrev company-ispell)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(org-agenda-files (quote ("~/org/todo.org")))
 '(paradox-automatically-star t)
 '(paradox-github-token t))


(require 'package)

(when (>= emacs-major-version 24)
  (setq package-archives
        '(("gnu" . "http://elpa.gnu.org/packages/")
          ("melpa" . "http://melpa.org/packages/")
          ("melpa-stable" . "http://stable.melpa.org/packages/"))))

(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(when (not package-archive-contents)
  (package-refresh-contents))
(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(require 'use-package)

;;; Misc
;; Set some Emacs defaults
(setq-default inhibit-splash-screen t ; Don't show the splash screen
              visible-bell t ; The audible bell is obnoxious
              vc-make-backup-files t ; Make backups of files,
              vc-follow-symlinks t   ; even when they're in version control
              backup-directory-alist ; Save backups to a central location
              `(("." . ,(expand-file-name
                         (concat user-emacs-directory "backups"))))
              global-auto-revert-non-file-buffers t ; Refresh dired buffers,
              auto-revert-verbose nil               ; but do it quietly
              indent-tabs-mode nil      ; Don't use tabs unless buffer-local
              gui-select-enable-clipboard t
              x-select-enable-primary t
              save-interprogram-paste-before-kill t
              apropos-do-all t
              mouse-yank-at-point t
              save-place-file (concat user-emacs-directory "places")
              ;; When scrolling, make sure to come back to the same spot
              scroll-preserve-screen-position 'always
              scroll-error-top-bottom t ; Scroll similar to vim
              )

;; Turn off the toolbar and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(when (memq window-system '(mac ns))
  (set-frame-font "Menlo 12"))
(global-prettify-symbols-mode 1)

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ensure that when we go to a new line, it's indented properly
(electric-indent-mode)

(use-package whitespace-mode
  :commands whitespace-mode
  :init
  ;; Highlight cols past 100 chars
  (setq-default whitespace-line-column 100
                whitespace-style '(face lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

;; Fill mode is pretty handy
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode 'turn-on-auto-fill)
(add-hook 'markdown-mode 'turn-on-auto-fill)

;; Auto-refresh buffers
(global-auto-revert-mode)

;; Quick access to a few files
(global-set-key (kbd "C-c e i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e t")
                (lambda () (interactive) (find-file "~/org/todo.org")))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(use-package saveplace
  :init (setq-default save-place t)
  :demand t)

;; Highlight matching parens
(show-paren-mode 1)

;; Ensure that a server is running for quicker start times
(use-package server
  :demand t
  :config
  (unless (server-running-p)
    (server-start)))

;; Package Management
(require 'init-packages)

;; External user config
(require 'init-funcs)

(require 'init-org)
(require 'init-auctex)
(require 'init-clojure)

;; Work-specific code - not to be checked in
(if (file-exists-p (concat user-emacs-directory "lisp/init-work.el"))
    (require 'init-work))

;; Use company mode for completion
(add-hook 'after-init-hook 'global-company-mode)
;; Add tooltips for company completion candidates
(company-quickhelp-mode 1)

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(projectile-global-mode)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; For some reason, zsh files are not opened in shell mode =/
(add-to-list 'auto-mode-alist '("\\*.zsh*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))

;; Use js2 mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Show the git gutter everywhere
(global-git-gutter-mode +1)

;;; elisp-slime-nav
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; Asynchronous updating is nice
(setq-default paradox-execute-asynchronously t)

;; Emacs Speaks Statistics
(require 'ess-site)
(setq ess-R-font-lock-keywords '((ess-R-fl-keyword:modifiers . t)
                                 (ess-R-fl-keyword:fun-defs . t)
                                 (ess-R-fl-keyword:keywords . t)
                                 (ess-R-fl-keyword:assign-ops . t)
                                 (ess-R-fl-keyword:constants . t)
                                 (ess-fl-keyword:fun-calls . t)
                                 (ess-fl-keyword:numbers . t)
                                 (ess-fl-keyword:operators . t)
                                 (ess-fl-keyword:delimiters . t)
                                 (ess-fl-keyword:= . t)
                                 (ess-R-fl-keyword:F&T . t)))

;; The fact that ess-mode doesn't inherit from prog-mode is a bit of a
;; pain
(setq ess-mode-hook (append ess-mode-hook prog-mode-hook))
(setq ess-default-style 'GNU)

;; Magit
(global-set-key (kbd "C-c g") 'magit-status)
(setq-default magit-last-seen-setup-instructions "1.4.0")

;; Make it easy to move between buffers
(windmove-default-keybindings)

;; Also make it easy to get back to the last window configuration
(winner-mode 1)

;; paredit
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
