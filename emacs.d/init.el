;;; init --- configuration starting point

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:

;; Add custom to the start of the file in an attempt to avoid emacs
;; asking about smart-mode-line's theme every time on startup

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "70b51a849b665f50a97a028c44cec36b398398357d8f7c19d558fe832b91980f" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(js-bounce-indent-p t)
 '(js-indent-level 2)
 '(package-selected-packages
   (quote
    (yaml-mode which-key wakatime-mode super-save sql-indent solarized-theme smex smart-mode-line scala-mode rainbow-delimiters protobuf-mode paredit-everywhere paradox page-break-lines org-trello monokai-theme markdown-mode magit latex-extra js2-mode idomenu ido-ubiquitous idle-highlight-mode hi2 haskell-mode git-gutter flycheck flx-ido expand-region exec-path-from-shell elisp-slime-nav dumb-jump dockerfile-mode diminish csv-mode company-quickhelp company-auctex color-theme-solarized color-theme-sanityinc-tomorrow clj-refactor browse-kill-ring ag)))
 '(paradox-automatically-star t)
 '(wakatime-cli-path "/opt/pkg/bin/wakatime"))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)
(require 'init-packages)

(load-theme 'solarized-light t)
(setq active-theme 'solarized-light)
(defun toggle-dark-light-theme ()
  "Toggle the current solarized theme between light and dark."
  (interactive)
  (if (eq active-theme 'solarized-light)
      (setq active-theme 'solarized-dark)
    (setq active-theme 'solarized-light))
  (load-theme active-theme))

(sml/setup)

;;; Misc
;; Set some Emacs defaults
(setq-default inhibit-splash-screen t ; Don't show the splash screen
              ring-bell-function 'ignore ; Just ignore error notifications
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
  (set-frame-font "-*-Menlo-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
(global-prettify-symbols-mode 1)
;; (insert "\n(set-frame-font \"" (cdr (assoc 'font (frame-parameters))) "\")")

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ensure that when we go to a new line, it's indented properly
(electric-indent-mode)

;; Highlight cols past 100 chars
(setq-default whitespace-line-column 100
              whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

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
(global-set-key (kbd "C-c e d")
                (lambda () (interactive) (find-file "~/org/dev.org")))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(require 'saveplace)
(setq-default save-place t)

;; Highlight matching parens
(show-paren-mode t)

;; Ensure that a server is running for quicker start times
(require 'server)
(unless (server-running-p)
  (server-start))

;; External user config
(require 'init-funcs)
(require 'init-org)
(require 'init-auctex)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-elixir)

;; Work-specific code - not to be checked in
(if (file-exists-p (concat user-emacs-directory "lisp/init-work.el"))
    (require 'init-work))

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; For some reason, zsh files are not opened in shell mode =/
(add-to-list 'auto-mode-alist '("\\*.zsh*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))

;;; Config other packages
;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(add-hook 'prog-mode-hook 'idle-highlight-mode)

;; Ag
(setq-default ag-highlight-search t
              ag-reuse-buffers t)

;; Ensure that the PATH is set correctly
(exec-path-from-shell-initialize)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

;; ido
(setq-default ido-enable-flex-matching t
              ido-use-filename-at-point nil
              ido-auto-merge-work-directories-length 0
              ido-use-virtual-buffers t
              ido-default-buffer-method 'selected-window
              ido-use-faces nil)
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up]
                                  'previous-history-element)))

;; smex
(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(global-git-gutter-mode)

(setq-default paradox-execute-asynchronously t)

;; Magit
(setq-default magit-last-seen-setup-instructions "1.4.0")
(global-set-key (kbd "C-c g") 'magit-status)
;; Gravatars are messed up in OSX
(setq-default magit-revision-use-gravatar-kludge t)

;; Easily move between windows
(windmove-default-keybindings)

;; Allow for easy undo/redo of window changes
(winner-mode 1)

;; Paredit
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(projectile-global-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

;; Make the kill-ring a little more accessible
(browse-kill-ring-default-keybindings)

;; Increase the GC threshold
(setq gc-cons-threshold 20000000)

(global-wakatime-mode)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Save all the time
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(dumb-jump-mode)

(add-to-list 'auto-mode-alist '("\\.yml.*\\'" . yaml-mode))

(which-key-mode)

;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
