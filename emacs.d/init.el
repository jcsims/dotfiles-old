;;; init --- configuration starting point

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:

(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))
(package-initialize)

;; Personal info
(setq user-full-name "Chris Sims"
      user-mail-address "chris@jcsi.ms"
      calendar-latitude 47.4
      calendar-longitude -122.2
      calendar-location-name "Kent, WA")

(setq-default paradox-execute-asynchronously t)

(setq active-theme 'solarized-light)
(load-theme 'solarized-light t)
(defun toggle-dark-light-theme ()
  "Toggle the current solarized theme between light and dark."
  (interactive)
  (if (eq active-theme 'solarized-light)
      (setq active-theme 'solarized-dark)
    (setq active-theme 'solarized-light))
  (load-theme active-theme))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking)

;; Add custom to the start of the file in an attempt to avoid emacs
;; asking about smart-mode-line's theme every time on startup
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(custom-set-faces)
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(exec-path-from-shell-initialize)

(setq-default inhibit-splash-screen t   ; Don't show the splash screen
              ring-bell-function 'ignore ; Just ignore error notifications
              vc-make-backup-files t     ; Make backups of files,
              vc-follow-symlinks t ; even when they're in version control
              backup-directory-alist ; Save backups to a central location
              `(("." . ,(expand-file-name
                         (concat user-emacs-directory "backups"))))
              global-auto-revert-non-file-buffers t ; Refresh dired buffers,
              auto-revert-verbose nil   ; but do it quietly
              indent-tabs-mode nil ; Don't use tabs unless buffer-local
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

(require 're-builder)
(setq reb-re-syntax 'string)

;; External user config
(require 'init-funcs)
(require 'init-org)
(require 'init-auctex)
(require 'init-clojure)
(require 'init-haskell)
(require 'init-elixir)

(setq-default whitespace-line-column 80
              whitespace-style '(face trailing lines-tail))

(column-number-mode)

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

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(setq-default save-place t)

;; Highlight matching parens
(show-paren-mode t)

;; Ensure that a server is running for quicker start times
(require 'server)
(unless (server-running-p)
  (server-start))

;; Allow for seamless gpg interaction
(require 'epa-file)

;; Work-specific code - should be encrypted!
(setq work-init (concat user-emacs-directory "lisp/init-work.el.gpg"))
(if (file-exists-p work-init)
    (load work-init))

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)

;; For some reason, zsh files are not opened in shell mode =/
(add-to-list 'auto-mode-alist '("\\*.zsh*\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\zshrc\\'" . sh-mode))

;; Config other packages
(add-hook 'after-init-hook 'global-company-mode)

;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

(add-hook 'prog-mode-hook 'idle-highlight-mode)

(setq-default ag-highlight-search t
              ag-reuse-buffers t)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)

(add-hook 'after-init-hook #'global-flycheck-mode)

(setq-default ido-enable-flex-matching t
              ido-use-filename-at-point nil
              ido-auto-merge-work-directories-length 0
              ido-use-virtual-buffers t
              ido-default-buffer-method 'selected-window
              ido-use-faces nil)

(ido-mode t)
(ido-everywhere t)
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up]
                                  'previous-history-element)))

(ido-ubiquitous-mode t)
(flx-ido-mode t)

(setq-default smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(setq-default magit-last-seen-setup-instructions "1.4.0"
              ;; Gravatars are messed up in OSX
              magit-revision-use-gravatar-kludge t
              magit-branch-adjust-remote-upstream-alist '(("upstream/master" . "issue-")))
(global-set-key (kbd "C-c g") 'magit-status)

(windmove-default-keybindings)

(winner-mode 1)

(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

(projectile-mode)

(global-set-key (kbd "C-=") 'er/expand-region)

(browse-kill-ring-default-keybindings)

;; Increase the GC threshold
(setq gc-cons-threshold 20000000)

(add-to-list 'auto-mode-alist '("Dockerfile" . dockerfile-mode))

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(dumb-jump-mode)

(add-to-list 'auto-mode-alist '("\\.yml.*\\'" . yaml-mode))

(which-key-mode)

(global-set-key (kbd "C-+") 'mc/mark-next-like-this)

;; Some keybinds for this mode:
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]

(require 'diff-hl)

(add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
(add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)

(sml/setup)
(global-linum-mode)
;;; init.el ends here
