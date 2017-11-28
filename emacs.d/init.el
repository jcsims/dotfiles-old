;;; init --- configuration starting point

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu"          . "https://elpa.gnu.org/packages/")
        ("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
(use-package delight)
(use-package bind-key)

(use-package validate)

;;; Personal info
(validate-setq user-full-name "Chris Sims"
      user-mail-address "chris@jcsi.ms")

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Increase the GC threshold
(validate-setq gc-cons-threshold 20000000)

;; Get rid of the insert key. I never use it, and I turn it on
;; accidentally all the time
(global-set-key (kbd "<insert>") nil)

;; (global-set-key (kbd "C-s") 'isearch-forward-regexp)
;; (global-set-key (kbd "C-r") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

;; Turn off the toolbar and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Handy to get the current font/size that you've got:
;; (insert "\n(set-frame-font \"" (cdr (assoc 'font (frame-parameters))) "\")")
(when (memq window-system '(mac ns))
  (set-frame-font "-*-Menlo-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(when (memq window-system '(x))
  (set-frame-font "-PfEd-DejaVu Sans Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1"))
(global-prettify-symbols-mode 1)

;; Quick access to a few files
(defun find-init-file ()
  "Open the init file for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e i") 'find-init-file)

(use-package autorevert
  :ensure nil)

;;; Misc settings
(setq inhibit-splash-screen t           ; Don't show the splash screen
      ring-bell-function 'ignore     ; Just ignore error notifications
      vc-follow-symlinks t      ; even when they're in version control
      backup-directory-alist    ; Save backups to a central location
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups"))))
      global-auto-revert-non-file-buffers t ; Refresh dired buffers,
      auto-revert-verbose nil               ; but do it quietly
      indent-tabs-mode nil        ; Don't use tabs unless buffer-local
      gui-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      ;; When scrolling, make sure to come back to the same spot
      scroll-preserve-screen-position 'always
      scroll-error-top-bottom t         ; Scroll similar to vim
      )

;;; Packages
(use-package paradox
  :config
  (validate-setq paradox-execute-asynchronously t)
  (paradox-enable))

(use-package solarized-theme
  :init
  :disabled
  (validate-setq jcs-active-theme 'solarized-dark)
  (defun toggle-dark-light-theme ()
    "Toggle the current solarized theme between light and dark."
    (interactive)
    (if (eq jcs-active-theme 'solarized-light)
        (validate-setq jcs-active-theme 'solarized-dark)
      (validate-setq jcs-active-theme 'solarized-light))
    (load-theme jcs-active-theme))
  :config (load-theme jcs-active-theme t))

(use-package monokai-theme
  :config (load-theme 'monokai t))

(use-package macrostep
  :bind ("C-c m" . macrostep-expand))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package re-builder
  :ensure f
  :bind (("C-c R" . re-builder))
  :config (validate-setq reb-re-syntax 'string))

;; External user config
(use-package init-funcs
  :ensure f
  :load-path "lisp")

(use-package threatgrid
  :ensure f
  :load-path "lisp")

(use-package whitespace
  :delight whitespace-mode
  :config
  (validate-setq whitespace-line-column 100
        whitespace-style '(face trailing lines-tail))
  (add-hook 'prog-mode-hook 'whitespace-mode))

(use-package markdown-mode
  :config (validate-setq markdown-fontify-code-blocks-natively t))

(use-package simple
  :ensure f
  :delight auto-fill-function
  :defer 2
  :config
  (column-number-mode)
  ;; Fill mode is pretty handy
  (add-hook 'text-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode 'turn-on-auto-fill)
  (add-hook 'markdown-mode 'turn-on-auto-fill))

;; Ensure that when we go to a new line, it's indented properly
(use-package electric
  :defer 2
  :config (electric-indent-mode))

(use-package autorevert
  :delight auto-revert-mode
  :defer 2
  :config
  ;; Auto-refresh buffers
  (global-auto-revert-mode))

(use-package saveplace
  :config (save-place-mode))

;; Highlight matching parens
(use-package paren
  :defer 2
  :config
  (show-paren-mode t))

;; Ensure that a server is running for quicker start times
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; Allow for seamless gpg interaction
(use-package epa-file :ensure f)

;; Work-specific code - should be encrypted!
(defvar work-init (concat user-emacs-directory "lisp/init-work.el.gpg"))
(if (file-exists-p work-init)
    (load work-init))

;; Flyspell mode
(use-package flyspell
  :defer 2
  :config (add-hook 'text-mode-hook 'flyspell-mode))

;; Config other packages
(use-package company-mode
  :ensure company
  :defer 5
  :config (global-company-mode))

(use-package company-quickhelp)

(use-package elisp-slime-nav
  :delight
  :config
  ;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook 'turn-on-elisp-slime-nav-mode)))

(use-package idle-highlight-mode
  :delight
  :defer 2
  :config (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package ag
  :config
  (validate-setq ag-highlight-search t
                 ag-reuse-buffers t))

(use-package rainbow-delimiters
  :defer 2
  :config (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package flycheck
  :defer 2
  :config (global-flycheck-mode))

(use-package ido
  :disabled
  :config
  (validate-setq ido-use-filename-at-point nil
        ido-auto-merge-work-directories-length 0
        ido-use-virtual-buffers t
        ido-default-buffer-method 'selected-window
        ido-use-faces nil
        ido-enable-flex-matching t)
  (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up]
                                    'previous-history-element)))
  (ido-mode t)
  (ido-everywhere t))

(use-package idomenu
  :disabled)

(use-package ido-completing-read+
  :disabled
  :config
  (validate-setq ido-ubiquitous-auto-update-overrides t)
  (ido-ubiquitous-mode t))

(use-package flx-ido
  :disabled
  :config (flx-ido-mode t))

(use-package smex
  :init
  :disabled
  (validate-setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(use-package ivy
  :delight
  :bind (("C-c M-r" . ivy-resume)) ; TODO: Find a binding that doesn't
				   ; get overwritten...
  :config
  (validate-setq ivy-use-virtual-buffers t
		 enable-recursive-minibuffers t
		 ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package swiper)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-s" . counsel-grep-or-swiper))
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (validate-setq counsel-grep-base-command
        "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package counsel-projectile
  :config (counsel-projectile-on))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :config
  (validate-setq magit-branch-adjust-remote-upstream-alist '(("upstream/master" . "issue-"))
                 magit-prefer-remote-upstream t)
  :bind ("C-c g" . magit-status))

(use-package git-timemachine :defer 5)
(use-package git-gutter :defer 2)
(use-package ghub :defer 2)
(use-package ghub+ :defer 2)

(use-package windmove
  :config (windmove-default-keybindings))

(use-package winner
  :defer 2
  :config (winner-mode 1))

(use-package paredit
  :delight
  :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package paredit-everywhere
  :delight
  :config (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package projectile
  :delight
  :config
  (projectile-mode)
  ;; Note: remove this once
  ;; https://github.com/bbatsov/projectile/issues/1183 is resolved
  (validate-setq projectile-mode-line
        '(:eval (format " Projectile[%s]"
                        (projectile-project-name)))))

(use-package expand-region
  :defer 2
  :bind ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :defer 2
  :config (browse-kill-ring-default-keybindings))

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package dumb-jump
  :defer 2
  :bind ("C-M-g" . dumb-jump-go)
  :config (dumb-jump-mode))

(use-package yaml-mode
  :mode "\\.yml.*\\'")

(use-package which-key
  :delight
  :defer 2
  :config (which-key-mode))

;; Some keybinds for this mode:
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
(use-package diff-hl
  :defer 2
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

(use-package super-save
  :delight
  :init
  (validate-setq auto-save-default nil)
  :defer 2
  :config
  (super-save-mode +1)
  (validate-setq super-save-auto-save-when-idle t))

;; Set up the fancy mode-line
(use-package smart-mode-line
  :config
  (sml/setup))

;; Turn on line numbers everywhere
(use-package nlinum
  :config (global-nlinum-mode))

(use-package dired-collapse :defer 2)
(use-package salt-mode
  :defer 2
  :mode "\\.sls\\'")

;; Use es-mode for ElasticSearch buffers
(use-package es-mode
  :mode "\\.es$")

(use-package multiple-cursors
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))

(use-package alchemist
  :config
  ;; Run the whole test suite with alchemist-mix-test after saving a buffer.
  (validate-setq alchemist-hooks-test-on-save t)
  ;; Compile your project with alchemist-mix-compile after saving a
  ;; buffer.
  (validate-setq alchemist-hooks-compile-on-save t))

(use-package tex
  :disabled
  :ensure auctex
  :config
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (validate-setq TeX-auto-save t
                 TeX-parse-self t
                 TeX-master nil
                 TeX-PDF-mode t)
  (when (eq system-type 'darwin) ;; mac-specific settings
    (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
    (validate-setq TeX-source-correlate-method 'synctex)
    (validate-setq TeX-view-program-list
                   '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
    (validate-setq TeX-view-program-selection '((output-pdf "Skim")))
    (add-hook 'TeX-mode-hook
              (lambda ()
                (add-to-list
                 'TeX-output-view-style
                 '("^pdf$" "."
                   "/Applications/Skim.app/Contents/SharedSupport/displayline -b %n %o %b"))))))

;; Use latex-extra package
(use-package latex-extra
  :commands latex-extra-mode
  :config
  (add-hook 'LaTeX-mode-hook #'latex-extra-mode))

(use-package company-auctex)

(use-package org
  :config
  (validate-setq org-directory "~/org/")
  (defvar gtd-file (concat org-directory "gtd.org"))
  (defvar work-gtd-file (concat org-directory "work-gtd.org"))
  (defvar someday-file (concat org-directory "someday.org"))
  (defvar tickler-file (concat org-directory "tickler.org"))
  (defvar work-tickler-file (concat org-directory "work-tickler.org"))
  (defvar inbox-file (concat org-directory "inbox.org"))
  (defvar reference-file (concat org-directory "reference.org"))
  (validate-setq org-log-done 'time
                 org-startup-indented t
                 org-startup-folded t
                 org-agenda-files (file-expand-wildcards (concat org-directory "*.org"))
                 org-default-notes-file (concat org-directory "inbox.org")
                 org-src-fontify-natively t
                 org-use-fast-todo-selection t
                 org-refile-allow-creating-parent-nodes t
                 org-refile-use-outline-path 'file
                 org-outline-path-complete-in-steps nil
                 org-completion-use-ido nil
                 ;; Don't ask every time before evaluating an org source block
                 org-confirm-babel-evaluate nil)
  (setq org-refile-targets '((gtd-file . (:maxlevel . 2))
                             (work-gtd-file . (:maxlevel . 2))
                             (someday-file . (:level . 1))
                             (tickler-file . (:level . 1))
                             (work-tickler-file . (:level . 1))
                             (reference-file . (:level . 1)))
        org-todo-keywords
        (quote ((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)"))))
  (defun find-gtd-file () (interactive) (find-file gtd-file))
  (defun find-work-gtd-file () (interactive) (find-file work-gtd-file))
  (defun find-someday-file () (interactive) (find-file someday-file))
  (defun find-inbox-file () (interactive) (find-file inbox-file))
  (defun find-tickler-file () (interactive) (find-file tickler-file))
  (defun find-reference-file () (interactive) (find-file reference-file))
  ;; Add a few languages for execution in org source blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (sh . t)
                                 (emacs-lisp . t)
                                 (elasticsearch . t)
                                 (restclient . t)))

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c e g" . find-gtd-file)
         ("C-c e w" . find-work-gtd-file)
         ("C-c e s" . find-someday-file)
         ("C-c e n" . find-inbox-file)
         ("C-c e t" . find-tickler-file)
         ("C-c e r" . find-reference-file)))

(use-package org-capture
  :ensure f
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates '(("t" "Todo [inbox]" entry
                                 (file (concat org-directory "inbox.org"))
                                 "* TODO %i%?")
                                ("T" "Tickler" entry
                                 (file+headline (concat org-directory "tickler.org") "Tickler")
                                 "* %i%? \n %U")
                                ("w" "Work Tickler" entry
                                 (file+headline (concat org-directory "work-tickler.org") "Tickler")
                                 "* %i%? \n %U")
                                ("r" "Reference" entry
                                 (file (concat org-directory "reference.org"))
                                 "* %i%? \n %U"))))

(use-package org-agenda
  :ensure f
  :config
  ;; Use the current window to open the agenda
  (validate-setq org-agenda-window-setup 'current-window)
  (setq org-agenda-custom-commands
        '(("c" "Agenda and all action items"
           ((agenda "")
            (todo "WAITING")
            (todo "DOING|TODO"
                  ((org-agenda-sorting-strategy '(todo-state-down))))))
          ("w" "Work tasks"
           ((agenda ""
                    ((org-agenda-files (list work-tickler-file work-gtd-file inbox-file))
                     (org-agenda-overriding-header "Work")))
            (todo "WAITING" ((org-agenda-files (list work-tickler-file work-gtd-file inbox-file))))
            (todo "DOING"
                  ((org-agenda-files (list work-tickler-file work-gtd-file inbox-file))))
            (todo "TODO"
                  ((org-agenda-files (list work-tickler-file work-gtd-file inbox-file))))))
          ("a" todo "WAITING")))

  (defun org-current-is-todo ()
    "Is the current org heading a TODO?"
    (string= "TODO" (org-get-todo-state)))

  ;; Borrowed from https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (validate-setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (validate-setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max)))))))

(use-package alert
  :config
  (setq alert-default-style 'libnotify))

(use-package org-alert
  :disabled
  :config
  (validate-setq org-alert-notification-title "Org Agenda")
  (validate-setq org-alert-interval (* 60 60))
  (org-alert-enable))

(use-package ox-md :ensure f)
(use-package restclient)
(use-package ob-restclient)
(use-package ob-sql-mode)
(use-package go-mode)
(use-package csv-mode)
(use-package sql-indent)
(use-package eldoc :delight :ensure f)

(use-package clojure-mode
  :defer 2
  :delight
  :mode ("\\.clj.*\\'" "\\.edn.*\\'")
  :bind (:map clojure-mode-map
              ("C-c i" . cider-inspect-last-result))
  :config
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'paredit-mode)
  ;; Add some goodies from Emacs Live
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\)("
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "ƒ")
                              nil)))))
  (font-lock-add-keywords
   'clojure-mode `(("\\(#\\){"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "∈")
                              nil)))))
  ;; Set up proper indentation for a few compojure functions
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)))

;;; Cider
(use-package cider
  :delight
  :defer 2
  :config
  (validate-setq cider-prompt-for-symbol nil ; Don't prompt for a symbol with `M-.`
                 cljr-favor-prefix-notation nil
                 cider-repl-display-help-banner nil
                 nrepl-hide-special-buffers t)
  (add-hook 'cider-mode-hook 'eldoc-mode)
  (add-hook 'clojure-mode-hook 'cider-mode)
  (add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  (add-hook 'cider-repl-mode-hook 'eldoc-mode)

  (defun tdd-test ()
    "Thin wrapper around `cider-test-run-project-tests', borrowed from
  http://endlessparentheses.com/test-driven-development-in-cider-and-emacs.html"
    (when (cider-connected-p)
      (cider-test-run-project-tests)))

  (define-minor-mode tdd-mode
    "Run all Clojure tests whenever a file is saved"
    nil " TDD" nil
    (if tdd-mode
        (add-hook 'after-save-hook #'tdd-test nil 'local)
      (remove-hook 'after-save-hook #'tdd-test 'local))))

(use-package clj-refactor
  :defer 2
  :delight
  :config
  (validate-setq cljr-suppress-middleware-warnings t
        ;; Lazily build ASTs, instead of immediately on REPL connect
        cljr-warn-on-eval t
        cljr-eagerly-build-asts-on-startup nil)
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c r"))))

(use-package yasnippet :delight)

;; Try out a linter...
(use-package flycheck-joker :defer 2)

;; Seed the PRNG anew, from the system's entropy pool
(random t)

(use-package systemd :defer 2)

(use-package shell-pop
  :custom
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))) "Use eshell.")
  (shell-pop-universal-key "C-t")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

;;; init.el ends here
