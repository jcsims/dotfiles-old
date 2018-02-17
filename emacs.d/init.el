;;; init --- configuration starting point -*- no-byte-compile: t -*-

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:
(setq jcs/custom-file "~/.emacs.d/custom.el")
(load jcs/custom-file)

(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))

(package-initialize)

;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(use-package validate :ensure t)
(validate-setq use-package-always-ensure t
               use-package-compute-statistics t
               use-package-verbose t)
(use-package delight)
(use-package bind-key)

(use-package auto-compile
  :config
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode))

(setq source-directory "~/code/emacs")

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
  (set-frame-font "Hack 9"))

(global-prettify-symbols-mode 1)

;; Quick access to a few files
(defun find-init-file ()
  "Open the init file for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e e") 'find-init-file)

(use-package autorevert
  :ensure f)

(use-package saveplace
  :ensure f
  :config (save-place-mode))

;;; Misc settings
(validate-setq inhibit-splash-screen t  ; Don't show the splash screen
               ring-bell-function 'ignore ; Just ignore error notifications
               vc-follow-symlinks t ; even when they're in version control
               backup-directory-alist ; Save backups to a central location
               `(("." . ,(expand-file-name
                          (concat user-emacs-directory "backups"))))
	       auto-save-file-name-transforms
	       `((".*" ,(expand-file-name
			 (concat user-emacs-directory "auto-saves")) t))
               global-auto-revert-non-file-buffers t ; Refresh dired buffers,
               auto-revert-verbose nil  ; but do it quietly
               indent-tabs-mode nil ; Don't use tabs unless buffer-local
               select-enable-primary t
               save-interprogram-paste-before-kill t
               mouse-yank-at-point t
               save-place-file (concat user-emacs-directory "places")
               ;; When scrolling, make sure to come back to the same spot
               scroll-preserve-screen-position 'always
               scroll-error-top-bottom t ; Scroll similar to vim
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

(use-package whitespace
  :delight whitespace-mode
  :config
  (validate-setq whitespace-line-column 80
                 whitespace-style '(face trailing lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package markdown-mode
  :config (validate-setq markdown-fontify-code-blocks-natively t))

(use-package simple
  :ensure f
  :delight auto-fill-function
  :defer 2
  :config (column-number-mode)
  :bind ("M-SPC" . cycle-spacing)
  :hook ((text-mode org-mode markdown-mode) . turn-on-auto-fill))

;; Ensure that when we go to a new line, it's indented properly
(use-package electric
  :config (electric-indent-mode))

(use-package autorevert
  :delight auto-revert-mode
  :config
  ;; Auto-refresh buffers
  (global-auto-revert-mode))

;; Highlight matching parens
(use-package paren
  :config
  (show-paren-mode))

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
  :hook (text-mode . flyspell-mode))

;; Config other packages
(use-package company
  :config
  (setq company-tooltip-limit 20)                       ; bigger popup window
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing)
  (global-company-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package elisp-slime-nav
  :delight
  :config
  ;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-package idle-highlight-mode
  :delight
  :hook (prog-mode . idle-highlight-mode))

(use-package ag
  :config
  (validate-setq ag-highlight-search t
                 ag-reuse-buffers t))

(use-package rainbow-delimiters
  :hook (emacs-lisp-mode . rainbow-delimiters-mode))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package ido
  :disabled
  :hook (ido-setup . (lambda () (define-key ido-completion-map [up]
                             'previous-history-element)))
  :config
  (validate-setq ido-use-filename-at-point nil
                 ido-auto-merge-work-directories-length 0
                 ido-use-virtual-buffers t
                 ido-default-buffer-method 'selected-window
                 ido-use-faces nil
                 ido-enable-flex-matching t)
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
  :bind (("C-c C-r" . ivy-resume)) ; TODO: Find a binding that doesn't
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

(use-package projectile
  :delight
  :config
  (projectile-mode)
  ;; Note: remove this once
  ;; https://github.com/bbatsov/projectile/issues/1183 is resolved
  (validate-setq projectile-mode-line
                 '(:eval (format " Projectile[%s]"
                                 (projectile-project-name)))))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :config
  (validate-setq magit-branch-adjust-remote-upstream-alist '(("upstream/master" . "issue-"))
                 magit-prefer-remote-upstream t)
  :bind ("C-c g" . magit-status))

(use-package git-timemachine )
(use-package git-gutter )

;; ghub and dash are required by threatgrid.el
(use-package ghub )
(use-package dash
  :config (dash-enable-font-lock))
(use-package threatgrid
  :ensure f
  :commands (preq tg-insert-weekly-work-report tg-create-tb-issue)
  :load-path "lisp")

(use-package windmove
  :config (windmove-default-keybindings))

(use-package winner
  :config (winner-mode 1))

(use-package paredit
  :delight
  :hook (emacs-lisp-mode . paredit-mode))

(use-package paredit-everywhere
  :delight
  :hook (prog-mode . paredit-everywhere-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package browse-kill-ring
  :config (browse-kill-ring-default-keybindings))

(use-package dockerfile-mode
  :mode "Dockerfile")

(use-package dumb-jump
  :bind ("C-M-g" . dumb-jump-go)
  :config (dumb-jump-mode))

(use-package yaml-mode
  :mode "\\.yml.*\\'")

(use-package which-key
  :delight
  :config (which-key-mode))

;; Some keybinds for this mode:
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
(use-package diff-hl
  :hook ((prog-mode vc-dir-mode) . turn-on-diff-hl-mode))

(use-package super-save
  :delight
  :init
  (validate-setq auto-save-default nil)
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

(use-package dired-collapse)
(use-package salt-mode
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
  :disabled
  :config
  ;; Run the whole test suite with alchemist-mix-test after saving a buffer.
  (validate-setq alchemist-hooks-test-on-save t)
  ;; Compile your project with alchemist-mix-compile after saving a
  ;; buffer.
  (validate-setq alchemist-hooks-compile-on-save t))

(use-package tex
  :disabled
  :ensure auctex
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . auto-fill-mode)
  (LaTeX-mode . flyspell-mode)
  :config
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
  :disabled
  :commands latex-extra-mode
  :hook (LaTeX-mode . latex-extra-mode))

(use-package company-auctex :disabled)

(defvar org-dir "~/org/")
(defvar jcs/projects-file (expand-file-name "projects.org" org-dir))
(defvar jcs/someday-file (expand-file-name "someday.org" org-dir))
(defvar jcs/next-file (expand-file-name "next.org" org-dir))
(defvar jcs/tickler-file (expand-file-name "tickler.org" org-dir))
(defvar jcs/inbox-file (expand-file-name "inbox.org" org-dir))
(defvar jcs/reference-file (expand-file-name "reference.org" org-dir))
(defvar jcs/checklists-file (expand-file-name "checklists.org" org-dir))

;; Load some org-babel dependencies
(use-package restclient)
(use-package ob-restclient)
(use-package ob-sql-mode :disabled)

(use-package org
  :config
  (validate-setq org-directory org-dir
                 org-log-done 'time
                 org-log-into-drawer t
                 org-startup-indented t
                 org-startup-folded t
                 org-default-notes-file jcs/inbox-file
                 org-src-fontify-natively t
                 org-use-fast-todo-selection t
                 org-refile-allow-creating-parent-nodes t
                 org-refile-use-outline-path 'file
                 org-outline-path-complete-in-steps nil
                 ;; Don't ask every time before evaluating an org source block
                 org-confirm-babel-evaluate nil
                 org-agenda-files (list jcs/projects-file
                                        jcs/inbox-file
                                        jcs/someday-file
                                        jcs/next-file
                                        jcs/tickler-file))
  (setq org-refile-targets '((jcs/projects-file . (:maxlevel . 2))
                             (jcs/someday-file . (:level . 0))
                             (jcs/next-file . (:level . 1))
                             (jcs/tickler-file . (:level . 0))
                             (jcs/reference-file . (:level . 1)))
        org-tag-alist (quote (("@work" . ?w)
                              ("@errand" . ?E)
                              ("@home" . ?h)
                              ("@computer" . ?c)
                              (:newline)
                              ("@kasey" . ?k)
                              ("@alex" . ?a)
                              (:newline)
                              ("james" . ?j)
                              ("weekend" . ?W)
                              ("evening" . ?e)
                              ("workstation" . ?K)
                              ("server" . ?s)
                              ("project_idea" . ?p)
			      ("business_hours" . ?b)))
        org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "DOING(o)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  ;; Add a few languages for execution in org source blocks
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (sh . t)
                                 (sql . t)
                                 (emacs-lisp . t)
                                 (elasticsearch . t)
                                 (restclient . t)))
  (defun find-projects-file () (interactive) (find-file jcs/projects-file))
  (defun find-someday-file () (interactive) (find-file jcs/someday-file))
  (defun find-inbox-file () (interactive) (find-file jcs/inbox-file))
  (defun find-next-file () (interactive) (find-file jcs/next-file))
  (defun find-tickler-file () (interactive) (find-file jcs/tickler-file))
  (defun find-reference-file () (interactive) (find-file jcs/reference-file))
  (defun find-checklists-file () (interactive) (find-file jcs/checklists-file))

  (defun visit-todays-log ()
    "Visit buffer for a log file for today's date."
    (interactive)
    (find-file (concat "~/org/log/" (format-time-string
				     "%Y-%m-%d.org" (current-time)))))

  ;; These tend to modify files, so save after doing it
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree-default :after 'org-save-all-org-buffers)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c e p" . find-projects-file)
         ("C-c e s" . find-someday-file)
         ("C-c e i" . find-inbox-file)
         ("C-c e n" . find-next-file)
         ("C-c e t" . find-tickler-file)
         ("C-c e r" . find-reference-file)
         ("C-c e c" . find-checklists-file)
         ("C-c e l" . visit-todays-log)))

(use-package ox-md :ensure f)

(use-package org-agenda
  :ensure f
  :after org
  :config
  ;; Use the current window to open the agenda
  (validate-setq org-agenda-window-setup 'current-window
                 org-agenda-block-separator nil)
  (setq jcs/agenda-files (list jcs/projects-file jcs/tickler-file jcs/next-file))
  (setq org-agenda-custom-commands
        '(("c" "Agenda and tasks"
           ((agenda ""
                    ((org-agenda-files jcs/agenda-files)))
            (todo ""
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '("~/org/inbox.org" "~/org/refile-beorg.org"))))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")
		   (org-agenda-files jcs/agenda-files)))
	    (todo "DOING"
                  ((org-agenda-overriding-header "In Progress")
		   (org-agenda-files jcs/agenda-files)))
            (todo "NEXT"
                  ((org-agenda-overriding-header "Next")
		   (org-agenda-files jcs/agenda-files)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Todo")
		   (org-agenda-files jcs/agenda-files))))))))

(use-package org-capture
  :ensure f
  :after org
  :bind ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file "inbox.org")
           "* TODO %i%?")
          ("n" "Next task [inbox]" entry
           (file "inbox.org")
           "* NEXT %i%?")
          ("T" "Tickler" entry
           (file "tickler.org")
           "* %i%? \n %U")
          ("r" "Reference" entry
           (file+headline "reference.org" "Reference")
           "* %i%? \n %U"))))

(use-package alert
  :config
  (setq alert-default-style 'libnotify))

(use-package org-alert
  :disabled
  :config
  (validate-setq org-alert-notification-title "Org Agenda")
  (validate-setq org-alert-interval (* 60 60))
  (org-alert-enable))

(use-package org-wild-notifier
  :disabled
  :after alert
  :config (org-wild-notifier-mode))


(use-package go-mode)
(use-package company-go)
(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))
(use-package go-errcheck)
(use-package csv-mode)
(use-package sql-indent)
(use-package eldoc :delight :ensure f)

(use-package clojure-mode
  :delight
  :mode ("\\.clj.*\\'" "\\.edn.*\\'")
  :hook
  (clojure-mode . rainbow-delimiters-mode)
  (clojure-mode . paredit-mode)
  :config

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
  :hook
  (clojure-mode . cider-mode)
  ((cider-mode cider-repl-mode) . eldoc-mode)
  (cider-repl-mode . rainbow-delimiters-mode)
  (cider-repl-mode . paredit-mode)
  :bind (:map clojure-mode-map
              ("C-c i" . cider-inspect-last-result))
  :custom (cider-jdk-src-paths '("~/code/clojure-1.8" "~/code/java8"))
  :config
  (validate-setq cider-prompt-for-symbol nil ; Don't prompt for a symbol with `M-.`
                 cider-repl-display-help-banner nil
                 nrepl-log-messages t))

(use-package clj-refactor
  :delight
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c r")))
  :config
  (validate-setq cljr-suppress-middleware-warnings t
                 cljr-favor-prefix-notation nil
                 ;; Lazily build ASTs, instead of immediately on REPL connect
                 cljr-eagerly-build-asts-on-startup nil))

(use-package yasnippet :delight)

;; Try out a linter...
(use-package flycheck-joker)

;; Seed the PRNG anew, from the system's entropy pool
(random t)

(use-package systemd)

(use-package shell-pop
  :custom
  (shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))) "Use eshell.")
  (shell-pop-universal-key "C-t")
  (shell-pop-window-size 30)
  (shell-pop-full-span t)
  (shell-pop-window-position "bottom"))

(use-package browse-url
  :ensure f
  ;; browse-url decides not to use xdg-open if you don't use one of a
  ;; handful of desktop environments...
  :config (when (eq system-type 'gnu/linux)
            (validate-setq browse-url-browser-function 'browse-url-xdg-open)))

(use-package rg)

(use-package wgrep)

(use-package nov)

(use-package json-snatcher
  :config (validate-setq jsons-path-printer 'jsons-print-path-jq))

(use-package json-mode)

(use-package rust-mode
  :custom (rust-format-on-save t))

(use-package lsp-rust
  :config (setq lsp-rust-rls-command '("rls"))
  :hook (rust-mode . lsp-rust-enable))

(use-package racer
  :hook ((rust-mode . racer-mode)
         (racer-mode . eldoc-mode)))

(use-package atomic-chrome
  :config (atomic-chrome-start-server))

(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c n". crux-cleanup-buffer-or-region)))

;;; init.el ends here
