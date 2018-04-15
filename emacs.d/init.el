;;; init --- configuration starting point -*- no-byte-compile: t -*-

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
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
(use-package bind-key)

(use-package no-littering
  :config
  (require 'files)
  (validate-setq auto-save-file-name-transforms
                 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package init-wm
  :load-path "lisp")

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

;; Turn off the toolbar, menu bar, and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Handy to get the current font/size that you've got:
;; (insert "\n(set-frame-font \"" (cdr (assoc 'font (frame-parameters))) "\")")

(when (memq window-system '(mac ns))
  (set-frame-font "-*-Menlo-normal-normal-normal-*-11-*-*-*-m-0-iso10646-1"))
(when (memq window-system '(x))
  (set-frame-font "Hack 9"))

(use-package prog-mode
  :ensure f
  :config (global-prettify-symbols-mode)
  (defun indicate-buffer-boundaries-left ()
    (setq indicate-buffer-boundaries 'left))
  (add-hook 'prog-mode-hook #'indicate-buffer-boundaries-left))

;; Quick access to a few files
(defun find-init-file ()
  "Open the init file for editing."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "C-c e e") 'find-init-file)


(defvar org-dir "~/org/")
(defvar jcs/projects-file (expand-file-name "projects.org" org-dir))
(defvar jcs/someday-file (expand-file-name "someday.org" org-dir))
(defvar jcs/next-file (expand-file-name "next.org" org-dir))
(defvar jcs/tickler-file (expand-file-name "tickler.org" org-dir))
(defvar jcs/inbox-file (expand-file-name "inbox.org" org-dir))
(defvar jcs/reference-file (expand-file-name "reference.org" org-dir))
(defvar jcs/checklists-file (expand-file-name "checklists.org" org-dir))

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
                             (jcs/tickler-file . (:level . 1))
                             (jcs/reference-file . (:level . 1)))
        org-tag-alist (quote (("@work" . ?w)
                              ("@errand" . ?E)
                              ("@home" . ?h)
                              ("@computer" . ?c)
                              (:newline)
                              ("@kasey" . ?k)
                              ("@alex" . ?a)
                              (:newline)
                              ("weekend" . ?W)
                              ("evening" . ?e)
                              ("business_hours" . ?b)
                              (:newline)
                              ("james" . ?j)
                              ("workstation" . ?K)
                              ("server" . ?s)
                              ("project_idea" . ?p)))
        org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "DOING(o)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
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
    ;; (find-file (concat "~/org/log/" (format-time-string
    ;; 				     "%Y-%m-%d.org" (current-time))))
    (find-file "~/org/log.org"))

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
         ("C-c e l" . visit-todays-log)

	 ;; Below stolen from
	 ;; https://github.com/raxod502/radian/blob/ee92ea6cb0473bf7d20c6d381753011312ef4a52/radian-emacs/radian-org.el
         :map org-mode-map

         ;; Prevent Org from overriding the bindings for windmove. By
         ;; default, these keys are mapped to `org-shiftleft', etc.
         ("S-<left>" . nil)
         ("S-<right>" . nil)
         ("S-<up>" . nil)
         ("S-<down>" . nil)

         ;; Add replacements for the keybindings we just removed.
         ;; C-<left> and C-<right> are unused by Org. C-<up> and
         ;; C-<down> are bound to `org-backward-paragraph', etc. (but
         ;; see below).
         ("C-<left>" . org-shiftleft)
         ("C-<right>" . org-shiftright)
         ("C-<up>" . org-shiftup)
         ("C-<down>" . org-shiftdown)

         ;; By default, Org maps C-<up> to `org-backward-paragraph'
         ;; instead of `backward-paragraph' (and analogously for
         ;; C-<down>). However, it doesn't do the same remapping for
         ;; the other bindings of `backward-paragraph' (e.g. M-{).
         ;; Here we establish that remapping. (This is important since
         ;; we remap C-<up> and C-<down> to other things, above. So
         ;; otherwise there would be no easy way to invoke
         ;; `org-backward-paragraph' and `org-forward-paragraph'.)
         ([remap backward-paragraph] . org-backward-paragraph)
	 ([remap forward-paragraph] . org-forward-paragraph)))

(use-package ox-md :ensure org)

(use-package restclient)
(use-package ob-restclient)

  ;; Use es-mode for ElasticSearch buffers
(use-package es-mode)
(use-package ob-elasticsearch
  :ensure es-mode
  :config (add-to-list 'org-babel-load-languages '(elasticsearch . t)))

;; org-babel
(use-package ob
  :ensure org
  :config
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((clojure . t)
                                 (sh . t)
                                 (sql . t)
                                 (emacs-lisp . t)
                                 (elasticsearch . t)
                                 (restclient . t))))

(use-package org-agenda
  :ensure org
  :after org
  :bind (:map org-agenda-mode-map

	      ;; Prevent Org Agenda from overriding the bindings for
	      ;; windmove.
	      ("S-<up>" . nil)
	      ("S-<down>" . nil)
	      ("S-<left>" . nil)
	      ("S-<right>" . nil)

	      ;; Same routine as above. Now for Org Agenda, we could use
	      ;; C-up and C-down because M-{ and M-} are bound to the same
	      ;; commands. But I think it's best to take the same approach
	      ;; as before, for consistency.
	      ("C-<left>" . org-agenda-do-date-earlier)
	      ("C-<right>" . org-agenda-do-date-later))
  :config
  ;; Use the current window to open the agenda
  (validate-setq org-agenda-window-setup 'current-window
                 org-agenda-block-separator nil)
  (setq jcs/agenda-files (list jcs/projects-file jcs/tickler-file
			       jcs/next-file jcs/inbox-file))
  (setq org-agenda-custom-commands
        '(("c" "Agenda and tasks"
           ((agenda ""
                    ((org-agenda-files jcs/agenda-files)))
            (todo ""
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files '("~/org/inbox.org"))))
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
		   (org-agenda-files jcs/agenda-files)))
	    (todo "HOLD"
                  ((org-agenda-overriding-header "On Hold")
		   (org-agenda-files jcs/agenda-files))))))))

(use-package org-capture
  :ensure org
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

(use-package autorevert
  :ensure f
  :config
  (validate-setq global-auto-revert-non-file-buffers t ; Refresh dired buffers
                 auto-revert-verbose nil) ; but do it quietly
  ;; Auto-refresh buffers
  (global-auto-revert-mode))

(use-package dired
  :ensure f
  :config (setq dired-listing-switches "-alhv"))

(use-package saveplace
  :ensure f
  :when (version< "25" emacs-version)
  :config (save-place-mode))

;;; Misc settings
(validate-setq inhibit-splash-screen t  ; Don't show the splash screen
               ring-bell-function 'ignore ; Just ignore error notifications
               vc-follow-symlinks t ; even when they're in version control
               indent-tabs-mode nil ; Don't use tabs unless buffer-local
               select-enable-primary t
               save-interprogram-paste-before-kill t
               mouse-yank-at-point t
	       ;; When scrolling, make sure to come back to the same spot
               scroll-preserve-screen-position 'always
               scroll-error-top-bottom t ; Scroll similar to vim
               )

;;; Packages
(use-package recentf
  :ensure f
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package files
  :ensure f
  :config
  (validate-setq backup-directory-alist ; Save backups to a central location
                 `(("." . ,(no-littering-expand-var-file-name "backups/")))
		 auto-save-file-name-transforms
		 `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

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
  :config
  (validate-setq whitespace-line-column 80
                 whitespace-style '(face trailing lines-tail))
  :hook (prog-mode . whitespace-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (validate-setq markdown-fontify-code-blocks-natively t))

(use-package minions
  :config (minions-mode))

(use-package simple
  :ensure f
  :config (column-number-mode)
  :bind ("M-SPC" . cycle-spacing)
  :hook ((text-mode org-mode markdown-mode) . turn-on-auto-fill))

;; Ensure that when we go to a new line, it's indented properly
(use-package electric
  :config (electric-indent-mode))

;; Highlight matching parens
(use-package paren
  :config (show-paren-mode))

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
  :config
  ;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
  :hook ((emacs-lisp-mode ielm-mode) . turn-on-elisp-slime-nav-mode))

(use-package highlight-symbol
  :bind (:map mode-specific-map
              ("h h" . highlight-symbol)
              ("h r" . highlight-symbol-remove-all)
              ("h l" . highlight-symbol-list-all)
              ("h n" . highlight-symbol-next)
              ("h p" . highlight-symbol-prev)
              ("h a" . highlight-symbol-nav-mode)))

(use-package idle-highlight-mode
  :disabled
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
         ("C-s" . counsel-grep-or-swiper)
         ("M-y" . counsel-yank-pop)
         :map ivy-minibuffer-map
         ("M-y" . ivy-next-line))
  :config
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (validate-setq counsel-grep-base-command
                 "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))

(use-package projectile
  :config (projectile-mode))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package js2-mode
  :mode "\\.js\\'")

(use-package magit
  :bind (("C-c g"   . magit-status)
         ("C-c M-g" . magit-dispatch-popup))
  :custom
  (magit-branch-prefer-remote-upstream t)
  (magit-branch-adjust-remote-upstream-alist '(("upstream/master" . "issue-")))
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package git-timemachine)

;; ghub and dash are required by threatgrid.el
(use-package ghub)
(use-package dash
  :config (dash-enable-font-lock))
(use-package threatgrid
  :ensure f
  :commands (preq tg-insert-weekly-work-report tg-create-tb-issue)
  :load-path "lisp")

(use-package windmove
  :config (windmove-default-keybindings))

(use-package winner
  :config (winner-mode))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package paredit-everywhere
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

(use-package smart-jump
  :config (smart-jump-setup-default-registers))

(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.sls\\'" . yaml-mode)))

(use-package which-key
  :config (which-key-mode))

;; Some keybinds for this mode:
;; `diff-hl-diff-goto-hunk'  C-x v =
;; `diff-hl-revert-hunk'     C-x v n
;; `diff-hl-previous-hunk'   C-x v [
;; `diff-hl-next-hunk'       C-x v ]
(use-package diff-hl
  :config
  (validate-setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh t))

(use-package super-save
  :init (validate-setq auto-save-default nil)
  :config
  (validate-setq super-save-auto-save-when-idle t)
  (super-save-mode +1))

(use-package smart-mode-line
  :config (sml/setup))

;; Turn on line numbers everywhere
(use-package nlinum
  :config (global-nlinum-mode))

(use-package dired-collapse)

(use-package notmuch
  :bind (("C-x m" . notmuch))
  :custom
  (message-sendmail-envelope-from 'header)
  (send-mail-function (quote sendmail-send-it))
  :config
  (require 'gnus-art)
  (require 'message)
  (validate-setq mail-specify-envelope-from t)

  ;; Borrowed from https://notmuchmail.org/emacstips/
  (defun my-notmuch-show-view-as-patch ()
    "View the the current message as a patch."
    (interactive)
    (let* ((id (notmuch-show-get-message-id))
           (msg (notmuch-show-get-message-properties))
           (part (notmuch-show-get-part-properties))
           (subject (concat "Subject: " (notmuch-show-get-subject) "\n"))
           (diff-default-read-only t)
           (buf (get-buffer-create (concat "*notmuch-patch-" id "*")))
           (map (make-sparse-keymap)))
      (define-key map "q" 'notmuch-bury-or-kill-this-buffer)
      (switch-to-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert subject)
        (insert (notmuch-get-bodypart-text msg part nil)))
      (set-buffer-modified-p nil)
      (diff-mode)
      (lexical-let ((new-ro-bind (cons 'buffer-read-only map)))
        (add-to-list 'minor-mode-overriding-map-alist new-ro-bind))
      (goto-char (point-min))))

  (define-key 'notmuch-show-part-map "d" 'my-notmuch-show-view-as-patch))

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

(use-package go-mode)
(use-package company-go)
(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))
(use-package go-errcheck)
(use-package csv-mode)
(use-package sql-indent)
(use-package eldoc
  :ensure f
  :when (version< "25" emacs-version)
  :config (global-eldoc-mode))

(use-package clojure-mode
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
                              nil))))))

;;; Cider
(use-package cider
  :hook
  (clojure-mode . cider-mode)
  ((cider-mode cider-repl-mode) . eldoc-mode)
  (cider-repl-mode . rainbow-delimiters-mode)
  (cider-repl-mode . paredit-mode)
  :bind (:map clojure-mode-map
              ("C-c i" . cider-inspect-last-result))
  :custom
  (cider-jdk-src-paths '("~/code/clojure"
			 "/usr/lib/jvm/java-8-openjdk/src.zip"))
  (cider-save-file-on-load t)
  :config
  (validate-setq cider-prompt-for-symbol nil ; Don't prompt for a symbol with `M-.`
                 cider-repl-display-help-banner nil
                 nrepl-log-messages t))

(use-package yasnippet)

(use-package clj-refactor
  :hook (clojure-mode . (lambda ()
                          (clj-refactor-mode 1)
                          (yas-minor-mode 1)
                          (cljr-add-keybindings-with-prefix "C-c r")))
  :config
  (validate-setq cljr-suppress-middleware-warnings t
                 cljr-favor-prefix-notation nil
                 ;; Lazily build ASTs, instead of immediately on REPL connect
                 cljr-eagerly-build-asts-on-startup nil))

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

(use-package savehist
  :config (savehist-mode))

(use-package atomic-chrome
  :config (atomic-chrome-start-server))

(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c n". crux-cleanup-buffer-or-region)))

(use-package org-rich-yank
  :after org
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank)))

(use-package help
  :ensure f
  :config (temp-buffer-resize-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
	 ("C-h v" . helpful-variable)
	 ("C-h k" . helpful-key)
	 :map emacs-lisp-mode-map
	 ("C-c C-d" . helpful-at-point)))


;;; init.el ends here
