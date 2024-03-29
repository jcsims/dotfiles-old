;;; init --- configuration starting point -*- no-byte-compile: t -*-

;;; Commentary:
;; Most of what is found in these files has been pulled from the
;; dotfiles of others.  Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:

;; Prefer the newer version of a file, whether it's compiled or not.
(setq load-prefer-newer t)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Seed the PRNG anew, from the system's entropy pool
(random t)

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

;; Setup use-package

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t
      use-package-compute-statistics t
      use-package-verbose t)
(use-package bind-key)

(use-package gcmh
  :custom (gcmh-verbose t)
  :config (gcmh-mode 1))

;; Some combination of GNU TLS and Emacs fail to retrieve archive
;; contents over https.
;; https://www.reddit.com/r/emacs/comments/cdei4p/failed_to_download_gnu_archive_bad_request/etw48ux
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
(if (and (version< emacs-version "28.3") (>= libgnutls-version 30600))
    (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"))

(setq source-directory (concat "~/code/emacs-" emacs-version))

(use-package no-littering
  :config
  (require 'files)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(use-package auth-source
  :ensure f
  :custom (auth-sources '("~/.authinfo.gpg")))

;;; Personal info
(setq user-full-name "Chris Sims"
      user-mail-address "chris@jcsi.ms")

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; Blank scratch buffer
(setq initial-scratch-message nil)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Get rid of the insert key. I never use it, and I turn it on
;; accidentally all the time
(global-set-key (kbd "<insert>") nil)

(when (featurep 'ns)
  (defun ns-raise-emacs ()
    "Raise Emacs."
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  (defun ns-raise-emacs-with-frame (frame)
    "Raise Emacs and select the provided frame."
    (with-selected-frame frame
      (when (display-graphic-p)
        (ns-raise-emacs))))

  (add-hook 'after-make-frame-functions 'ns-raise-emacs-with-frame))

(setq isearch-allow-scroll t)
(global-set-key (kbd "C-S") 'isearch-forward-regexp)
(global-set-key (kbd "C-R") 'isearch-backward-regexp)
;; (global-set-key (kbd "C-M-s") 'isearch-forward)
;; (global-set-key (kbd "C-M-r") 'isearch-backward)

;; Turn off the toolbar, menu bar, and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

;; Font
(if (eq system-type 'gnu/linux)
    (set-frame-font "Hack 9")
  (set-frame-font "Hack Nerd Font 12"))

;;; Themes
(defvar jcs-active-theme)
(defvar jcs-light-theme)
(defvar jcs-dark-theme)

;;; Themes
(use-package solarized-theme)
(use-package color-theme-sanityinc-tomorrow)
(use-package base16-theme)
(use-package gruvbox-theme)

;; emacs-plus offers this handy hook to tie in to system appearance
(defun jcs/apply-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (when (featurep 'ns)
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (modus-themes-load-operandi))
      ('dark (modus-themes-load-vivendi)))))

(use-package modus-themes
  :disabled
  :init (modus-themes-load-themes)
  :config (if (eq system-type 'darwin)
              (jcs/apply-theme ns-system-appearance)
            (load-theme 'gruvbox-dark-soft t))
  (setq ns-system-appearance-change-functions '(jcs/apply-theme)))

;; (setq base16-theme-256-color-source 'base16-shell
;;       jcs-active-theme 'base16-tomorrow-night-eighties
;;       jcs-light-theme 'base16-gruvbox-light-hard
;;       jcs-dark-theme 'base16-tomorrow-night-eighties)

;; (setq jcs-active-theme 'solarized-gruvbox-dark
;;       jcs-light-theme 'solarized-gruvbox-light
;;       jcs-dark-theme 'solarized-gruvbox-dark)

(setq jcs-active-theme 'sanityinc-tomorrow-eighties
      jcs-light-theme 'sanityinc-tomorrow-day
      jcs-dark-theme 'sanityinc-tomorrow-eighties)

;; (setq jcs-active-theme 'gruvbox-dark-hard
;;       jcs-light-theme 'gruvbox-light-medium
;;       jcs-dark-theme 'gruvbox-dark-hard)

(load-theme jcs-active-theme t)

(defun toggle-dark-light-theme ()
  "Toggle the current theme between light and dark."
  (interactive)
  (if (eq jcs-active-theme jcs-light-theme)
      (setq jcs-active-theme jcs-dark-theme)
    (setq jcs-active-theme jcs-light-theme))
  (load-theme jcs-active-theme t)
  (sml/apply-theme 'respectful))

(use-package smart-mode-line
  :config
  (sml/setup))

(use-package prog-mode
  :ensure f
  :config
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
(defvar jcs/beorg-file (expand-file-name "beorg.org" org-dir))
(defvar jcs/reference-file (expand-file-name "reference/reference.org" org-dir))
(defvar jcs/checklists-file (expand-file-name "reference/checklists.org" org-dir))
(defvar jcs/archive-file (expand-file-name "archive/archive.org" org-dir))
(defvar jcs/habit-file (expand-file-name "habit.org" org-dir))
(defvar jcs/log-dir (expand-file-name "log/" org-dir))

(use-package org
  :custom
  (org-highest-priority ?A)
  (org-lowest-priority ?D)
  (org-default-priority ?C)
  :config
  (setq org-hide-leading-stars t
        org-hide-emphasis-markers t ;; Hide things like `*` for bold, etc.
        org-archive-location (concat jcs/archive-file "::* From %s")
        org-directory org-dir
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
                               jcs/tickler-file
                               (expand-file-name "log/" org-dir)))
  (setq org-refile-targets '((jcs/projects-file . (:maxlevel . 2))
                             (jcs/someday-file . (:level . 0))
                             (jcs/beorg-file . (:level . 0))
                             (jcs/next-file . (:level . 1))
                             (jcs/tickler-file . (:level . 1))
                             (jcs/reference-file . (:level . 1)))
        org-tag-alist (quote (("@work" . ?w)
                              ("@errand" . ?E)
                              ("@home" . ?h)
                              ("@computer" . ?c)
                              ("@phone" . ?p)
                              (:newline)
                              ("@kasey" . ?k)
                              ("@alex" . ?a)
                              ("@parents" . ?P)
                              ("james" . ?j)
                              (:newline)
                              ("weekend" . ?W)
                              ("evening" . ?e)
                              ("business_hours" . ?b)
                              ("learning" . ?l)
                              ("project_idea" . ?i)
                              (:newline)
                              ("high_effort" . ?H)
                              ("quick_win" . ?q)
                              ("focus" . ?f)))
        org-todo-keywords
        (quote ((sequence "TODO(t)" "DOING(o)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (defun find-projects-file () (interactive) (find-file jcs/projects-file))
  (defun find-someday-file () (interactive) (find-file jcs/someday-file))
  (defun find-inbox-file () (interactive) (find-file jcs/inbox-file))
  (defun find-next-file () (interactive) (find-file jcs/next-file))
  (defun find-tickler-file () (interactive) (find-file jcs/tickler-file))
  (defun find-reference-file () (interactive) (find-file jcs/reference-file))
  (defun find-checklists-file () (interactive) (find-file jcs/checklists-file))
  (defun find-habit-file () (interactive) (find-file jcs/habit-file))

  ;; TODO: Write some helpers for this, e.g.:
  ;; - Search across logs
  ;; - List top-level headings across logs
  (defun visit-todays-log ()
    "Visit buffer for a log file for today's date."
    (interactive)
    (find-file (concat "~/org/log/" (format-time-string
                                     "%Y-%m-%d.org" (current-time)))))

  (defun jcs/find-log-file ()
    (interactive)
    (find-file (expand-file-name "log.org" org-dir)))

  ;; These tend to modify files, so save after doing it
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree-default :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-archive-default-with-confirmation :after 'org-save-all-org-buffers)
  (advice-add 'org-agenda-todo :after 'org-save-all-org-buffers)

  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c e p" . find-projects-file)
         ("C-c e s" . find-someday-file)
         ("C-c e i" . find-inbox-file)
         ("C-c e n" . find-next-file)
         ("C-c e t" . find-tickler-file)
         ("C-c e r" . find-reference-file)
         ("C-c e c" . find-checklists-file)
         ("C-c e h" . find-habit-file)
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

(use-package org-tempo :ensure org)

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
                                 (shell . t)
                                 (sql . t)
                                 (emacs-lisp . t)
                                 (elasticsearch . t)
                                 (restclient . t))))
(use-package org-habit
  :ensure org
  :after org
  :config
  (setq org-habit-graph-column 80))

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
  ;; TODO: Use a `let*` binding here and turn on lexical scoping for
  ;; this file.
  ;; Use the current window to open the agenda
  (setq org-agenda-window-setup 'current-window
        org-agenda-block-separator nil
        jcs/agenda-files (list jcs/projects-file
                               jcs/tickler-file
                               jcs/next-file
                               jcs/inbox-file
                               jcs/habit-file
                               jcs/log-dir)
        jcs/non-inbox-files (remq jcs/beorg-file
                                  (remq jcs/inbox-file
                                        jcs/agenda-files))
        jcs/inbox-files (list jcs/inbox-file
                              jcs/beorg-file))
  (setq org-agenda-custom-commands
        '(("c" "Agenda and tasks"
           ((agenda ""
                    ((org-agenda-files jcs/agenda-files)
                     (org-agenda-skip-function
                      '(org-agenda-skip-if nil '(todo done)))))
            (todo ""
                  ((org-agenda-overriding-header "To Refile")
                   (org-agenda-files jcs/inbox-files)))
            (todo "WAITING"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-files jcs/non-inbox-files)))
            (todo "DOING"
                  ((org-agenda-overriding-header "In Progress")
                   (org-agenda-files jcs/non-inbox-files)))
            (todo "TODO"
                  ((org-agenda-overriding-header "Todo")
                   (org-agenda-files jcs/non-inbox-files)
                   (org-agenda-skip-function
                    '(org-agenda-skip-if nil '(scheduled deadline)))))
            (todo "HOLD"
                  ((org-agenda-overriding-header "On Hold")
                   (org-agenda-files jcs/non-inbox-files)
                   (org-agenda-skip-function
                    '(org-agenda-skip-if nil '(scheduled deadline))))))))))

(use-package org-capture
  :ensure org
  :init
  ;; ;; borrowed from https://fuco1.github.io/2017-09-02-Maximize-the-org-capture-buffer.html
  ;; (defvar my-org-capture-before-config nil
  ;;   "Window configuration before `org-capture'.")

  (defun my-org-capture-cleanup ()
    "Clean up the frame created while capturing."
    ;; In case we run capture from emacs itself and not an external app,
    ;; we want to restore the old window config
    (-when-let ((&alist 'name name) (frame-parameters))
      (when (equal name "capture")
        (delete-frame))))

  :bind ("C-c c" . org-capture)
  :hook ((org-capture-after-finalize . my-org-capture-cleanup)
         (org-capture-mode . delete-other-windows))
  :config
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file "inbox.org")
           "* TODO %i%?")
          ("w" "Work task [inbox]" entry
           (file "inbox.org")
           "* TODO %i%?      :@work:")
          ("T" "Tickler" entry
           (file "tickler.org")
           "* %i%? \n %U")
          ("r" "Reference" entry
           (file+headline "reference/reference.org" "Reference")
           "* %i%? \n %U")
          ("W" "Review: Weekly Review" entry
           ;; Destination
           (file+datetree "~/org/weekly-reviews.org")
           ;; Capture template
           (file "~/org/weekly-review-template.org"))
          ("h" "Housework log" entry
           (file+datetree "~/org/log/housework-log.org")
           "* %i%? \n %U"))))

(use-package autorevert
  :ensure f
  :config
  (setq global-auto-revert-non-file-buffers t ; Refresh dired buffers
        auto-revert-verbose nil)              ; but do it quietly
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
(setq inhibit-splash-screen t  ; Don't show the splash screen
      ring-bell-function 'ignore ; Just ignore error notifications
      indent-tabs-mode nil ; Don't use tabs unless buffer-local
      select-enable-primary t
      save-interprogram-paste-before-kill t
      mouse-yank-at-point t
      ;; When scrolling, make sure to come back to the same spot
      scroll-preserve-screen-position 'always
      scroll-error-top-bottom t ; Scroll similar to vim
      )

;;; Packages
(use-package vc-hooks
  :ensure f
  :config (setq vc-follow-symlinks t ; even when they're in version control
                ))

(use-package recentf
  :ensure f
  :config
  (add-to-list 'recentf-exclude "^/\\(?:ssh\\|su\\|sudo\\)?:")
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

(use-package files
  :ensure f
  :config
  (auto-save-visited-mode 1)
  (setq backup-directory-alist ; Save backups to a central location
        `(("." . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

;; Handles ssh-agent and gpg-agent configuration from `keychain`
(use-package keychain-environment
  :if (eq system-type 'gnu/linux)
  :config (keychain-refresh-environment))

;; Allow for seamless gpg interaction
(use-package epa-file
  :ensure f
  :config (epa-file-enable))

;; Used for async package updating in paradox
(use-package async)
(use-package paradox
  :after (auth-source epa-file epg exec-path-from-shell)
  :commands (paradox-list-packages)
  :config
  (setq paradox-execute-asynchronously t
        paradox-github-token (cadr (auth-source-user-and-password
                                    "api.github.com" "jcsims^paradox")))
  (paradox-enable))

(use-package macrostep
  :bind ("C-c m" . macrostep-expand))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config (exec-path-from-shell-initialize))

(use-package re-builder
  :ensure f
  :bind (("C-c R" . re-builder))
  :config (setq reb-re-syntax 'string))

;; External user config
(use-package init-funcs
  :ensure f
  :load-path "lisp")

(use-package whitespace
  :config
  (setq whitespace-line-column 80
        fill-column 80
        whitespace-style '(face tabs empty trailing lines-tail))
  :hook
  (prog-mode . whitespace-mode)
  (text-mode . whitespace-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config (setq markdown-fontify-code-blocks-natively t))

(use-package minions
  :config
  (setq minions-direct '(flycheck-mode
                         vlf-mode
                         lsp-mode
                         whitespace-cleanup-mode))
  (minions-mode))

(use-package simple
  :ensure f
  :after org
  :hook (org-mode . visual-line-mode)
  :config
  (column-number-mode)
  (setq-default what-cursor-show-names t)
  :bind ("M-SPC" . cycle-spacing)
  :hook ((text-mode org-mode markdown-mode) . turn-on-auto-fill))

(use-package text-mode
  :ensure f
  :hook (text-mode-hook . indicate-buffer-boundaries-left))

(use-package tramp
  :ensure f
  :defer t
  :config
  (add-to-list 'tramp-default-proxies-alist '(nil "\\`root\\'" "/ssh:%h:"))
  (add-to-list 'tramp-default-proxies-alist '("localhost" nil nil))
  (add-to-list 'tramp-default-proxies-alist
               (list (regexp-quote (system-name)) nil nil)))

;; Ensure that when we go to a new line, it's indented properly
(use-package electric
  :config (electric-indent-mode))

;; Highlight matching parens
(use-package paren
  :config (show-paren-mode))

;; Ensure that a server is running for quicker start times
(use-package server
  :if (display-graphic-p)
  :config (unless (server-running-p)
            (server-start)))

(use-package atomic-chrome
  :if (display-graphic-p)
  :config
  (setq atomic-chrome-url-major-mode-alist
        '(("github\\.com" . gfm-mode)
          ("github\\.threatbuild\\.com" . gfm-mode)))
  (atomic-chrome-start-server))

;; Work-specific code - should be encrypted!
(defvar work-init (concat user-emacs-directory "lisp/init-work.el.gpg"))
(if (file-exists-p work-init)
    (load work-init))

;; Flyspell mode
(use-package flyspell
  :hook
  (text-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode))

(use-package company
  :config
  (setq company-idle-delay .3)                          ; decrease delay before autocompletion popup shows
  (setq company-echo-delay 0)                           ; remove annoying blinking
  (global-company-mode))

(use-package company-quickhelp
  :config (company-quickhelp-mode))

(use-package elisp-slime-nav
  :config
  ;; Enable M-. and M-, along with C-c C-d {c,C-d} for elisp
  :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

(use-package highlight-symbol
  :bind (:map mode-specific-map
              ("h h" . highlight-symbol)
              ("h r" . highlight-symbol-remove-all)
              ("h l" . highlight-symbol-list-all)
              ("h n" . highlight-symbol-next)
              ("h p" . highlight-symbol-prev)
              ("h a" . highlight-symbol-nav-mode)))

(use-package flycheck
  :config (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("M-n" . flycheck-next-error)
              ("M-p" . flycheck-previous-error))
  :custom (flycheck-global-modes '(not org-mode
                                       cider-repl-mode)))

;; Borrowed from https://github.com/daviwil/dotfiles/commit/58eff6723515e438443b9feb87735624acd23c73
(defun jcs/minibuffer-backward-kill (arg)
  "When completing a filename in the minibuffer, kill according to path.
Passes ARG onto `zap-to-char` or `backward-kill-word` if used."
  (interactive "p")
  (if minibuffer-completing-file-name
      ;; Borrowed from https://github.com/raxod502/selectrum/issues/498#issuecomment-803283608
      (if (string-match-p "/." (minibuffer-contents))
          (zap-up-to-char (- arg) ?/)
        (delete-minibuffer-contents))
    (backward-kill-word arg)))

(use-package vertico
  ;; :disabled
  :init (vertico-mode)
  :custom (vertico-cycle t)
  :bind (:map vertico-map
              ("M-<backspace>" . jcs/minibuffer-backward-kill)))

;; (use-package icomplete
;;   :ensure f
;;   :demand
;;   :config
;;   (fido-mode 1)
;;   (fido-vertical-mode)
;;   (setq  icomplete-scroll t
;;          icomplete-show-matches-on-no-input t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package consult
  :demand ;; never want to lazy-load this package
  :bind (("M-y" . consult-yank-from-kill-ring)
         ([remap isearch-forward-regexp] . consult-line)))

(use-package projectile
  :demand ;; never want to lazy-load this package
  :after consult
  :init
  (setq projectile-project-search-path '("~/code" "~/dev"))
  (projectile-mode +1)
  :bind (("C-c p" . projectile-command-map)
         :map projectile-mode-map
         ("C-c p" . projectile-command-map)
         ;; This is handy on macOS
         ("s-p" . projectile-command-map)
         :map projectile-command-map
         ;; I'm used to this binding, and ripgrep is faster
         ("s s" . projectile-ripgrep)))

(use-package marginalia
  :init (marginalia-mode))

(use-package embark
  :demand
  :bind ("C-o" . embark-act))

(use-package magit
  :demand
  :bind (("C-c g"   . magit-status)
         ("C-c M-g" . magit-dispatch))
  :custom
  (magit-branch-prefer-remote-upstream t)
  (magit-branch-adjust-remote-upstream-alist '(("upstream/master" . "issue-")))
  (magit-save-repository-buffers 'dontask)
  :config
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-stashes
                          'append))

(use-package magit-libgit :after magit)

(use-package git-timemachine)

(use-package windmove
  :config (windmove-default-keybindings))

(use-package paredit
  :hook (emacs-lisp-mode . paredit-mode))

(use-package paredit-everywhere
  :hook (prog-mode . paredit-everywhere-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package dockerfile-mode
  :mode "Dockerfile")
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
  ;;(setq diff-hl-draw-borders nil)
  (global-diff-hl-mode)
  (diff-hl-dired-mode)
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package anzu
  :config
  (global-anzu-mode)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

(use-package display-line-numbers
  :ensure f
  :config
  (setq display-line-numbers-width-start t)
  (global-display-line-numbers-mode))

(use-package multiple-cursors
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C->" . mc/mark-all-like-this)))

(use-package clojure-mode
  :hook
  (clojure-mode . paredit-mode)
  :mode (("\\.edn\\'" . clojure-mode))
  :config
  (define-clojure-indent
    (prop/for-all 1)))

(use-package cider
  :hook
  ((clojure-mode . cider-mode)
   (cider-repl-mode . paredit-mode)
   (cider-repl-mode . cider-company-enable-fuzzy-completion)
   (cider-mode . cider-company-enable-fuzzy-completion))
  :bind (:map clojure-mode-map
              ("C-c i" . cider-inspect-last-result)
              ([remap cider-find-var] . lsp-find-definition))
  :custom
  ;; TODO: update these paths
  (cider-jdk-src-paths '("~/code/clojure-sources"
                         "/usr/local/opt/java11/libexec/openjdk.jdk/Contents/Home/lib/src.zip"))
  (cider-save-file-on-load t)
  (cider-repl-use-pretty-printing t)
  (nrepl-use-ssh-fallback-for-remote-hosts t)
  (cider-repl-print-length nil)
  (cider-auto-jump-to-error 'errors-only)
  :config
  ;; kill REPL buffers for a project as well
  (add-to-list 'project-kill-buffer-conditions
               '(derived-mode . cider-repl-mode)
               t)
  (setq cider-prompt-for-symbol nil ; Don't prompt for a symbol with `M-.`
        cider-repl-display-help-banner nil
        nrepl-log-messages t
        cider-known-endpoints '(("Face" "localhost" "4242")
                                ("Remote" "localhost" "8842")
                                ("Threatbrain Server" "localhost" "4243")
                                ("Integration Service" "localhost" "4244")
                                ("GUNDAM" "localhost" "4245"))))


(use-package systemd :if (eq system-type 'gnu/linux))

(use-package browse-url
  :ensure f
  ;; browse-url decides not to use xdg-open if you don't use one of a
  ;; handful of desktop environments...
  :config (when (eq system-type 'gnu/linux)
            (setq browse-url-browser-function 'browse-url-xdg-open)))

(use-package rg)

(use-package wgrep)

(use-package json-snatcher
  :config (setq jsons-path-printer 'jsons-print-path-jq))

(use-package json-mode
  :custom (json-reformat:indent-width 2))

(use-package jq-mode)

;; LSP
(use-package lsp-mode
  :hook ((rust-mode . lsp)
         (clojure-mode . lsp))
  :config
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-enable-indentation nil)
  ;; Using a locally-built version
  ;;:custom (lsp-clojure-custom-server-command "/Users/jcsims/code/clojure-lsp/clojure-lsp")
  :commands lsp)

(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :bind (:map lsp-ui-mode-map
              ("C-M-." . xref-find-references)
              ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
              ([remap xref-find-references] . lsp-ui-peek-find-references))
  :custom (lsp-ui-sideline-show-code-actions nil))

(use-package lsp-treemacs
  :after lsp-mode
  :config (setq treemacs-space-between-root-nodes nil))

(use-package rustic)

(use-package flycheck-rust
  :after rust-mode
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package savehist
  :ensure f
  :config (savehist-mode))

(use-package crux
  :bind (("C-x 4 t" . crux-transpose-windows)
         ("C-c n". cleanup-buffer)
         ("C-a" . crux-move-beginning-of-line)))

(use-package help
  :ensure f
  :config (temp-buffer-resize-mode))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         :map emacs-lisp-mode-map
         ("C-c C-d" . helpful-at-point)))

(use-package git-link
  :config
  (add-to-list 'git-link-remote-alist
               '("github\\.threatbuild\\.com" git-link-github)))

(use-package buffer-move
  :bind (("C-S-<up>" . buf-move-up)
         ("C-S-<down>" . buf-move-down)
         ("C-S-<right>" . buf-move-right)
         ("C-S-<left>" . buf-move-left)))

(use-package groovy-mode
  :hook (groovy-mode . (lambda ()
                         (setq indent-tabs-mode nil)
                         (setq tab-width 2)))
  :custom (groovy-indent-offset 2))

(use-package hl-todo
  :config (global-hl-todo-mode))

(use-package pkgbuild-mode
  :custom (pkgbuild-update-sums-on-save nil)
  :if (eq system-type 'gnu/linux))

(use-package vlf
  :config (require 'vlf-setup))

(use-package newcomment
  :ensure f
  :config (global-set-key [remap comment-dwim] #'comment-line))

;; Emoji support
(use-package unicode-fonts
  :config
  (unicode-fonts-setup))

(use-package whitespace-cleanup-mode
  :config (global-whitespace-cleanup-mode))

(use-package xref
  :ensure f
  :custom (xref-search-program 'ripgrep))

;; Local personalization
(let ((file (expand-file-name (concat (user-real-login-name) ".el")
                              user-emacs-directory)))
  (when (file-exists-p file)
    (load file)))

;;; init.el ends here
