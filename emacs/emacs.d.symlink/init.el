;; Package Management

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Load other files
(load "~/.emacs.d/funcs.el")
(load "~/.emacs.d/init-smartparens.el")
(load "~/.emacs.d/init-auctex.el")

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
(setq yas/root-directory '("~/.emacs.d/snippets"))
(mapc 'yas/load-directory yas/root-directory)

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


;; Fill mode is pretty handy
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode 'turn-on-auto-fill)
(add-hook 'org-mode 'turn-on-auto-fill)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/org/todo.org"
                             "~/org/thesis.org"
                             "~/org/ecsig.org"
                             "~/org/school.org"))
