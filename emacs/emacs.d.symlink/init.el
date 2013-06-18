;; Package Management
;; Make sure that all the packages I use are installed
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-eshell
                      clojure-mode
                      clojure-test-mode
                      nrepl
                      auctex
                      magit))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Ensure that on OSX, $PATH in Emacs matches that in the shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; Aesthetics
(load-theme 'monokai t)

;; Enable whitespace mode for progamming languages, and highlight when
;; lines are over 80 characters long
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Clojure mode
(add-hook 'clojure-mode-hook 'paredit-mode)

;; Save backups to a central location
;; Taken from http://whattheemacsd.com/init.el-02.html
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)
