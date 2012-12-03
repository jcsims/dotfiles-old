;; package management
;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; Ensure that certain packages are installed at launch,
;; and the package archive is updated
;; Pulled from github: technomancy/emacs-starter-kit
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit 
                      starter-kit-lisp 
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-eshell
                      starter-kit-js
                      clojure-mode
                      clojure-test-mode
                      haskell-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Make sure that the server is started when opening emacs
(server-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "71b172ea4aad108801421cc5251edb6c792f3adbaecfa1c52e94e3d99634dee7" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "71efabb175ea1cf5c9768f10dad62bb2606f41d110152f4ace675325d28df8bd" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" default)))
 '(line-number-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Colors are awesome
(load-theme 'sanityinc-tomorrow-eighties t)

;; General editing
;;;;;;;;;;;;;;;;;;
;; When going to the next line at the end of a file, insert newlines
(setq next-line-add-newlines t)


;; Org mode changes
;;;;;;;;;;;;;;;;;;;

;; Record time when a TODO is marked completed
(setq org-log-done 'time)

;; Set up the proper directory for use with MobileOrg
(setq org-mobile-directory "~/Dropbox/org/mobile")

;; The agenda view will pull info from these files
(setq org-agenda-files (quote ("~/Dropbox/org/today.org"  "~/Dropbox/org/todo.org")))

;; Global shortcuts
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; TODO states - the vertical seperates those that are considered
;; "Done"
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "PHONE")))

;; Haskell mode settings
;;;;;;;;;;;;;;;;;;;;;;;;
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indententation)

;; Load ghc-mod
(autoload 'ghc-init "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init) (flymake-mode)))

