;;; init --- configuration starting point

;;; Commentary:
;; Most of what is found in these files has
;; been pulled from the dotfiles of others.
;; Take what you want, but be prepared to
;; troubleshoot yourself!

;;; Code:
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;; Package Management
(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)

;; Ensure that the PATH is set correctly
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(require 'init-funcs)
(require 'init-smartparens)
(require 'init-auctex)
(require 'init-org)

;; Always use UTF-8
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; y/n keypresses instead of typing out yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Ensure that when we go to a new line, it's indented properly
(electric-indent-mode)
;; Since electric indent is turned on, re-purpose <C-j>
(global-set-key (kbd "C-j") 'join-line)

 ;;; Aesthetics
(load-theme 'base16-eighties t)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))
;; Clean up the modeline a bit
(require 'smart-mode-line)
(if after-init-time (sml/setup)
  (add-hook 'after-init-hook 'sml/setup))
(set-frame-font "Menlo 12")

;; The audible bell is obnoxious
(setq visible-bell t)

;; Load a few other packages
(require 'init-clojure)
(require 'init-yasnippet)
(require 'init-haskell)

;; Use company mode for completion
(add-hook 'after-init-hook 'global-company-mode)
(eval-after-load 'company '(add-to-list 'company-backends 'company-cider))
(eval-after-load 'company '(add-to-list 'company-backends 'company-tern))
(eval-after-load 'company '(add-to-list 'company-backends 'company-inf-ruby))


;; Enable whitespace mode for programming languages, and highlight when
;; lines are over 80 characters long
(setq whitespace-line-column 80)
(setq whitespace-style '(face lines-tail))
(add-hook 'prog-mode-hook 'whitespace-mode)

;; Save backups to a central location
;; Taken from http://whattheemacsd.com/init.el-02.html
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Fill mode is pretty handy
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode 'turn-on-auto-fill)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; ido
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)

;; Flycheck mode
;; We shouldn't need this require, but it isn't loaded without it
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; Flyspell mode
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(projectile-global-mode)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Auto-refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired buffers, but do it quietly
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Octave mode for .m files
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

;; Quick access to a few files
(global-set-key (kbd "C-c e i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e t")
                (lambda () (interactive) (find-file "~/org/todo.org")))

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

;; poly-mode R modes
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

(add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))

(add-to-list 'auto-mode-alist '("\\.org" . poly-org-mode))

;; mu4e
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
;; SMTP Setings
(setq smtpmail-smtp-server "mail.messagingengine.com"
      smtpmail-smtp-service 465
      smtpmail-smtp-user "jcsims@f-m.fm"
      smtpmail-stream-type 'ssl)
(require 'mu4e)

(setq
 mu4e-maildir       "~/mail"
 mu4e-sent-folder   "/INBOX.Sent Items"
 mu4e-drafts-folder "/INBOX.Drafts"
 mu4e-trash-folder  "/INBOX.Trash"
 mu4e-refile-folder "/INBOX.Archive"
 mu4e-get-mail-command "true" ;; let offlineimap handle it
 message-send-mail-function 'smtpmail-send-it
 send-mail-function 'smtpmail-send-it
 mu4e-view-show-images t
 mu4e-view-prefer-html t
 mu4e-html2text-command "html2text -width 72"
 message-kill-buffer-on-exit t
 mail-user-agent 'mu4e-user-agent
 mu4e-use-fancy-chars t
 mu4e-user-mail-address-list
 '("chris@jcsi.ms" "chris@jcsims.me" "jcsims@gmail.com")
 user-mail-address "chris@jcsi.ms"
 user-full-name "Chris Sims"
 mu4e-get-mail-command "offlineimap")

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))

(defun mu4e-msgv-action-view-in-browser (msg)
      "View the body of the message MSG in a web browser."
      (interactive)
      (let ((html (mu4e-msg-field (mu4e-message-at-point t) :body-html))
            (tmpfile (format "%s/%d.html" temporary-file-directory (random))))
        (unless html (error "No html part for this message"))
        (with-temp-file tmpfile
          (insert
           "<html>"
           "<head><meta http-equiv=\"content-type\""
           "content=\"text/html;charset=UTF-8\">"
           html))
        (browse-url (concat "file://" tmpfile))))
 
    (add-to-list 'mu4e-view-actions
                 '("View in browser" . mu4e-msgv-action-view-in-browser) t)

;; For some reason, zsh files are not opened in shell mode =/
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
;;; init.el ends here