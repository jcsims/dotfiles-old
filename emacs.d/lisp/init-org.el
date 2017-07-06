;;; Org-mode
;;; Commentary:

;;; Code:
(require 'org)
(require 'org-agenda)
(require 'ox-md)

(setq-default org-directory "~/org"
              org-log-done t
              org-startup-indented t
              org-startup-folded t
              org-agenda-file-regexp "tg-.*\\.org\.*"
              org-agenda-files (list org-directory)
              org-default-notes-file (concat org-directory "/tg-inbox.org")
              org-src-fontify-natively t
              org-use-fast-todo-selection t
              refile-targets (mapcar (lambda (org-file)
                                      (concat org-directory "/" org-file))
                                    '("tg-dev.org" "tg-ops.org" "tg-design.org"))
              org-refile-targets '((refile-targets . (:level . 0)))
              org-refile-allow-creating-parent-nodes t
              org-refile-use-outline-path 'file
              org-completion-use-ido t)

;; Use the current window to open the agenda
(setq-default org-agenda-window-setup 'current-window)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

(setq org-agenda-custom-commands
      '(("c" "Agenda and all action items"
         ((agenda "")
          (todo "WAITING")
          (todo "NEXT|TODO"
                ((org-agenda-sorting-strategy '(todo-state-down))))))
        ("h" todo "HOLD")))

(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "DOING(o)" "|" "DONE(d)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; Taken from the org-mode manual - Automatically mark a parent task
;; as DONE when all child nodes are marked DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; Add a few languages for execution in org source blocks
(org-babel-do-load-languages 'org-babel-load-languages
                             '((clojure . t)
                               (sh . t)
                               (emacs-lisp . t)
                               (restclient . t)))

;; Don't ask every time before evaluating an org source block
(setq org-confirm-babel-evaluate nil)

(provide 'init-org)
;;; init-org.el ends here
