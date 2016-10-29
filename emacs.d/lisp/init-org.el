;;; Org-mode
;;; Commentary:

;;; Code:
(require 'org)

(setq org-directory "~/org"
      org-log-done t
      org-startup-indented t
      org-startup-folded t
      org-agenda-files (list (concat org-directory "/todo.org")
                             (concat org-directory "/todo.org_archive"))
      org-default-notes-file (concat org-directory "/todo.org"))

;; Use the current window to open the agenda
(setq-default org-agenda-window-setup 'current-window)

(defun jcs-agenda ()
    "Open TODO list and Org agenda side-by-side"
  (interactive)
  (delete-other-windows)
  (find-file (concat org-directory "/todo.org"))
  (split-window-horizontally)
  (other-window 1)
  (org-agenda-list))

(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'jcs-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Taken from the org-mode manual - Automatically mark a parent task
;; as DONE when all child nodes are marked DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(provide 'init-org)
;;; init-org.el ends here
