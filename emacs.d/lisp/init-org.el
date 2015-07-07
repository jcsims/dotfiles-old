;;; Org-mode
;;; Commentary:

;;; Code:
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-directory "~/org")
(setq org-log-done t)
(setq org-startup-indented t)
(setq org-startup-folded t)

(setq org-agenda-files (list (concat org-directory "/todo.org")
                             (concat org-directory "/todo.org_archive")))
(setq org-default-notes-file
      (concat org-directory "/todo.org"))

;; Taken from the org-mode manual - Automatically mark a parent task
;; as DONE when all child nodes are marked DONE
(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

(provide 'init-org)
;;; init-org.el ends here
