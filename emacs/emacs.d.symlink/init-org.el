;;; Org-mode
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

(setq org-directory "~/org")
(setq org-log-done t)
(setq org-startup-indented t)
(setq org-startup-folded t)

(setq org-agenda-files (list "~/org/todo.org"
                             "~/org/thesis.org"
                             "~/org/ecsig.org"
                             "~/org/school.org"))
(setq org-default-notes-file
      (concat org-directory "/todo.org"))

;; Mobile Org setup
(setq org-mobile-inbox-for-pull "~/org/todo.org")
(setq org-mobile-directory "~/mobileorg")

(provide 'init-org)
;;; init-org.el ends here