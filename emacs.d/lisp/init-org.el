;;; Org-mode
;;; Commentary:

;;; Code:
(require 'org)
(require 'ox-md)

(setq org-directory "~/org"
      org-log-done t
      org-startup-indented t
      org-startup-folded t
      org-agenda-files (list (concat org-directory "/todo.org")
                             (concat org-directory "/dev.org")
                             (concat org-directory "/todo.org_archive"))
      org-default-notes-file (concat org-directory "/todo.org")
      org-src-fontify-natively t
      org-outline-path-complete-in-steps nil
      org-refile-use-outline-path t
      org-use-fast-todo-selection t)

;; Ensure that we can refile to headings a few levels down. This
;; should help with organizing.
(setq org-refile-targets '((nil . (:maxlevel . 4))))

;; Use the current window to open the agenda
(setq-default org-agenda-window-setup 'current-window)

(defun jcs-agenda ()
  "Open TODO list and Org agenda side-by-side."
  (interactive)
  (delete-other-windows)
  (find-file (concat org-directory "/todo.org"))
  (split-window-horizontally)
  (other-window 1)
  (org-agenda-list))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cm" 'jcs-agenda)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)

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

(defun reset-weekly-summary ()
  (interactive)
  (find-file (concat org-directory "/weekly-summary.org"))
  (erase-buffer)
  (insert
   "#+OPTIONS: toc:nil
* jcsims
** Did
** Doing
** Blockers
** Questions
"))

;; Add a few languages for execution in org source blocks
(org-babel-do-load-languages 'org-babel-load-languages
                             '((clojure . t)
                               (sh . t)
                               (emacs-lisp . t)))

;; Add capture templates for questions to ask
(setq org-capture-templates
      '(("s" "Templates for the Weekly Summary")
        ("sd" "Summary: Did" item (file+olp (concat org-directory "/weekly-summary.org")
                                            "jcsims" "Did"))
        ("so" "Summary: Doing" item (file+olp (concat org-directory "/weekly-summary.org")
                                              "jcsims" "Doing"))
        ("sb" "Summary: Blockers" item (file+olp (concat org-directory "/weekly-summary.org")
                                                 "jcsims" "Blockers"))
        ("sq" "Summary: Quesions" item (file+olp (concat org-directory "/weekly-summary.org")
                                                 "jcsims" "Questions"))
        ("d" "Templates for docs")
        ("dm" "Docs: Misc" entry (file+olp (concat org-directory "/dev.org")
                                          "Misc"))
        ("de" "Docs: Emacs" entry (file+olp (concat org-directory "/dev.org")
                                           "Emacs"))
        ("dg" "Docs: Git" entry (file+olp (concat org-directory "/dev.org")
                                         "Git"))
        ("dt" "Docs: ThreatGRID" entry (file+olp (concat org-directory "/dev.org")
                                                "TG"))
        ("dc" "Docs: Clojure" entry (file+olp (concat org-directory "/dev.org")
                                                "Clojure"))
        ("q" "Alex Question" entry (file (concat org-directory "/questions.org")
                                         "Questions"))
        ("t" "Templates for TODOs")
        ("ta" "TODO: Admin" entry (file+olp (concat org-directory "/todo.org")
                                            "Admin"))
        ("ti" "TODO: Issues" entry (file+olp (concat org-directory "/todo.org")
                                            "Issues"))
        ("tt" "TODO: Tasks" entry (file+olp (concat org-directory "/todo.org")
                                            "Tasks"))))

(provide 'init-org)
;;; init-org.el ends here
