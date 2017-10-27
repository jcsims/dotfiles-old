;;; threatgrid.el --- Helper functions for working at Threatgrid

;;; Commentary:

;;; Code:

(require 'ghub+)

(defvar tg-base-url "https://github.threatbuild.com/api/v3")
(defvar tg-gh-username)

;; (ghubp-get-issues "/users/chrsims")

(defun tb-get-threatbrain-issues ()
  "Get all issues from the Threatbrain repo."
  (let ((ghub-base-url tg-base-url)
        (repo (ghub-get "/repos/threatgrid/threatbrain")))
    (ghubp-get-repos-owner-repo-issues repo)))

;; (tb-get-threatbrain-issues)

(defun tb-get-threatbrain-prs ()
  "Get all pull requests from the Threatbrain repo."
  (let ((ghub-base-url tg-base-url)
        (repo (ghub-get "/repos/threatgrid/threatbrain")))
    (ghubp-get-repos-owner-repo-pulls repo)))

;; (tb-get-threatbrain-prs)

(defun preq (issue-name)
  "Convert a particular ISSUE-NAME to a pull-request.
See: https://developer.github.com/v3/pulls/#alternative-input"
  (let ((ghub-base-url tg-base-url)
        (repo (ghub-get "/repos/threatgrid/threatbrain"))
        (issue-number (replace-regexp-in-string "^issue-" "" issue-name))
        (pr-head (concat tg-gh-username ":" issue-name)))
    (ghubp-post-repos-owner-repo-pulls repo
                                       (list
                                        (cons 'issue issue-number)
                                        (cons 'head pr-head)
                                        (cons 'base "master")))))

;; (preq "issue-5329")

(provide 'threatgrid)
;;; threatgrid.el ends here
