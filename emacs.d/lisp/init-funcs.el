;;; init-funcs.el --- Collection of functions added

;;; Commentary:

;;; Code:
;; Originally taken from:
;; http://emacsredux.com/blog/2013/05/22/smarter-navigation-to-the-beginning-of-a-line/
(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))
  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; Taken from the Emacs Wiki: http://www.emacswiki.org/emacs/InsertDate
(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO
  format."
  (interactive "P")
  (let ((format (cond
                 ((not prefix) "%a %d %b %Y")
                 ((equal prefix '(4)) "%Y-%m-%d"))))
    (insert (format-time-string format))))

(global-set-key (kbd "C-c d") 'insert-date)

;; Taken from http://whattheemacsd.com/editing-defuns.el-01.html
(defun open-line-below ()
  "Anywhere on the line, open a new line below current line."
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  "Anywhere on the line, open a new line above current line."
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)

;; A few taken from bodil
(defun sudo-edit ()
  "Edit current buffer using sudo."
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

;; Set transparency of current frame
(defun transparency (value)
  "Set the transparency of the frame window.  VALUE: 0=transparent/100=opaque."
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))

;; Define a nice multi-purpose commenting command
;; Taken from http://endlessparentheses.com/implementing-comment-line.html
(defun endless/comment-line-or-region (n)
  "Comment or uncomment current line and leave point after it.
With positive prefix, apply to N lines including current one.
With negative prefix, apply to -N lines above.
If region is active, apply to active region instead."
  (interactive "p")
  (if (use-region-p)
      (comment-or-uncomment-region
       (region-beginning) (region-end))
    (let ((range
           (list (line-beginning-position)
                 (goto-char (line-end-position n)))))
      (comment-or-uncomment-region
       (apply #'min range)
       (apply #'max range)))
    (forward-line 1)
    (back-to-indentation)))

(global-set-key (kbd "C-c ;") 'endless/comment-line-or-region)

;; Clear buffer in eshell
(defun eshell-clear-buffer ()
  "Clear eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

(add-hook 'eshell-mode-hook
	  '(lambda()
             (local-set-key (kbd "C-l") 'eshell-clear-buffer)))

(defun dired-do-ispell (&optional arg)
  "Check all marked files ARG with ispell.  Borrowed from the
Emacswiki."
  (interactive "P")
  (dolist (file (dired-get-marked-files
                 nil arg
                 #'(lambda (f)
                     (not (file-directory-p f)))))
    (save-window-excursion
      (with-current-buffer (find-file file)
        (ispell-buffer)))
    (message nil)))

;; Some functions carried over from the emacs starter kit
(defun esk-local-comment-auto-fill ()
  "Only auto-fill in comment strings, in prog-mode-derived buffers."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-pretty-lambdas ()
  "Make the `lambda` keyword a pretty one."
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  "Highlight common `TODO`-like words in the buffer, so they stand out."
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)


(use-package magit-git :ensure magit)
(use-package git-commit :ensure magit)
(defun jcs/magit-commit-template (&rest _)
  "Ensure that commits on an issue- branch have the issue name in the commit as well."
  (let ((prefix (magit-get-current-branch)))
    (if (string-prefix-p "issue-" prefix)
        (progn
          (goto-char (point-min))
          (if (not (search-forward prefix (line-end-position) t))
              (progn
                (goto-char (point-min))
                (insert prefix ": ")
                (insert "\n")
                (goto-char (point-min))
                (move-end-of-line nil))
            (goto-char (point-min)))))))

(add-hook 'git-commit-mode-hook 'jcs/magit-commit-template)

(defun urldecode ()
  "Call `url-unhex-string` on the active region."
  (interactive)
  (if (not (use-region-p))
      (message "`urldecode` only works with an active region!")
    (let ((unhexed (url-unhex-string
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))))
      (kill-new unhexed)
      (message "%s" unhexed))))

(defun urlencode ()
  "Call `url-hexify-string` on the active region."
  (interactive)
  (if (not (use-region-p))
      (message "`urlencode` only works with an active region!")
    (let ((hexed (url-hexify-string
                  (buffer-substring-no-properties
                   (region-beginning) (region-end)))))
      (kill-new hexed)
      (message "%s" hexed))))

(defun random-lowercase-char ()
  "Return a random lowercase character, from a-z."
  (format "%c" (+ 97 (random 26))))

;; Taken from technomancy's emacs.d
(global-set-key (kbd "C-c n")
                (defun pnh-cleanup-buffer ()
                  (interactive)
                  (delete-trailing-whitespace)
                  (untabify (point-min) (point-max))
                  (indent-region (point-min) (point-max))))

(defvar jcs/tab-sensitive-modes '(makefile-bsdmake-mode))
(defvar jcs/indent-sensitive-modes '(conf-mode
                                     coffee-mode
                                     haml-mode
                                     python-mode
                                     slim-mode
                                     yaml-mode))

;; Slightly  modified from crux's version
(defun cleanup-buffer ()
  "Cleanup the buffer, including whitespace and indentation."
  (interactive)
  (unless (member major-mode jcs/tab-sensitive-modes)
    (untabify (point-min) (point-max)))
  (unless (member major-mode jcs/indent-sensitive-modes)
    (indent-region (point-min) (point-max)))
  (whitespace-cleanup))

(provide 'init-funcs)
;;; init-funcs.el ends here
