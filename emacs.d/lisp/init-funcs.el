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

(defmacro rename-modeline (package-name mode new-name)
  "Change the name of a mode on the mode-line.
In PACKAGE-NAME, change MODE from PACKAGE-NAME to NEW-NAME.
Taken from what the emacs.d."
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

;; Taken from technomancy's emacs.d
(global-set-key (kbd "C-c n")
                (defun pnh-cleanup-buffer ()
                  (interactive)
                  (delete-trailing-whitespace)
                  (untabify (point-min) (point-max))
                  (indent-region (point-min) (point-max))))

;; A few taken from bodil
(defun recompile-init ()
  "Byte-compile all your dotfiles again."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

(defun sudo-edit ()
  "Edit current buffer using sudo."
  (interactive)
  (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name)))

(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

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

;; Stolen from Reddit:
;; https://www.reddit.com/r/emacs/comments/3uu1iw/setting_and_using_emacs_in_three_columns/
(defun emc-working-split (window-count)
  "Make vertical splits for working window setup, and populate
them with appropriate buffers.  Buffers are the most recently
used from (projectile-project-buffers), falling back
to (buffer-list) when not in a project.

If optional argument WINDOW-COUNT is omitted or nil, default to
max splits of at least 90 chars wide."
  (interactive "P")
  (recentf-mode t) ; Make sure recentf mode is on - won't work without it
  (let* ((window-count (if window-count window-count (/ (frame-width) 104)))
         (show-buffers (cond
                        ((projectile-project-p)
                         (dotimes (i window-count) ;; ensure enough
                           ;; buffers open
                           (let ((num-files (length (projectile-recentf-files))))
                             (unless (>= i num-files)
                               (find-file-noselect (concat (projectile-project-root)
                                                           (nth i (projectile-recentf-files)))))))
                         (projectile-project-buffers))
                        (t
                         (remove-if 'minibufferp (buffer-list))))))
    (delete-other-windows)
    ;; split window appropriate count - make 2nd window current
    (dotimes (i (- window-count 1))
      (split-window-horizontally)
      (if (= i 0) (other-window 1)))
    (balance-windows)
    ;; set window buffer from show-buffers list
    (mapcar* 'set-window-buffer (window-list nil "no-minibuf") show-buffers)))

(global-set-key (kbd "C-x 3") 'emc-working-split)

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
(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\(ME\\)?\\|TODO\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-column-number-mode)
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
;; (add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)

(provide 'init-funcs)
;;; init-funcs.el ends here
