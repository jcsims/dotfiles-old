;;; Clojure --- Summary
;;; Commentary:
;; Heavily borrowed from
;; https://martintrojer.github.io/clojure/2015/02/14/clojure-and-emacs-without-cider-redux/
;; and https://github.com/oneness/rcfiles/blob/master/emacs/conf/clojure.el
;;; Code:

(require 'inf-clojure)
(require 'clojure-mode)
(add-hook 'clojure-mode-hook 'inf-clojure-minor-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'inf-clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'inf-clojure-mode-hook 'paredit-mode)

(defun find-tag-without-ns (next-p)
  (interactive "P")
  (find-tag (first (last (split-string (symbol-name (symbol-at-point)) "/"))) next-p))

(defun reload-current-clj-ns (next-p)
  (interactive "P")
  (let ((ns (clojure-find-ns)))
    (message (format "Loading %s ..." ns))
    (inf-clojure-eval-string (format "(require '%s :reload)" ns))
    (when (not next-p) (inf-clojure-eval-string (format "(in-ns '%s)" ns)))))

;; So that we can clear the whole repl buffer
(setq inf-clojure-prompt-read-only nil)
(defun erase-inf-buffer ()
  (interactive)
  (with-current-buffer (get-buffer "*inf-clojure*")
    (erase-buffer))
  (inf-clojure-eval-string ""))

(defun clj-run-tests-ns ()
  (interactive)
  (let ((ns (clojure-find-ns)))
    (message (format "Running tests in ns %s ..." ns))
    (inf-clojure-eval-string "(run-tests)")
    (message (format "Finished running all tests in %s" ns))))

(defun clj-run-test-at-point ()
  (interactive)
  (let ((ns (clojure-find-ns))
	(def (second (clojure-find-def))))
    (message (format "Running test %s" def))
    (inf-clojure-eval-string (format "(test-vars [#'%s/%s])" ns def))
    (message (format "Finished running test %s" def))))

(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map (kbd "C-c C-k") 'reload-current-clj-ns)
             (define-key clojure-mode-map (kbd "C-c ,") 'clj-run-tests-ns)
	     (define-key clojure-mode-map (kbd "C-c M-,") 'clj-run-test-at-point)
             (define-key clojure-mode-map (kbd "M-.") 'find-tag-without-ns)
             (define-key clojure-mode-map (kbd "M-,") 'pop-tag-mark)
             (define-key clojure-mode-map (kbd "C-c l") 'erase-inf-buffer)
             (define-key clojure-mode-map (kbd "C-c C-t") 'clojure-toggle-keyword-string)))
(add-hook 'inf-clojure-mode-hook
          '(lambda ()
             (define-key inf-clojure-mode-map (kbd "C-c l") 'erase-inf-buffer)))


(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c r")))
(setq-default cljr-suppress-middleware-warnings t)


(require 'company)
(require 'company-etags)
(add-to-list 'company-etags-modes 'clojure-mode)

(require 'align-cljlet)
(add-hook 'clojure-mode-hook
          '(lambda ()
             (define-key clojure-mode-map "\C-c\C-y" 'align-cljlet)))

;; Add some goodies from Emacs Live
(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "λ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

;; Set up proper indentation for a few compojure functions
(eval-after-load 'clojure-mode
  '(define-clojure-indent
     (defroutes 'defun)
     (GET 2)
     (POST 2)
     (PUT 2)
     (DELETE 2)
     (HEAD 2)
     (ANY 2)
     (context 2)))

(provide 'init-clojure)
;;; init-clojure.el ends here
