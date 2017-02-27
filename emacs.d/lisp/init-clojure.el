;;; Clojure --- Summary
;;; Commentary:
;;; Code:

(rename-modeline "clojure-mode" clojure-mode "Clj")

(require 'clojure-mode)
;;; Cider
;; Don't prompt for a symbol with `M-.`
(setq-default cider-prompt-for-symbol nil
              cljr-favor-prefix-notation nil
              cider-repl-display-help-banner nil)
(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'clojure-mode-hook 'cider-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-repl-mode-hook #'eldoc-mode)

(defun tdd-test ()
  "Thin wrapper around `cider-test-run-project-tests', borrowed from
  http://endlessparentheses.com/test-driven-development-in-cider-and-emacs.html"
  (when (cider-connected-p)
    (cider-test-run-project-tests)))

(define-minor-mode tdd-mode
  "Run all Clojure tests whenever a file is saved"
  nil " TDD" nil
  (if tdd-mode
      (add-hook 'after-save-hook #'tdd-test nil 'local)
    (remove-hook 'after-save-hook #'tdd-test 'local)))

(require 'clj-refactor)
(add-hook 'clojure-mode-hook (lambda ()
                               (clj-refactor-mode 1)
                               (yas-minor-mode 1)
                               (cljr-add-keybindings-with-prefix "C-c r")))


(setq nrepl-hide-special-buffers t)

;; Add some goodies from Emacs Live
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

(require 'clj-refactor)
(setq-default cljr-suppress-middleware-warnings t)

;; Lazily build ASTs, instead of immediately on REPL connect
(setq cljr-warn-on-eval t)
(setq cljr-eagerly-build-asts-on-startup nil)

(add-to-list 'auto-mode-alist '("\\.clj.*\\'" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.edn.*\\'" . clojure-mode))

;; Try out a linter...
(require 'flycheck-joker)
(provide 'init-clojure)
;;; init-clojure.el ends here
