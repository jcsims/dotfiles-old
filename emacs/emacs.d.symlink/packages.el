(defvar my-packages '(clojure-mode
                      clojure-test-mode
                      nrepl
                      markdown-mode
                      yaml-mode
                      auctex
                      magit
                      expand-region
                      auto-complete
                      ac-nrepl
                      ac-slime
                      ac-math
                      smex
                      idle-highlight-mode
                      ido-ubiquitous
                      find-file-in-project
                      paredit
                      rainbow-delimiters
                      yasnippet
                      expand-region
                      el-autoyas
                      base16-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
