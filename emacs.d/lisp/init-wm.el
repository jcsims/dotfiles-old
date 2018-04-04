;;; init-wm.el --- Initialize the Emacs X Window Manager

;;; Commentary:

;;; Code:

(require 'use-package)
(require 'validate)

(use-package exwm
  :disabled
  :config
  (require 'exwm-systemtray)
  (validate-setq display-time-day-and-date t
                 exwm-systemtray-height 14)
  (display-time-mode)
  (exwm-systemtray-enable)

  (require 'exwm-config)
  (exwm-config-default)

  (dolist (k '(("s-p" "(playerctl pause || true) && i3lock --image=\"$HOME/wallpapers/cat.png\"")
               ("s-S-p" "(playerctl pause || true) && ${HOME}/bin/lock && systemctl suspend")
               ("s-c" "clipmenu -b -i")
               ("s-m" "passmenu -b -i")
               ("s-<return>" "termite")
               ("s-a" "maim -s | xclip -selection clipboard -t image/png")
               ("s-s" "maim -s ${HOME}/pictures/screenshots/screenshot-$(date -I'seconds').png")))
    (let ((f (lambda () (interactive)
               (save-window-excursion
                 (start-process-shell-command (cadr k) nil (cadr k))))))
      (exwm-input-set-key (kbd (car k)) f)))

  (exwm-input-set-simulation-keys
   (mapcar (lambda (c) (cons (kbd (car c)) (cdr c)))
	   `(("C-b" . left)
	     ("C-f" . right)
	     ("C-p" . up)
	     ("C-n" . down)
	     ("C-a" . home)
	     ("C-e" . end)
	     ("M-v" . prior)
	     ("C-v" . next)
	     ("C-d" . delete)
	     ("C-m" . return)
	     ("C-i" . tab)
	     ("C-g" . escape)
	     ("C-s" . ?\C-f)
	     ("C-y" . ?\C-v)
	     ("M-w" . ?\C-c)
	     ("M-<" . C-home)
	     ("M->" . C-end)
	     ("C-M-h" . C-backspace)))))

(provide 'init-wm)
;;; init-wm.el ends here
