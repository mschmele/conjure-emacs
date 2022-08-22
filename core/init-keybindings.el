;;; init-keybindings.el -- global keybindings
;;; Commentary:
;;; Code:
(require 'init-common)

(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") 'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; toggle menu-bar visibility
(global-set-key (kbd "<f12>") 'menu-bar-mode)

(global-set-key (kbd "C-c g") 'magit-file-dispatch)

(global-set-key (kbd "M-z") 'zop-up-to-char)
(global-set-key (kbd "M-Z") 'zop-to-char)

(global-set-key (kbd "s-w") 'ace-window)
(global-set-key [remap other-window] 'ace-window)

(global-set-key (kbd "C-c h p") #'pulsar-pulse-line)
(global-set-key (kbd "C-c h h") #'pulsar-highlight-line)

(provide 'init-keybindings)
;;; init-keybindings.el ends here
