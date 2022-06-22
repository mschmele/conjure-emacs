;;; init-keybindings.el -- global keybindings
;;; Commentary:
;;; Code:
(require 'init-common)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer)

(require 'hydra)
(global-set-key
 (kbd "<f2>")
 (defhydra hydra-text-scale (:timeout 4)
   "scale text"
   ("j" text-scale-increase "in")
   ("k" text-scale-decrease "out")
   ("=" (text-scale-set 0) "reset")
   ("q" nil "quit" :exit t)))

(provide 'init-keybindings)
;;; init-keybindings.el ends here
