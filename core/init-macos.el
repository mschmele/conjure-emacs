;;; init-macos.el -- macos specific functions
;;; Commentary:
;;; Code:
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setq ns-function-modifier 'hyper
      dired-use-ls-dired nil)

(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

;; don't hide menu-bar for mac
(menu-bar-mode +1)

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

(provide 'init-macos)
;; init-macos.el ends here
