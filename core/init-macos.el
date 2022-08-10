;;; init-macos.el --- macos specific functions
;;; Commentary:
;;; Code:
(conjure-require-packages '(exec-path-from-shell))

(require 'exec-path-from-shell)

(with-eval-after-load 'exec-path-from-shell
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; Allow GPG to decrypt gpg file
(setf epa-pinentry-mode 'loopback)

(setq ns-function-modifier 'hyper
      dired-use-ls-dired nil)

(setq mouse-wheel-scroll-amount '(1
                                  ((shift) . 5)
                                  ((control))))

;; don't hide menu-bar for mac
(menu-bar-mode +1)

(when (fboundp 'set-fontset-font)
  (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

;; handle nvm
(when (file-exists-p "~/.nvm")
  ;; TODO get the actual nvm executable to get current npm version
  ;; (call-process "/bin/bash" nil t nil "-c" "nvm")
  (setq exec-path (append exec-path '("~/.nvm/versions/node/v14.20.0/bin"))))

(provide 'init-macos)
;;; init-macos.el ends here
