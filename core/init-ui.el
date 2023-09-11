;;; init-ui.el --- ui settings
;;; Commentary:
;;; Code:

(conjure-require-packages '(all-the-icons
                            all-the-icons-dired
                            all-the-icons-ibuffer
                            pulsar))

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(scroll-bar-mode -1)   ; Disable visible scroll-bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(blink-cursor-mode -1) ; no blinky cursor

(setq-default cursor-type 'box)

(column-number-mode)
(size-indication-mode)
(global-display-line-numbers-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t ; disable startup
      visible-bell t
      use-dialog-box nil
      ring-bell-function 'ignore)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; window positioning
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist `((left . 80)
                                  (top . 50)
                                  (height . 50)
                                  (width . 240)))

      (setq default-frame-alist `((left . 80)
                                  (top . 50)
                                  (height . 50)
                                  (width . 240)))))

(when conjure-theme
  (load-theme conjure-theme t))

(add-hook 'dired-mode-hook (lambda ()
                             (all-the-icons-dired-mode)
                             (diminish 'all-the-icons-dired-mode)))

(add-hook 'ibuffer-mode-hook (lambda ()
                               (all-the-icons-ibuffer-mode)))

(require 'pulsar)
(setq pulsar-pulse t
      pulsar-delay 0.055
      pulsar-iterations 10)
(add-hook 'next-error-hook #'pulsar-pulse-line)

(when (fboundp 'ace-window)
  ;; pulsar doesn't detect the override because of ordering so we have to set it ourselves
  (setq pulsar-pulse-functions (add-to-list 'pulsar-pulse-functions 'ace-window)))

(pulsar-global-mode 1)

;; improve `hl-line' highlighting
(require 'lin)
(lin-global-mode 1)

(set-face-attribute 'default nil :font "Iosevka" :height 130)
(setq goto-address-url-face 'link
      goto-address-url-mouse-face 'highlight
      goto-address-mail-face 'link
      goto-address-mail-mouse-face 'highlight)

(provide 'init-ui)
;;; init-ui.el ends here
