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
(blink-cursor-mode -1) ; no blinky

(setq-default cursor-type 'box)

(column-number-mode)
(size-indication-mode)
(global-display-line-numbers-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t ; disable startup
      display-time-24hr-format t
      display-time-use-mail-icon t
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

(setq all-the-icons-ibuffer-formats
      '((mark modified read-only locked " "
              (icon 2 2 :left :elide)
              #(" " 0 1
                (display
                 (space :align-to 8)))
              (name 30 30 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode+ 16 16 :left :elide)
              " " filename-and-process+)
        (mark " "
              (name 30 -1)
              " " filename)))
(add-hook 'ibuffer-mode-hook (lambda ()
                               (all-the-icons-ibuffer-mode)))

(require 'pulsar)
(setq pulsar-pulse t
      pulsar-delay 0.055
      pulsar-iterations 10
      pulsar-pulse-on-window-change t)
(add-hook 'next-error-hook #'pulsar-pulse-line)

(when (fboundp 'ace-window)
  ;; pulsar doesn't detect the override because of ordering so we have to set it ourselves
  (setq pulsar-pulse-functions (add-to-list 'pulsar-pulse-functions 'ace-window)))

(pulsar-global-mode 1)

;; Setup window defaults
(setq display-buffer-alist
      '(("\\*e?shell\\*"
         (display-buffer-in-side-window)
         (setq-local window-height 0.25
                     side 'bottom
                     slot 0))))

;; make the icons smaller for a more compact tree
(setq treemacs--icon-size 15)

;; improve `hl-line' highlighting
(require 'lin)
(lin-global-mode 1)

(provide 'init-ui)
;;; init-ui.el ends here
