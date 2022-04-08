;;; init.el --- Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(setq inhibit-startup-message t
      display-time-24hr-format t
      display-time-use-mail-icon t
      visible-bell t
      use-dialog-box nil)

(setq-default electric-indent-inhibit t)

(scroll-bar-mode -1)   ; Disable visible scroll-bar
(tool-bar-mode -1)     ; Disable the toolbar
(tooltip-mode -1)      ; Disable tooltips
(set-fringe-mode 10)   ; Make space on the left

(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)
(setq-default indent-tabs-mode nil)
(fset 'yes-or-no-p 'y-or-n-p)
(column-number-mode)
(global-display-line-numbers-mode t)

(when *is-a-mac*
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(customize-set-variable 'kill-do-not-save-duplicates t)

;; Make scrolling less stuttered
(setq auto-window-vscroll nil)
(customize-set-variable 'fast-but-imprecise-scrolling t)
(customize-set-variable 'scroll-conservatively 101)
(customize-set-variable 'scroll-margin 0)
(customize-set-variable 'scroll-preserve-screen-position t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; Make shbang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "<f5>") 'revert-buffer)

(set-face-attribute 'default nil :font "MesloLGS NF" :height 140)

(defun light ()
  "Set a light theme."
  (interactive)
  (load-theme 'doom-one-light t))

(defun dark ()
  "Set a dark theme."
  (interactive)
  (load-theme 'zenburn))

(defun disable-active-themes ()
  "Disable themes before switching."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Update theme loading."
  (disable-active-themes))

;; Initialize Package Sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package paradox
  :init (paradox-enable))

(use-package exec-path-from-shell
  :config
  (dolist (var '("JAVA_HOME" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package diminish)

(use-package hydra)
(require 'hydra)

(global-set-key
 (kbd "<f2>")
 (defhydra hydra-text-scale (:timeout 4)
   "scale text"
   ("j" text-scale-increase "in")
   ("k" text-scale-decrease "out")
   ("=" (text-scale-set 0) "reset")
   ("q" nil "quit" :exit t)))

;; See what commands were run
(use-package command-log-mode)

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package rg
  :hook (after-init . rg-enable-default-bindings))

(use-package flycheck
  :init
  (setq flycheck-highlighting-mode 'symbols)
  :custom
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc)

(setq python-shell-interpreter "python3")

(use-package aggressive-indent
  :diminish
  :defer t
  :hook ((emacs-lisp-mode . aggressive-indent-mode)
         (scheme-mode . aggressive-indent-mode)
         (lisp-mode . aggressive-indent-mode)
         (clojure-mode . aggressive-indent-mode)))

(use-package auto-compile)
(use-package auto-highlight-symbol
  :commands (global-auto-highlight-symbol-mode auto-highlight-symbol-mode)
  :bind (("C-x a h" . auto-highlight-symbol-mode)
         :map
         auto-highlight-symbol-mode-map)
  :config
  (add-to-list 'ahs-modes 'clojure-mode))

(use-package dired-quick-sort)

(use-package drag-stuff)

(use-package editorconfig)

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(use-package highlight-indentation
  :hook ((python-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-mode)))

(use-package highlight-numbers
  :diminish
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :diminish
  :hook (prog-mode . hl-todo-mode))

(use-package lorem-ipsum)

(use-package rainbow-delimiters
  :diminish
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package uuidgen)

(use-package undo-tree
  :diminish
  :config
  (setq undo-tree-auto-save-history t
        undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo-tree" user-emacs-directory))))
  :init
  (global-undo-tree-mode 1))

(use-package winum
  :diminish
  :init
  (winum-mode))

(use-package all-the-icons)
(require 'init-ivy)

(use-package projectile
  :diminish
  :bind (("C-c p" . #'projectile-command-map))
  :config
  (projectile-mode +1)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-project-search-path '("~/github" "~/workspace")))

(use-package flx
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)
  (counsel-mode 1))

(use-package which-key
  :diminish
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.3))

(use-package paredit
  :diminish
  :hook ((lisp-mode . paredit-mode)
	 (clojure-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))



(use-package helpful
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package company
  :diminish
  :bind ("C-;" . company-complete-common)
  :config
  (global-company-mode t)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :hook (company-mode . company-statistics-mode))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(setq-default js-indent-level 2)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((ruby-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (typescript-mode . lsp-deferred))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-java)
(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package whitespace-cleanup-mode
  :diminish
  :config
  (whitespace-cleanup-mode t))

(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :requires all-the-icons
  :init (all-the-icons-ibuffer-mode 1))

(require 'init-git)
(require 'init-clojure)
(require 'init-org)

(use-package darkroom)
(use-package terraform-mode)
(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package feature-mode)
(use-package yaml-mode)
(use-package scala-mode)
(use-package go-mode
  :hook (before-save-hook . 'gofmt-before-save)
  :config
  (setq tab-width 4
        indent-tabs-mode 1))
(use-package eglot)
(use-package vue-mode)
;;; init.el ends here
