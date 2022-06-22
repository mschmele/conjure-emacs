;;; init.el --- Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(setq gc-cons-threshold (* 50 1024 1024)
      gc-cons-percentage 0.6)

(defvar conjure-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar conjure-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Folder for storing generated history files")

(message "[conjure] Emacs is coming online...")

(setq load-prefer-newer t)

;; Setup directories for splitting out individual configurations
(add-to-list 'load-path (expand-file-name "core" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Do not change order
(require 'init-packages)
(require 'init-custom)
(require 'init-ui)
(require 'init-common)
(require 'init-editor)
(require 'init-keybindings)

(when *is-a-mac*
  (require 'init-macos))

(when *is-linux*
  (require 'init-linux))

;; Modules
(require 'init-ivy)
(require 'init-emacs-lisp)
(require 'init-clojure)

;; Make shbang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(use-package paradox
  :init (paradox-enable))

(use-package exec-path-from-shell
  :config
  (dolist (var '("JAVA_HOME"))
    (add-to-list 'exec-path-from-shell-variables var)))

(use-package pulsar
  :demand
  :bind (("C-x l" . pulsar-highlight-dwim))
  :init
  (pulsar-global-mode 1)
  :config
  (setq pulsar-pulse-on-window-change t
        pulsar-face 'pulsar-magenta
        pulsar-highlight-face 'pulsar-yellow))

;; better light theme
(use-package modus-themes
  :demand
  :config
  (setq modus-themes-mode-line '(accented borderless padded)
        modus-themes-region  '(bg-only)
        modus-themes-completions '((matches . (extrabold background))
                                   (selection . (semibold accented))
                                   (popup . (extrabold)))))

;; Useful dark themes
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
  :config
  (setq-default flycheck-emacs-lisp-load-path 'inherit))

;; (use-package ibuffer
;;   :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc)

(use-package dired-quick-sort)

;; (use-package hl-line
;;   :hook (prog-mode . hl-line-mode))

(setq python-shell-interpreter "python3")

(use-package highlight-indentation
  :hook ((python-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-mode)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode) . rainbow-delimiters-mode))

(use-package uuidgen)

(use-package all-the-icons
  :demand)

(use-package counsel
  :diminish
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-c b" . counsel-bookmark)
         ("C-c d" . counsel-descbinds)
         ("C-c g" . counsel-git)
         ("C-c o" . counsel-outline)
         ("C-c t" . counsel-load-theme)
         ("C-c F" . counsel-org-file)
         ("C-c J" . counsel-file-jump)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))



(use-package amx
  :after ivy
  :config
  (setq amx-backend 'auto
        amx-save-file (expand-file-name "amx-items" user-emacs-directory)
        amx-history-length 50
        amx-show-key-bindings nil)
  :init
  (amx-mode 1))

(use-package which-key
  :diminish
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.3))

(use-package ace-window)

(use-package super-save
  :diminish
  :config
  (add-to-list 'super-save-triggers 'ace-window)
  (super-save-mode +1))

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
  :init
  (global-company-mode t)
  :config
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package company-box
  :diminish
  :hook (company-mode . company-box-mode))

(use-package company-statistics
  :init
  (company-statistics-mode 1))

(use-package yasnippet
  :diminish yas-minor-mode
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

(use-package elixir-mode)
(use-package haskell-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((clojure-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (sgml-mode . lsp-deferred)
         ;; (python-mode . lsp-deferred)
         (java-mode . lsp-deferred)
         (elixir-mode . lsp-deferred)
         (terraform-mode . lsp-deferred)
         (c-mode-common . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-java)
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package whitespace-cleanup-mode
  :config
  (whitespace-cleanup-mode t))

(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :requires all-the-icons
  :init (all-the-icons-ibuffer-mode 1))

(require 'init-git)
(require 'init-java)
(require 'init-clojure)
(require 'init-org)
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i" . completion-at-point)))

(use-package org-roam-ui
  :after org-roam
  ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
  ;;         a hookable mode anymore, you're advised to pick something yourself
  ;;         if you don't care about startup time, use
  ;;  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package darkroom)
(use-package terraform-mode)
(use-package dockerfile-mode)
(use-package markdown-mode)
(use-package feature-mode)
(use-package yaml-mode)
(use-package ansible)
(use-package scala-mode)
(use-package pug-mode)

(use-package vue-mode)
(use-package async)

(use-package rust-mode
  :config
  (setq indent-tabs-mode nil))

(use-package dashboard
  :demand
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t))

(use-package elfeed)
(use-package fontaine)
(use-package lorem-ipsum)

(require 'init-go)

;;; init.el ends here
