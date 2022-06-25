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

(defvar conjure-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Conjure distribution.")

(defvar conjure-core-dir (expand-file-name "core" conjure-dir)
  "The home of Conjure's core functionality.")

(defvar conjure-modules-dir (expand-file-name  "modules" conjure-dir)
  "This directory houses all of the built-in Prelude modules.")

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
(require 'conjure-mode)
(require 'init-editor)
(require 'init-keybindings)

(when *is-a-mac*
  (require 'init-macos))

(when *is-linux*
  (require 'init-linux))

;; Modules
;; Enable or disable
(require 'init-ivy)
(require 'init-company)
(require 'init-emacs-lisp)
(require 'init-clojure)
(require 'init-js)
(require 'init-ts)
(require 'init-org)
;; (require 'init-go)

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

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc)

(use-package dired-quick-sort)

(setq python-shell-interpreter "python3")

(use-package highlight-indentation
  :hook ((python-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-mode)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package uuidgen)

(use-package all-the-icons)

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

(use-package yasnippet
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)


(use-package elixir-mode)
(use-package haskell-mode)

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook ((clojure-mode . lsp-deferred)
;;          (ruby-mode . lsp-deferred)
;;          (go-mode . lsp-deferred)
;;          (js-mode . lsp-deferred)
;;          (sgml-mode . lsp-deferred)
;;          ;; (python-mode . lsp-deferred)
;;          (java-mode . lsp-deferred)
;;          (elixir-mode . lsp-deferred)
;;          (terraform-mode . lsp-deferred)
;;          (c-mode-common . lsp-deferred)
;;          (lsp-mode . lsp-enable-which-key-integration))
;;   :init
;;   (setq lsp-keymap-prefix "C-c l"))

;; (use-package lsp-java)
;; (use-package lsp-ivy
;;   :commands lsp-ivy-workspace-symbol)

;; (use-package dap-mode
;;   :after lsp-mode
;;   :config (dap-auto-configure-mode))

(use-package whitespace-cleanup-mode
  :config
  (whitespace-cleanup-mode t))

(use-package all-the-icons-dired
  :requires all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package all-the-icons-ibuffer
  :requires all-the-icons
  :init (all-the-icons-ibuffer-mode 1))


(require 'init-java)


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
