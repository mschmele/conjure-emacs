;;; init.el --- Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(setq gc-cons-threshold (* 100 1024 1024)
      gc-cons-percentage 0.6)

;; Setup directory for splitting out individual configurations
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(require 'dired)
(when *is-a-mac*
  (setq dired-use-ls-dired nil))

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

(require 'time)
(setq inhibit-startup-message t
      display-time-24hr-format t
      display-time-use-mail-icon t
      visible-bell t
      use-dialog-box nil)

(setq-default electric-indent-inhibit t
              cursor-type 'box)

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
  (load-theme 'doom-one t))

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
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/"))
      gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

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
  :demand
  :config
  (dolist (var '("JAVA_HOME" "SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "SNYK_TOKEN"))
    (add-to-list 'exec-path-from-shell-variables var)))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(use-package diminish
  :demand)

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
  :demand
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  :init
  (load-theme 'doom-ayu-mirage t)) 

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

(use-package hl-line
  :hook (prog-mode . hl-line-mode))

(setq python-shell-interpreter "python3")

(use-package highlight-indentation
  :hook ((python-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-mode)))

(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))

(use-package lorem-ipsum)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package uuidgen)

(use-package winum
  :init
  (winum-mode))

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

(require 'init-ivy)

(use-package projectile
  :diminish
  :bind (("C-c p" . #'projectile-command-map))
  :init
  (projectile-mode +1)
  :config
  (setq projectile-completion-system 'ivy
        projectile-switch-project-action #'projectile-dired))

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
(use-package scala-mode)
(use-package pug-mode)

(use-package go-mode
  :config
  (setq tab-width 4
        indent-tabs-mode 1))

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

(setq auto-mode-alist
      (append '(("\\.native\\'" . nxml-mode)
                ("\\.echo10\\'" . nxml-mode)
                ("\\.dif\\'" . nxml-mode)
                ("\\.dif10\\'" . nxml-mode)
                ;; ISO may need pre-processing to not open in so-long-mode
                ;; ("\\.iso\\'" . nxml-mode)
                ;; ("\\.iso19115\\'" . nxml-mode)
                ("\\.umm_json\\'" . js-mode))
              auto-mode-alist))

(use-package elfeed)
(use-package fontaine)

;;; init.el ends here
