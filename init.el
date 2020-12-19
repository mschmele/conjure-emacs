;;; init.el --- basic emacs initialization
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(set-fringe-mode 10)

(setq visible-bell t)

;; Install Fira Code to the system first
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

(load-theme 'tango-dark)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize Package Sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var)))

;;;; UI Elements
(column-number-mode)
(global-display-line-numbers-mode t)
;; Disable line numbers for certain modes
(dolist (mode '(eshell-mode-hook
		org-mode-hook
		shell-mode-hook
		term-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package all-the-icons)

(use-package diminish)

(require 'init-ivy)

(require 'init-counsel)

;; Adds M-x recent command sorting for counsel-M-x
(use-package smex
  :defer 1
  :after counsel)

(use-package simple-modeline
  :hook (after-init . simple-modeline-mode))

(use-package paredit
  :hook (prog-mode . paredit-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-varaible-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-increase "out")
  ("f" nil "finished" :exit t))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package gitignore-mode)
(use-package git-timemachine)
(use-package git-messenger)
(use-package git-gutter
  :hook (after-init . global-git-gutter-mode))

(use-package fullframe)

(use-package clojure-mode)
(use-package cljsbuild-mode)
(use-package elein)

(use-package cider)

(use-package flycheck-clojure
  :init (flycheck-clojure-setup))

(use-package ruby-mode)
(use-package ruby-hash-syntax)
(use-package rspec-mode)

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package python-mode)

(require 'init-osx-keys)
(require 'init-exec-path)
(require 'init-behaviors)
(require 'init-flycheck)
;;(require 'init-uniquify)
(require 'init-projectile)

(use-package uniquify-files)

(use-package company
  :diminish (company-mode)
  :hook (after-init . global-company-mode))

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package ace-window
  :bind (("M-p" . 'ace-window)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package whitespace-cleanup-mode
  :hook (after-init . whitespace-cleanup-mode))

(use-package rg)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(rg whitespace-cleanup-mode git-messenger fullframe git-timemachine gitignore-mode python-mode uniquify-files exec-path-from-shell syntax-subword flycheck-clojure eldoc-mode subword-mode cider magit flycheck-color-mode-line flycheck counsel-projectile clojure-mode hydra projectile helpful ivy-rich which-key simple-modeline rainbow-delimiters paredit flx smex counsel ivy diminish command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
