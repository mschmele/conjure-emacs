;;; init.el --- basic emacs initialization
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))

;; Set where your projects lives here
(defconst workspace-dir "~/workspace/")

(setq inhibit-startup-message t)

(setq backup-directory-alist `(("" . "~/.emacs.d/backup")))
(setq default-directory "~/workspace/")

;; Basic UI tweaks
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(set-fringe-mode 10)

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)

(if (display-graphic-p)
    (progn
      (setq initial-frame-alist `((left . 80)
				                          (top . 50)
				                          (height . 55)
				                          (width . 140)))

      (setq default-frame-alist `((left . 80)
				                          (top . 50)
				                          (height . 55)
				                          (width . 140))))
  (progn
    (setq initial-frame-alist `((tool-bar-lines . 0)))
    (setq default-frame-alist `((tool-bar-lines . 0)))))

(defun disable-active-themes ()
  "Disable themes before switching."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  "Update theme loading."
  (disable-active-themes))

;; Install MesloLGS to the system first
(set-face-attribute 'default nil :font "MesloLGS NF" :height 140)

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

(use-package doom-themes
  :init (load-theme 'doom-one t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(defun light ()
  "Set a light theme."
  (interactive)
  (load-theme 'doom-one-light t))

(defun dark ()
  "Set a dark theme."
  (interactive)
  (load-theme 'doom-one))

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

(require 'init-exec-path)
(require 'init-ivy)
(require 'init-counsel)

(use-package which-key
  :init (which-key-mode)
  :diminish
  :config
  (setq which-key-idle-delay 0.8))

(use-package helpful
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-varaible-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package transpose-frame)

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

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "Manage Window"
   ("h" windmove-left)
   ("j" windmove-down)
   ("k" windmove-up)
   ("l" windmove-right)
   ("v" (lambda ()
	        (interactive)
	        (split-window-right)
	        (windmove-right))
    "vert")
   ("x" (lambda ()
	        (interactive)
	        (split-window-below)
	        (windmove-down))
    "horz")
   ("t" transpose-frame "'")
   ("o" delete-other-windows "one" :color blue)
   ("a" ace-window "ace")
   ("s" ace-swap-window "swap")
   ("d" ace-delete-window "del")
   ("i" ace-delete-other-windows "ace-one" :color blue)
   ("b" counsel-switch-buffer "buf")
   ("q" nil "cancel" :color blue)))

(use-package ivy-hydra
  :after (ivy hydra))

(use-package paredit
  :hook ((cider-mode . paredit-mode)
	       (prog-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package git-gutter
  :config (global-git-gutter-mode t))

(use-package fullframe)
(fullframe magit-status magit-mode-quit-window nil)

(use-package cider
  :defer t
  :config
  (setq cider-repl-history-file ".cider-repl-history"
	      nrepl-log-messages t)
  (flycheck-clojure-setup))

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(setq-default flycheck-emacs-lisp-load-path 'inherit)

(use-package flycheck-pos-tip
  :after flycheck)

(require 'flycheck-pos-tip)

(use-package flycheck-color-mode-line
  :after flycheck)

;;; Languages and Syntax Highlighting
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(setq python-shell-interpreter "python3")

(use-package lsp-mode
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(require 'init-exec-path)
(require 'init-behaviors)
(require 'init-org)
(require 'init-projectile)

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode 1))

(use-package uniquify-files)

(use-package company
  :diminish
  :defer t
  :config (global-company-mode))

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package ace-window
  :bind (("M-p" . 'ace-window)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :after ibuffer
  :hook ((ibuffer-mode . hl-line-mode)))

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode)
  :custom
  (add-hook 'before-save-hook #'whitespace-cleanup))

(use-package aggressive-indent
  :hook ((clojure-mode lisp-mode emacs-lisp-mode) . aggressive-indent-mode))

(use-package cljsbuild-mode)
(use-package clojure-mode)
(use-package company-terraform)
(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package elixir-mode)
(use-package erlang)
(use-package feature-mode)
(use-package gitignore-mode)
(use-package git-timemachine)
(use-package haskell-mode)
(use-package magit)
(use-package markdown-mode)
(use-package pug-mode)
(use-package rg)
(use-package rspec-mode)
(use-package ruby-hash-syntax)
(use-package ruby-mode)
(use-package terraform-mode)
(use-package typescript-mode)

(require 'js)
(setq js-indent-level 2)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" default))
 '(exwm-floating-border-color "#c8c8c8")
 '(fci-rule-color "#383a42")
 '(highlight-tail-colors
   ((("#e9f1e8" "#50a14f" "green")
     . 0)
    (("#e1eef3" "#0184bc" "brightcyan")
     . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(pug-mode typescript-mode all-the-icons-ivy whitespace-cleanup-mode which-key use-package uniquify-files transpose-frame smex ruby-hash-syntax rspec-mode rg rainbow-delimiters paredit org-bullets magit lsp-ui lsp-java kaocha-runner ivy-hydra ibuffer-vc helpful haskell-mode gitignore-mode git-timemachine git-messenger git-gutter fullframe flycheck-pos-tip flycheck-color-mode-line flycheck-clojure flx fira-code-mode feature-mode exec-path-from-shell erlang elpy elixir-mode doom-themes doom-modeline dockerfile-mode docker-compose-mode diminish counsel-projectile company-terraform company-box cljsbuild-mode beacon all-the-icons-ivy-rich aggressive-indent))
 '(pdf-view-midnight-colors (cons "#383a42" "#fafafa"))
 '(rustic-ansi-faces
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(vc-annotate-background "#fafafa")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50a14f")
    (cons 40 "#688e35")
    (cons 60 "#807b1b")
    (cons 80 "#986801")
    (cons 100 "#ae7118")
    (cons 120 "#c37b30")
    (cons 140 "#da8548")
    (cons 160 "#c86566")
    (cons 180 "#b74585")
    (cons 200 "#a626a4")
    (cons 220 "#ba3685")
    (cons 240 "#cf4667")
    (cons 260 "#e45649")
    (cons 280 "#d2685f")
    (cons 300 "#c07b76")
    (cons 320 "#ae8d8d")
    (cons 340 "#383a42")
    (cons 360 "#383a42")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
