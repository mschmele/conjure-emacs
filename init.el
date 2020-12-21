;;; init.el --- basic emacs initialization
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(defconst *is-a-mac* (eq system-type 'darwin))

(setq inhibit-startup-message t)

(setq backup-directory-alist `(("" . "~/.emacs.d/backup")))

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode 0)

(set-fringe-mode 10)

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

;; Install Fira Code to the system first
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

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
  :init (load-theme 'doom-dracula t))

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
  (load-theme 'doom-dracula t))

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

(use-package ivy
  :diminish ivy-mode
  :bind(("C-s" . swiper)
	:map ivy-minibuffer-map
	("TAB" . ivy-alt-done)
	("C-l" . ivy-alt-done)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	:map ivy-switch-buffer-map
	("C-k" . ivy-previous-line)
	("C-l" . ivy-done)
	("C-d" . ivy-switch-buffer-kill)
	:map ivy-reverse-i-search-map
	("C-k" . ivy-previous-line)
	("C-d" . ivy-revers-i-search-kill))
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :init
  (ivy-rich-mode 1))

;; Improve fuzzy searching in Ivy
(use-package flx
  :defer t
  :init
  (setq ivy-flx-limit 10000))

(use-package counsel
  :diminish counsel-mode
  :bind (("C-M-j" . 'counsel-switch-buffer)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package smex
  :defer 1
  :after counsel)

(use-package paredit
  :hook ((cider-mode . paredit-mode)
	 (prog-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-increase "out")
  ("f" nil "finished" :exit t))

(global-set-key
 (kbd "C-M-o")
 (defhydra hydra-window ()
   "window"
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

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package gitignore-mode)
(use-package git-timemachine)
(use-package git-messenger)
(use-package git-gutter
  :diminish git-gutter-mode
  :hook (after-init . global-git-gutter-mode))

(use-package fullframe)

(use-package clojure-mode)
(use-package cljsbuild-mode)

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-repl-history-file ".cider-repl-history")
  (flycheck-clojure-setup))

(use-package flycheck-clojure
  :defer t
  :commands (flycheck-clojure-setup)
  :config
  (eval-after-load 'flycheck
    '(setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))
(setq-default flycheck-emacs-lisp-load-path 'inherit)

(use-package flycheck-pos-tip
  :ensure t
  :after flycheck)

(use-package flycheck-color-mode-line
  :after flycheck)

(use-package ruby-mode)
(use-package ruby-hash-syntax)
(use-package rspec-mode)

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(use-package feature-mode)

(require 'init-exec-path)
(require 'init-behaviors)

(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile-mode
  :config (counsel-projectile-mode))

(use-package uniquify-files)

(use-package company
  :diminish company-mode
  :hook (after-init . global-company-mode))

(use-package beacon
  :hook (after-init . beacon-mode))

(use-package ace-window
  :bind (("M-p" . 'ace-window)))

(use-package ibuffer
  :bind (("C-x C-b" . ibuffer)))

(use-package ibuffer-vc
  :after ibuffer)

(use-package whitespace-cleanup-mode
  :hook (prog-mode . whitespace-cleanup-mode)
  :custom
  (add-hook 'before-save-hook #'whitespace-cleanup))

(use-package aggressive-indent
  :hook (prog-mode . aggressive-indent-mode))

(use-package rg)

(defun ds/org-font-setup ()
  "Setup fonts for 'org-mode'."
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
			  '(("^ *\\([-]\\) "
			     (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for headings
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Fix fixed-pitch in Org
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil     :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil    :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil  :inherit 'fixed-pitch))

(defun ds/org-mode-setup()
  "Setup 'org-mode'."
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :pin org
  :hook (org-mode . ds/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")

  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-agenda-files
	'(~/workspace/organization/todo.org))

  (ds/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package markdown-mode)

(use-package fira-code-mode
  :custom (fira-code-mode-disabled-ligatures '("[]" "x" "#_"))
  :hook prog-mode)

(use-package display-fill-column-indicator-mode
  :ensure nil
  :hook prog-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#fafafa" "#e45649" "#50a14f" "#986801" "#4078f2" "#a626a4" "#0184bc" "#383a42"])
 '(custom-safe-themes
   '("c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "99ea831ca79a916f1bd789de366b639d09811501e8c092c85b2cb7d697777f93" default))
 '(fci-rule-color "#383a42")
 '(jdee-db-active-breakpoint-face-colors (cons "#f0f0f0" "#4078f2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f0f0f0" "#50a14f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f0f0f0" "#9ca0a4"))
 '(objed-cursor-color "#e45649")
 '(package-selected-packages
   '(ibuffer-vc ibuffer-projectile transpose-frame feature-mode fixture-mode ivy-hydra aggressive-indent aggressive-indent-mode fira-code-mode markdown-mode org-bullets doom-modeline doom-themes flycheck-pos-tip whitespace-cleanup-mode which-key use-package uniquify-files smex simple-modeline ruby-hash-syntax rspec-mode rg rainbow-delimiters python-mode paredit magit ivy-rich hydra helpful gitignore-mode git-timemachine git-messenger git-gutter fullframe flycheck-color-mode-line flycheck-clojure flx exec-path-from-shell elein dockerfile-mode docker-compose-mode diminish counsel-projectile company cljsbuild-mode beacon all-the-icons ace-window))
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
