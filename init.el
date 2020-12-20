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

;; (use-package simple-modeline
;;   :hook (after-init . simple-modeline-mode))

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
  :diminish
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

(use-package python-mode
  :ensure nil
  :custom
  (python-shell-interpreter "python3"))

(use-package dockerfile-mode)
(use-package docker-compose-mode)

(require 'init-exec-path)
(require 'init-behaviors)
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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(fira-code-mode markdown-mode org-bullets doom-modeline doom-themes flycheck-pos-tip whitespace-cleanup-mode which-key use-package uniquify-files smex simple-modeline ruby-hash-syntax rspec-mode rg rainbow-delimiters python-mode paredit magit ivy-rich hydra helpful gitignore-mode git-timemachine git-messenger git-gutter fullframe flycheck-color-mode-line flycheck-clojure flx exec-path-from-shell elein dockerfile-mode docker-compose-mode diminish counsel-projectile company cljsbuild-mode beacon all-the-icons ace-window)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;; init.el ends here
