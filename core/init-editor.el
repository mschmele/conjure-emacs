;;; init-editor.el -- editor configuration
;;; Commentary:
;;; Code:
(setq-default indent-tabs-mode nil) ;; no tabs
(setq-default tab-width 8)          ;; fake it with tabs

(setq require-final-newline t)

(customize-set-variable 'global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)

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

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)

(require 'diminish)

(require 'uniquify)
(setq uniqify-buffer-name-style 'forward)
(setq uniqify-separator "/")
(setq uniqify-after-kill-buffer-p t) ; rename after killing unique
(setq uniquify-ignore-buffers-re "^\\*") ; ignore special buffers

(setq save-place-file (expand-file-name "saveplace" conjure-savefile-dir))
(save-place-mode 1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" conjure-savefile-dir))

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
      bookmark-save-flag 1)

(require 'ivy)
(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
      projectile-completion-system 'ivy
      projectile-switch-project-action #'projectile-dired)
(define-key (current-global-map) (kbd "C-c p") 'projectile-command-map)
(projectile-mode t)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

(customize-set-variable 'kill-do-not-save-duplicates t)

(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(defun conjure-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `conjure-clean-whitespace-on-save' is not nil."
  (when conjure-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun conjure-enable-whitespace ()
  "Enable `whitespace-mode' if `conjure-whitespace' is not nil."
  (when conjure-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)
    (whitespace-mode +1)))

(defun conjure-enable-flyspell ()
  "Enable command 'flyspell-mode' if 'conjure-flyspell' is not nil."
  (when (and conjure-flyspell (executable-find ispell-program-name))
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'conjure-enable-flyspell)

(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(provide 'init-editor)
;;; init-editor.el
