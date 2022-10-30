;;; init-editor.el --- editor configuration
;;; Commentary:
;;; Code:
(require 'diminish)

(setq-default indent-tabs-mode nil) ;; no tabs
(setq-default tab-width 8)          ;; fake it with tabs

(setq require-final-newline t)
(setq use-dialog-box nil)

(setq global-auto-revert-non-file-buffers t)
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
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; smart tab behavior
(setq tab-always-indent 'complete)

;; smart pairs
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(diminish 'smartparens-mode)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing unique
(setq uniquify-ignore-buffers-re "^\\*") ; ignore special buffers

(require 'saveplace)
(setq save-place-file (expand-file-name "saveplace" conjure-savefile-dir))
(save-place-mode 1)

(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" conjure-savefile-dir)
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      recentf-auto-cleanup 'never)

(defun confjure-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (cl-some (lambda (dir)
               (string-prefix-p dir file-dir))
             (mapcar 'file-truename (list conjure-savefile-dir)))))

(add-to-list 'recentf-exclude 'conjure-recentf-exclude-p)
(recentf-mode +1)

(require 'super-save)
(add-to-list 'super-save-triggers 'ace-window)
(super-save-mode +1)
(diminish 'super-save-mode)

(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" conjure-savefile-dir)
      bookmark-save-flag 1)

(require 'projectile)
(setq projectile-cache-file (expand-file-name "projectile.cache" conjure-savefile-dir)
      projectile-create-missing-test-files t
      projectile-require-project-root t)
(projectile-mode t)

(global-hl-line-mode +1)

(require 'volatile-highlights)
(volatile-highlights-mode t)
(diminish 'volatile-highlights-mode)

;; Better support for files with long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq-default bidi-inhibit-bpa t)
(global-so-long-mode 1)

;; jump to char
(require 'avy)
(setq avy-background t)
(setq avy-style 'at-full)
(global-set-key (kbd "C-:") 'avy-goto-char)

;; better find and replace
(require 'anzu)
(diminish 'anzu-mode)
(global-anzu-mode)

(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

(put 'dired-find-alternate-file 'disabled nil)

(require 'dired-x)

(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies'always)
(setq dired-dwim-target t)

(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(require 'midnight)

(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)
(global-set-key (kbd "s-y") 'browse-kill-ring)

(customize-set-variable 'kill-do-not-save-duplicates t)

(require 'flyspell)
(setq ispell-program-name "aspell"
      ispell-extra-args '("--sug-mode=ultra"))

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" conjure-savefile-dir))

(defun conjure-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `conjure-clean-whitespace-on-save' is not nil."
  (when conjure-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun conjure-enable-whitespace ()
  "Enable `whitespace-mode' if `conjure-whitespace' is not nil."
  (when conjure-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (setq whitespace-line-column 100)
    (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)
    (whitespace-mode +1)))

(defun conjure-enable-flyspell ()
  "Enable command 'flyspell-mode' if 'conjure-flyspell' is not nil."
  (when (and conjure-flyspell
             (executable-find ispell-program-name))
    (flyspell-mode +1)))

(add-hook 'text-mode-hook 'conjure-enable-flyspell)

(global-diff-hl-mode +1)
(add-hook 'dired-mode-hook 'diff-hl-dired-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)

;; Conjure very own mode
(conjure-mode +1)

(require 'undo-tree)
;; autosave the undo-tree history
(setq undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq undo-tree-auto-save-history t)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

(winner-mode +1)

(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; Make shbang (#!) file executable when saved
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

(require 'which-key)
(which-key-mode 1)
(diminish 'which-key-mode)

;; handle colorized output in compiled windows (e.g. `dap-mode' output)
(add-hook 'compilation-filter-hook
          (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

(require 'prescient)
(setq-default history-length 1000)
(setq-default prescient-history-length 1000)
(setq prescient-save-file (expand-file-name "prescient-save.el" conjure-savefile-dir))
(prescient-persist-mode +1)

(provide 'init-editor)
;;; init-editor.el ends here
