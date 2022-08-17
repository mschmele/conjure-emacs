;;; init-programming.el -- core programming setup
;;; Commentary:
;;; Code:
(conjure-require-packages '(highlight-numbers
                            yasnippet
                            yasnippet-snippets))

(defun conjure-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(require 'hl-todo)
(global-hl-todo-mode 1)

(require 'which-func)
(which-function-mode 1)

;; smart curly braces
(sp-pair "{" nil :post-handlers
         '(((lambda (&rest _ignored)
              (crux-smart-open-line-above)) "RET")))

;; Warn when using commands that aren't emacsy enough
(setq guru-warn-only t)

(defun conjure-prog-mode-defaults ()
  "Default coding hook."
  (when conjure-guru
    (guru-mode +1)
    (diminish 'guru-mode))

  (highlight-numbers-mode +1)
  (smartparens-mode +1)
  (conjure-enable-whitespace)
  (conjure-local-comment-auto-fill))

(setq conjure-prog-mode-hook 'conjure-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda () (run-hooks 'conjure-prog-mode-hook)))
(add-hook 'prog-mode-hook (lambda () (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)))

(setq-default flycheck-emacs-lisp-load-path 'inherit)
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(require 'yasnippet)
(yas-global-mode 1)
(diminish 'yas-minor-mode)

(with-eval-after-load 'subword (diminish 'subword-mode))

(message "[conjure] Preparing Unicorn Sparkles...")

(provide 'init-programming)
;;; init-programming.el ends here
