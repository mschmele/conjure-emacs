;;; init-programming.el -- core programming setup
;;; Commentary:
;;; Code:
(defun conjure-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(require 'hl-todo)
(global-hl-todo-mode 1)

(require 'which-func)
(which-function-mode 1)

(setq guru-warn-only t)

(defun conjure-prog-mode-defaults ()
  "Default coding hook"
  (when conjure-guru
    (guru-mode +1)
    (diminish 'guru-mode))
  (smartparens-mode +1)
  (conjure-enable-whitespace)
  (conjure-local-comment-auto-fill))

(setq conjure-prog-mode-hook 'conjure-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'conjure-prog-mode-hook)))

(message "[conjure] Preparing space...")

(provide 'init-programming)
;;; init-programming.el ends here
