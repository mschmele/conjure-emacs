;;; init-ts.el --- typescript mode configuration
;;; Commentary:
;;; Code:

(require 'init-programming)
(conjure-require-packages '(tide))

(require 'typescript-mode)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

(with-eval-after-load 'typescript-mode
  (defun conjure-ts-mode-defaults ()
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifer-mode +1))

  (add-hook 'before-save-hook
            (lambda ()
              (when conjure-format-on-save
                (tide-format-before-save))))

  (setq conjure-ts-mode-hook 'conjure-ts-mode-defaults)

  (add-hook 'typescript-mode-hook
            (lambda ()
              (run-hooks 'conjure-js-mode-hook))))

(provide 'init-ts)
;;; init-ts.el ends here
