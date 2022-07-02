;;; init-lsp.el --- lsp-mode config
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode))

(require 'lsp-mode)

(setq lsp-lens-enable nil)

(define-key lsp-mode-map (kbd "C-c C-l r") 'lsp-rename)
(define-key lsp-mode-map (kbd "C-c C-l x") 'lsp-workspace-restart)
(define-key lsp-mode-map (kbd "C-c C-l d") 'lsp-describe-thing-at-point)
(define-key lsp-mode-map (kbd "C-c C-l e") 'lsp-execute-code-action)

(provide 'init-lsp)
;;; init-lsp.el ends here
