;;; init-go.el --- go-mode initialization
;;; Commentary:
;;; Code:
(require 'init-programming)
(require 'init-lsp)

(conjure-require-packages '(go-mode
                            go-projectile
                            lsp-mode
                            company
                            gotest))
(require 'go-projectile)
(add-to-list 'completion-ignored-extensions ".test")

(define-key 'help-command (kbd "G") 'godoc)

(add-to-list 'super-save-predicates
             (lambda () (not (eq major-mode 'go-mode))))

(with-eval-after-load 'go-mode
  (defun conjure-go-mode-defaults ()
    (let ((map go-mode-map))
      (define-key map (kbd "C-c a") 'go-test-current-project)
      (define-key map (kbd "C-c m") 'go-test-current-file)
      (define-key map (kbd "C-c .") 'go-test-current-test)
      (define-key map (kbd "C-c b") 'go-run)
      (define-key map (kbd "C-h f") 'godoc-at-point))

    ;; prefer using goimports over gofmt
    (let ((goimports (executable-find "goimports")))
      (when goimports
        (setq gofmt-command goimports)))

    ;; don't highlight tabs for go
    (whitespace-toggle-options '(tabs))

    ;; camelcase awareness when editing
    (subword-mode +1))

  (if (fboundp 'yas-global-mode)
      (yas-global-mode))

  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))

  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp-deferred)

  (setq conjure-go-mode-hook 'conjure-go-mode-defaults)
  (add-hook 'go-mode-hook (lambda ()
                            (run-hooks 'conjure-go-mode-hook))))

(provide 'init-go)
;;; init-go.el ends here
