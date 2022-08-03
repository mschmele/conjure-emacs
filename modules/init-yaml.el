;;; init-yaml.el --- yaml-mode config
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode
                         yaml-mode))

(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda () (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)))

(add-hook 'yaml-mode-hook #'lsp)

(provide 'init-yaml)
;;; init-yaml.el ends here
