;;; init-yaml.el --- yaml-mode config
;;; Commentary:
;;; Code:
(conjure-require-packages '(yaml-mode))

(add-hook 'yaml-mode-hook 'whitespace-mode)
(add-hook 'yaml-mode-hook 'subword-mode)
(add-hook 'yaml-mode-hook
          (lambda () (add-hook 'before-save-hook 'conjure-cleanup-maybe nil t)))


(provide 'init-yaml)
;;; init-yaml.el ends here
