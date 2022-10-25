;;; init-terraform.el --- terraform-mode configuration
;;; Commentary:
;;; Code:
(require 'init-programming)
(conjure-require-packages '(lsp-mode
                            terraform-mode))

(require 'terraform-mode)
(add-hook 'terraform-mode-hook #'lsp-deferred)

(provide 'init-terraform)
;;; init-terraform.el ends here
