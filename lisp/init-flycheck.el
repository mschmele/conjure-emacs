;;; init-flycheck.el --- base flycheck configs
;;; Commentary:
;;; Code:
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package flycheck-color-mode-line
  :after flycheck)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
