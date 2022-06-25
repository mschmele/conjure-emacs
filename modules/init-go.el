;;; init-go.el -- go-mode initialization
;;; Commentary:
;;; Code:
(require 'init-programming)

(use-package go-mode
  :config
  (setq tab-width 4
        indent-tabs-mode 1))

(provide 'init-go)
;;; init-go.el ends here
