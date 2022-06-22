;;; init-programming.el -- core programming setup
;;; Commentary:
;;; Code:

(require 'hl-todo)
(global-hl-todo-mode 1)

(require 'which-func)
(which-function-mode 1)

(require 'smartparens)
(smartparens-mode +1)

(message "[conjure] preparing code space...")

(provide 'init-programming)
;;; init-programming.el ends here
