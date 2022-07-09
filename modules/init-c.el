;;; init-c.el --- c-mode configuration
;;; Commentary:
;;; Code:

(require 'init-programming)

(setq-default js-indent-level 2)

(defun conjure-c-mode-common-defaults ()
  "Setup defaults for `c-mode-common' in Conjure."
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

(setq conjure-c-mode-common-hook 'conjure-c-mode-common-defaults)

(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'conjure-c-mode-common-hook)))

(defun conjure-makefile-mode-defaults ()
  "Setup defaults for `makefile-mode' in Conjure."
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t))

(setq conjure-makefile-mode-hook 'conjure-makefile-mode-defaults)
(add-hook 'makefile-mode-hook (lambda()
                                (run-hooks 'conjure-makefile-mode-hook)))

(provide 'init-c)
;;; init-c.el ends here
