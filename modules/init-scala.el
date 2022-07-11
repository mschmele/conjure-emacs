;;; init-scala.el --- scala-mode configuration
;;; Commentary:
;;; Code:

(require 'init-programming)
(conjure-require-packages '(scala-mode
                            lsp-metals
                            lsp-mode))

(defun conjure-scala-mode-common-defaults ()
  "Setup defaults for `scala-mode-common' in Conjure."
  (subword-mode +1)
  (lsp))

(setq conjure-scala-mode-common-hook 'conjure-scala-mode-common-defaults)

(add-hook 'scala-mode-hook (lambda ()
                                (run-hooks 'conjure-scala-mode-common-hook)))

(provide 'init-scala)
;;; init-scala.el ends here
