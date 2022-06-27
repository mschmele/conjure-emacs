;;; init-ruby.el --- ruby-mode config
;;; Commentary:
;;; Code:
(require 'init-programming)

(conjure-require-packages '(inf-ruby
                            lsp-mode
                            yari))

;; ignore rubinius bytecode
(add-to-list 'completion-ignored-extensions ".rbc")

(define-key 'help-command (kbd "R") 'yari)

(with-eval-after-load 'ruby-mode
  (defun conjure-ruby-mode-defaults ()
    (setq ruby-insert-encoding-magic-comment nil)
    (inf-ruby-minor-mode +1)
    (subword-mode +1))

  (setq conjure-ruby-mode-hook 'conjure-ruby-mode-defaults)

  (add-hook 'ruby-mode-hook (lambda ()
                              (run-hooks 'conjure-ruby-mode-hok)))
  (add-hook 'ruby-mode-hook #'lsp-deferred))

(provide 'init-ruby)
;;; init-ruby.el ends here
