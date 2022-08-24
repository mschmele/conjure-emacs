;;; init-lisp.el -- Lisp languages setup
;;; Commentary:
;;; Code:
(require 'init-programming)
(conjure-require-packages '(rainbow-delimiters))

(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(defun conjure-lisp-coding-defaults ()
  "Defaults for `lisp-mode' editing."
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq conjure-lisp-coding-hook 'conjure-lisp-coding-defaults)

(defun conjure-interactive-lisp-coding-defaults ()
  "Defaults for interactive Lisp (REPL) buffers."
  (message "[conjure] For a more... civilized age")

  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq conjure-interactive-lisp-coding-hook 'conjure-interactive-lisp-coding-defaults)

(add-hook 'lisp-data-mode-hook (lambda ()
                                 (run-hooks 'conjure-lisp-coding-hook)))

(provide 'init-lisp)
;;; init-lisp.el ends here
