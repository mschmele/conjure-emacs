;;; init-lisp.el -- lisp languages setup
;;; Commentary:
;;; Code:
(require 'init-programming)
(conjure-require-packages '(rainbow-delimiters))

(define-key read-expression-map (kbd "TAB") 'completion-at-point)

(defun conjure-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1))

(setq conjure-lisp-coding-hook 'conjure-lisp-coding-defaults)

(defun conjure-interactive-lisp-coding-defaults ()
  (smartparens-strict-mode +1)
  (rainbow-delimiters-mode +1)
  (whitespace-mode -1))

(setq conjure-interactive-lisp-coding-hook 'conjure-interactive-lisp-coding-defaults)

(provide 'init-lisp)
;;; init-programming.el ends here
