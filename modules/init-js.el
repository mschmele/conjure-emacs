;;; init-js.el --- javascript mode configuration
;;; Commentary:
;;; Code:

(require 'init-programming)
(conjure-require-packages '(js2-mode json-mode))

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(setq-default js-indent-level 2)

(with-eval-after-load 'js2-mode
  (defun conjure-js-mode-defaults ()
    ;; prevent electric-layout from fighting with smartparens
    (setq-local electric-layout-rules '((?\; . after)))
    (setq mode-name "JS2")
    (js2-imenu-extras-mode +1)
    (subword-mode +1))

  (setq conjure-js-mode-hook 'conjure-js-mode-defaults)

  (add-hook 'js2-mode-hook (lambda () (run-hooks 'conjure-js-mode-hook))))

(provide 'init-js)
;;; init-js.el ends here
