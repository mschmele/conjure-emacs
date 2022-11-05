;;; init-js.el --- javascript mode configuration
;;; Commentary:
;;; Code:
(require 'init-programming)
(conjure-require-packages '(prettier-js
                            rjsx-mode
                            typescript-mode
                            lsp-mode
                            dap-mode
                            tide
                            web-mode))

(add-to-list 'auto-mode-alist '("\\.[mc]?js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'" . rjsx-mode))

(setq js-chain-indent t
      js2-basic-offset 2
      js2-skip-preprocessor-directives t
      ;; flycheck handles these
      js2-mode-show-parse-errors nil
      js2-mode-show-strict-warnings nil
      js2-strict-missing-semi-warning nil
      js2-highlight-level 3
      js2-idle-timer-delay 0.15)

(defun conjure-js-mode-defaults ()
  "Configure sensible defaults for JS development."
  (rainbow-delimiters-mode +1))

(setq conjure-js-mode-hook 'conjure-js-mode-defaults)

(add-hook 'rjsx-mode-hook
          (lambda()
            (run-hooks 'conjure-js-mode-hook)))
(add-hook 'rjsx-mode-hook #'lsp-deferred)

(require 'typescript-mode)
(setq typescript-indent-level 2)

(require 'dap-node)
(dap-node-setup)

(require 'dap-firefox)
(dap-firefox-setup)

(require 'dap-chrome)
(dap-chrome-setup)

(provide 'init-js)
;;; init-js.el ends here
