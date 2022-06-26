;;; init-company.el --- company-mode setup
;;; Commentary:
;;; Code:
(conjure-require-packages '(company
                            company-box))

(require 'company)
(require 'diminish)

(setq company-idle-delay 0.5
      company-tooltip-limit 10
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above t)

(global-company-mode 1)
(diminish 'company-mode)

(require 'company-box)
(with-eval-after-load 'company-box
  (defun conjure-company-box-defaults ()
    (company-box-mode 1)
    (diminish 'company-box-mode))

  (setq conjure-company-box-hook 'conjure-company-box-defaults)
  (add-hook 'company-mode-hook
            (lambda ()
              (run-hooks 'conjure-company-box-hook))))

(provide 'init-company)
;;; init-company.el ends here
