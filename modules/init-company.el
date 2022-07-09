;;; init-company.el --- company-mode setup
;;; Commentary:
;;; Code:
(conjure-require-packages '(company))

(require 'company)
(require 'diminish)

(setq company-idle-delay 0.2
      company-tooltip-limit 12
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above t)

(global-company-mode 1)
(diminish 'company-mode)

(provide 'init-company)
;;; init-company.el ends here
