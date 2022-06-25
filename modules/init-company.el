;;; init-company.el --- Company config
;;; Commentary:
;;; Code:
(conjure-require-packages '(company
                            company-box))

(require 'company)
(require 'diminish)

(setq company-idle-delay 0.5)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-tooltip-flip-when-above t)

(global-company-mode 1)
(diminish 'company-mode)

(require 'company-box)
(company-box-mode 1)
(diminish 'company-box-mode)

(provide 'init-company)
;;; init-company.el ends here
