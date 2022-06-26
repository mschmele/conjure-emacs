;;; init-xml.el --- nxml-mode config
;;; Commentary:
;;; Code:

(require 'nxml-mode)

(push '("<\\?xml" . nxml-mode) magic-mode-alist)

;; treat POM files as xml
(add-to-list 'auto-mode-alist '("\\.pom\\'" . nxml-mode))

(setq nxml-child-indent 4
      nxml-attribute-indent 4
      nxml-auto-insert-xml-declaration-flag nil
      nxml-bind-meta-tab-to-complete-flag t
      nxml-slash-auto-complete-flag t)

(provide 'init-xml)
;;; init-xml.el ends here
