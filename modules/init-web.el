;;; init-web.el --- web-mode configuration
;;; Commentary:
;;; Code:
(require 'init-programming)
(conjure-require-packages '(web-mode))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; smartparens concession
(setq web-mode-enable-auto-pairing nil)

(sp-with-modes '(web-mode)
  (sp-local-pair "%" "%"
                 :unless '(sp-in-string-p)
                 :post-handlers '(((lambda (&rest _ignored)
                                     (just-one-space)
                                     (save-excursion (insert " ")))
                                   "SPC" "=" "#")))
  (sp-local-tag "%" "<% " " %>")
  (sp-local-tag "=" "<%= " " %>")
  (sp-local-tag "#" "<%# " " %>"))

(with-eval-after-load 'web-mode
  (defun conjure-web-mode-defaults ()
    "Setup defaults for `web-mode'."
    (message "[conjure] conjure-web-mode activated"))

  (setq conjure-web-mode-hook 'conjure-web-mode-defaults)

  (add-hook 'web-mode-hook (lambda ()
                             (run-hooks 'conjure-web-mode-hook))))

(provide 'init-web)
;;; init-web.el ends here
