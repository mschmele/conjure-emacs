;;; init-counsel.el --- counsel config
;;; Commentary:
;;; Code:

(use-package counsel
  :diminish
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(provide 'init-counsel)
;;; init-counsel.el ends here
