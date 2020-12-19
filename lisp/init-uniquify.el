;;; init-uniquify.el --- nicely name identical files
;;; Commentary:
;;; Code:
(use-package uniquify
  :custom
  ((setq uniquify-buffer-name-style 'reverse)
   (setq uniquify-separator " • ")
   (setq uniquify-after-kill-buffer-p t)
   (setq uniquify-ignore-buffers-re "^\\*")))

(provide 'init-uniquify)
;;; init-uniquify.el ends here
