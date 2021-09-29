;;; init-org.el --- org mode config
;;; Commentary:
;;; Code:

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(provide 'init-org)
;;; init-org.el ends here
