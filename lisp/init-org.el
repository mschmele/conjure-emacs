;;; init-org.el -- Org Mode Initialization
;;; Commentary:
;;; Code:
(use-package org
  :hook (org-mode . (lambda ()
                      (linum-mode 0)
                      (setq truncate-lines nil)))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (ruby . t)
     (python . t)))
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(message "Loaded Org-Mode configs")
(provide 'init-org)
;;; init-org.el ends here
