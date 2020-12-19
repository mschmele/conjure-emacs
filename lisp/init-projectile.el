;;; init-projectile.el --- projectile configuration
;;; Commentary:
;;; Code:
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace")))
  (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
  :after projectile-mode
  :config (counsel-projectile-mode))

(provide 'init-projectile)
;;; init-projectile.el ends here
