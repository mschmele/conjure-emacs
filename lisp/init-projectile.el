;;; init-projectile.el --- projectile configuration
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind (("C-c p" . projectile-command-map))
  :init
  (when (file-directory-p "~/workspace")
    (setq projectile-project-search-path '("~/workspace/" "~/workspace/cmr")))
  (setq projectile-switch-project-action #'projectile-dired))

(provide 'init-projectile)
;;; init-projectile.el ends here
