;;; init-git.el -- Git Packages Initialization
;;; Commentary:
;;; Code:
(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode))

(use-package forge)

(message "Loaded Git configs")
(provide 'init-git)
;;; init-git.el ends here
