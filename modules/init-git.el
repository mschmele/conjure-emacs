;;; init-git.el -- Git Packages Initialization
;;; Commentary:
;;; Code:
(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(require 'diff-hl)
(global-diff-hl-mode)

(provide 'init-git)
;;; init-git.el ends here
