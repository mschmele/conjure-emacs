;;; init-git.el -- Git Packages Initialization
;;; Commentary:
;;; Code:
(use-package magit
  :defer t)

(use-package git-gutter
  :diminish
  :hook (prog-mode . git-gutter-mode))

(message "Loaded Git configs")
(provide 'init-git)
;;; init-git.el ends here
