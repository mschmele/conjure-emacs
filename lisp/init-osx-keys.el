;;; init-osx-keys.el --- osx configs
;;; Commentary:
;;; Code:
(when *is-a-mac*
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control)))))

(provide 'init-osx-keys)
;;; init-osx-keys.el ends here
