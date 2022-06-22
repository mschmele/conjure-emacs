;;; init-custom.el -- custom properties
;;; Commentary:
;;; Code:
(defgroup conjure nil
  "Emacs Conjure configuration."
  :prefix "conjure-"
  :group 'convenience)

(defcustom conjure-flyspell t
  "Non-nil values enable flyspell support."
  :type 'boolean
  :group 'conjure)

(provide 'init-custom)
;;; init-custom.el
