;;; init-custom.el --- custom properties
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

(defcustom conjure-super-keybindings t
  "Non-nil values enable super-key."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-guru t
  "Non-nil values enable 'guru-mode'."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-whitespace nil
  "Non-nil values enable whitespace visualization."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-clean-whitespace-on-save t
  "Cleanup whitespace from file before it's saved.
Will only occur if `conjure-whitespace' is also enabled."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-format-on-save t
  "When set and supported in the current mode, the file will be formatted on save."
  :type 'boolean
  :group 'conjure)

(defcustom conjure-theme 'zenburn
  "The default theme of Conjure."
  :type 'symbol
  :group 'conjure)

(provide 'init-custom)
;;; init-custom.el ends here
