;;; init-packages.el -- core packages
;;; Commentary:
;;; Code:

;; Initialize Package Sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(defvar conjure-packages
  '(ace-window
    ag
    avy
    anzu
    browse-kill-ring
    crux
    discover-my-major
    diff-hl
    diminish
    doom-themes
    easy-kill
    editorconfig
    epl
    expand-region
    flycheck
    gist
    git-timemachine
    git-modes
    guru-mode
    hl-todo
    hydra
    projectile
    magit
    move-text
    nlinum
    operate-on-number
    smartparens
    smartrep
    super-save
    undo-tree
    use-package
    volatile-highlights
    which-key
    zenburn-theme
    zop-to-char)
  "List of core packages to make sure are present.")

(defun conjure-packages-installed-p ()
  "Check if core packages are installed."
  (cl-every #'package-installed-p conjure-packages))

(defun conjure-require-package (package)
  "Ensure PACKAGE is installed."
  (unless (memq package conjure-packages)
    (add-to-list 'conjure-packages package))
  (unless (package-installed-p package)
    (package-install package)))

(defun conjure-require-packages (packages)
  "Ensure PACKAGES are installed."
  (mapc #'conjure-require-package packages))

(defun conjure-install-packages ()
  "Install all of the core packages."
  (unless (conjure-packages-installed-p)
    (message "%s" "Conjure is updating the package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (conjure-require-packages conjure-packages)))

(conjure-install-packages)

(require 'use-package)
(setq use-package-always-ensure t)

(provide 'init-packages)
;;; init-packages.el ends here
