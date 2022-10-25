;;; init.el --- Conjure Emacs Initialization File
;;; Commentary:
;;; Code:
(defconst emacs-start-time (current-time))
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-linux* (eq system-type 'gnu/linux))

(setq gc-cons-threshold (* 50 1024 1024)
      gc-cons-percentage 0.6)

(defvar conjure-user
  (getenv (if (equal system-type 'windows-nt) "USERNAME" "USER")))

(defvar conjure-dir (file-name-directory load-file-name)
  "The root dir of the Emacs Conjure distribution.")

(defvar conjure-core-dir (expand-file-name "core" conjure-dir)
  "The home of Conjure's core functionality.")

(defvar conjure-modules-dir (expand-file-name  "modules" conjure-dir)
  "This directory houses all of the built-in Prelude modules.")

(defvar conjure-savefile-dir (expand-file-name "savefile" user-emacs-directory)
  "Folder for storing generated history files.")

(defvar conjure-org-dir (expand-file-name "org" (file-truename "~"))
  "Folder for storing org notes.")

(defvar conjure-org-roam-dir (expand-file-name "roam" conjure-org-dir)
  "Folder for storing org-roam notes.")

(unless (file-exists-p conjure-savefile-dir)
  (make-directory conjure-savefile-dir))

(defun conjure-add-subfolders-to-load-path (parent-dir)
  "Add all level PARENT-DIR subdirs to the `load-path'."
  (dolist (f (directory-files parent-dir))
    (let ((name (expand-file-name f parent-dir)))
      (when (and (file-directory-p name)
                 (not (string-prefix-p "." f)))
        (add-to-list 'load-path name)
        (conjure-add-subfolders-to-load-path name)))))

(message "[conjure] Emacs is coming online...")

(setq load-prefer-newer t)

;; Setup directories for splitting out individual configurations
(add-to-list 'load-path conjure-core-dir)
(add-to-list 'load-path conjure-modules-dir)

;; Save customization variables to a separate file
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

(message "[conjure] Invoking the Deep Magic...")
;; vvvvvv Do not change order vvvvvv
(require 'init-packages)
(require 'init-custom)
(require 'init-ui)
(require 'init-common)
(require 'conjure-mode)
(require 'init-editor)
(require 'init-keybindings)
;; ^^^^^ Do not change order ^^^^^

(when *is-a-mac*
  (require 'init-macos))

(when *is-linux*
  (require 'init-linux))

(message "[conjure] Configuring packages...")
;; Enable or disable as needed
(require 'init-c)
(require 'init-clojure)
(require 'init-company)
(require 'init-emacs-lisp)
(require 'init-go)
(require 'init-graphql)
(require 'init-ivy)
(require 'init-java)
(require 'init-js)
(require 'init-org)
(require 'init-python)
(require 'init-ruby)
(require 'init-rust)
(require 'init-scala)
(require 'init-terraform)
(require 'init-web)
(require 'init-xml)
(require 'init-yaml)

;; One-offs that don't have their own setups yet
(conjure-require-packages '(darkroom
                            dashboard
                            elfeed
                            lorem-ipsum
                            restclient
                            uuidgen))

(require 'darkroom)
(define-key global-map [f12] 'darkroom-mode)

(require 'dashboard)
(setq dashboard-center-content t
      dashboard-banner-logo-title "Conjure Emacs"
      dashboard-startup-banner (expand-file-name  "images/witch_hat.png" conjure-core-dir)
      dashboard-items '((recents . 5)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)))
(dashboard-setup-startup-hook)

;; TODO this should be determined by the system
(setq datetime-timezone 'US/Eastern)

(require 'server)
(unless (server-running-p) (server-start))
;;; init.el ends here
