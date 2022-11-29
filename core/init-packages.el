;;; init-packages.el --- core packages
;;; Commentary:
;;; Code:

;; Initialize Package Sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(defvar conjure-ui-packages
  '(ace-window
    ef-themes))

(defvar conjure-nav-packages
  '(avy
    imenu-anywhere
    projectile))

(defvar conjure-editor-packages
  '(anzu
    crux
    diff-hl
    easy-kill
    editorconfig
    expand-region
    helm
    hl-todo
    lin
    move-text
    nlinum
    operate-on-number
    smartparens
    volatile-highlights
    wgrep
    zop-to-char))

(defvar conjure-utility-packages
  '(ag
    browse-kill-ring
    discover-my-major
    diminish
    epl
    flycheck
    gist
    git-timemachine
    git-modes
    guru-mode
    helpful
    magit
    smartrep
    super-save
    treemacs
    undo-tree
    which-key))

(defvar conjure-packages
  (append conjure-ui-packages
          conjure-nav-packages
          conjure-editor-packages
          conjure-utility-packages)
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

(defmacro conjure-auto-install (extension package mode)
  "When file with EXTENSION is opened triggers auto-install of PACKAGE.
PACKAGE is installed only if not already present.  The file is opened in MODE."
  `(add-to-list 'auto-mode-alist
                `(,extension . (lambda ()
                                 (unless (package-installed-p ',package)
                                   (package-install ',package))
                                 (,mode)))))

(defvar conjure-auto-install-alist
  '(("\\.adoc\\'" adoc-mode adoc-mode)
    ("\\.clj\\'" clojure-mode clojure-mode)
    ("\\.cljc\\'" clojure-mode clojurec-mode)
    ("\\.cljs\\'" clojure-mode clojurescript-mode)
    ("\\.edn\\'" clojure-mode clojure-mode)
    ("\\.cmake\\'" cmake-mode cmake-mode)
    ("CMakeLists\\.txt\\'" cmake-mode cmake-mode)
    ("\\.coffee\\'" coffee-mode coffee-mode)
    ("\\.css\\'" css-mode css-mode)
    ("\\.csv\\'" csv-mode csv-mode)
    ("Cask" cask-mode cask-mode)
    ("\\.d\\'" d-mode d-mode)
    ("\\.dart\\'" dart-mode dart-mode)
    ("\\.elm\\'" elm-mode elm-mode)
    ("\\.ex\\'" elixir-mode elixir-mode)
    ("\\.exs\\'" elixir-mode elixir-mode)
    ("\\.elixir\\'" elixir-mode elixir-mode)
    ("\\.erl\\'" erlang erlang-mode)
    ("\\.feature\\'" feature-mode feature-mode)
    ("\\.go\\'" go-mode go-mode)
    ("\\.graphql\\'" graphql-mode graphql-mode)
    ("\\.groovy\\'" groovy-mode groovy-mode)
    ("\\.haml\\'" haml-mode haml-mode)
    ("\\.hs\\'" haskell-mode haskell-mode)
    ("\\.jl\\'" julia-mode julia-mode)
    ("\\.json\\'" json-mode json-mode)
    ("\\.kt\\'" kotlin-mode kotlin-mode)
    ("\\.kv\\'" kivy-mode kivy-mode)
    ("\\.latex\\'" auctex LaTeX-mode)
    ("\\.less\\'" less-css-mode less-css-mode)
    ("\\.log\\'" logview log-view-mode)
    ("\\.lua\\'" lua-mode lua-mode)
    ("\\.markdown\\'" markdown-mode markdown-mode)
    ("\\.md\\'" markdown-mode markdown-mode)
    ("\\.ml\\'" tuareg tuareg-mode)
    ("\\.pp\\'" puppet-mode puppet-mode)
    ("\\.php\\'" php-mode php-mode)
    ("\\.proto\\'" protobuf-mode protobuf-mode)
    ("\\.pug\\'" pug-mode pug-mode)
    ("\\.pyd\\'" cython-mode cython-mode)
    ("\\.pyi\\'" cython-mode cython-mode)
    ("\\.pyx\\'" cython-mode cython-mode)
    ("PKGBUILD\\'" pkgbuild-mode pkgbuild-mode)
    ("\\.rkt\\'" racket-mode racket-mode)
    ("\\.rs\\'" rust-mode rust-mode)
    ("\\.sass\\'" sass-mode sass-mode)
    ("\\.scala\\'" scala-mode scala-mode)
    ("\\.scss\\'" scss-mode scss-mode)
    ("\\.slim\\'" slim-mode slim-mode)
    ("\\.styl\\'" stylus-mode stylus-mode)
    ("\\.svelte\\'" svelte-mode svelte-mode)
    ("\\.swift\\'" swift-mode swift-mode)
    ("\\.textile\\'" textile-mode textile-mode)
    ("\\.tf\\'" terraform-mode terraform-mode)
    ("\\.thrift\\'" thrift thrift-mode)
    ("\\.vue\\'" vue-mode vue-mode)
    ("\\.yml\\'" yaml-mode yaml-mode)
    ("\\.yaml\\'" yaml-mode yaml-mode)
    ("\\.zig\\'" zig-mode zig-mode)
    ("Dockerfile\\'" dockerfile-mode dockerfile-mode)))

;; markdown doesn't have autoloads, manually add them
(when (package-installed-p 'markdown-mode)
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

;; nor adoc
(when (package-installed-p 'adoc-mode)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))
  (add-to-list 'auto-mode-alist '("\\.asciidoc\\'" . adoc-mode)))

;; nor pkgbuild
(when (package-installed-p 'pkgbuild-mode)
  (add-to-list 'auto-mode-alist '("PKGBUILD\\'" . pkgbuild-mode)))

;; now register
(mapc (lambda (entry)
        (let ((extension (car entry))
              (package (cadr entry))
              (mode (cadr (cdr entry))))
          (unless (package-installed-p package)
            (conjure-auto-install extension package mode))))
      conjure-auto-install-alist)

(provide 'init-packages)
;;; init-packages.el ends here
