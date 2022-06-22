;;; init-clojure.el -- clojure-mode initialization
;;; Commentary:
;;; Code:
(require 'init-lisp)

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :defer t
  :config
  (setq cider-repl-use-pretty-printing t
        cider-repl-display-help-banner nil
        cider-print-fn 'puget))

(use-package flycheck-clj-kondo
  :requires flycheck)

(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (GET 2)
  (POST 2)
  (PUT 2)
  (DELETE 2)
  (HEAD 2)
  (ANY 2)
  (OPTIONS 2)
  (PATCH 2)
  (rfn 2)
  (let-routes 1)
  (context 2)
  (are3 1)
  (are2 1))

(provide 'init-clojure)
;;; init-clojure.el ends here
