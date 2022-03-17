;;; init-clojure.el -- clojure-mode initialization
;;; Commentary:
;;; Code:

(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :defer t)

(use-package flycheck-clj-kondo
  :requires flycheck)

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
  (context 2))

(message "Loaded Clojure configs")

(provide 'init-clojure)
;;; init-clojure.el ends here
