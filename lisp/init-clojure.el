;;; init-clojure.el -- clojure-mode initialization
;;; Commentary:
;;; Code:
(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :defer t
  :config
  (setq cider-repl-use-pretty-printing t
        cider-repl-display-help-banner nil))

(use-package flycheck-clj-kondo
  :requires flycheck)

(require 'clojure-mode)
(define-clojure-indent
  (defroutes 'defun)
  (routes 0)
  (GET 0)
  (POST 0)
  (PUT 0)
  (DELETE 0)
  (HEAD 0)
  (ANY 0)
  (OPTIONS 0)
  (PATCH 0)
  (rfn 2)
  (let-routes 1)
  (context 2)
  (are3 '(1 (1)))
  (are2 '(1 (2))))

(message "Loaded Clojure configs")
(provide 'init-clojure)
;;; init-clojure.el ends here
