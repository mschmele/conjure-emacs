;;; init-clojure.el --- clojure-mode initialization
;;; Commentary:
;;; Code:
(require 'init-lisp)
(conjure-require-packages '(clojure-mode cider flycheck-clj-kondo))
(require 'flycheck-clj-kondo)

(require 'clojure-mode)

(with-eval-after-load 'clojure-mode
  (defun conjure-clojure-mode-defaults ()
    "Setup defaults for `clojure-mode'."
    (subword-mode +1)
    (run-hooks 'conjure-lisp-coding-hook))

  (setq conjure-clojure-mode-hook 'conjure-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (run-hooks 'conjure-clojure-mode-hook))))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t
        cider-repl-display-help-banner nil
        cider-print-fn 'fipp)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun conjure-cider-repl-mode-defaults ()
    "Setup defaults for when `cider' loads."
    (subword-mode +1)
    (run-hooks 'conjure-interactive-lisp-coding-hook))

  (setq conjure-cider-repl-mode-hook 'conjure-cider-repl-mode-defaults)

  (define-key clojure-mode-map (kbd "C-c C-a") 'cider-format-buffer)

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (run-hooks 'conjure-cider-repl-mode-hook))))

;; handle Compojure formatting as specified
;; https://github.com/weavejester/compojure/wiki/Emacs-indentation
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

(provide 'init-clojure)
;;; init-clojure.el ends here
