;;; init-clojure.el -- clojure-mode initialization
;;; Commentary:
;;; Code:
(require 'init-lisp)
(conjure-require-packages '(clojure-mode cider))

(require 'clojure-mode)

(with-eval-after-load 'clojure-mode
  (defun conjure-clojure-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'conjure-lisp-coding-hook))

  (setq conjure-clojure-mode-hooks 'conjure-clojure-mode-defaults)

  (add-hook 'clojure-mode-hook
            (lambda ()
              (run-hooks 'conjure-clojure-mode-hooks))))

(with-eval-after-load 'cider
  (setq nrepl-log-messages t)

  (add-hook 'cider-mode-hook 'eldoc-mode)

  (defun conjure-cider-repl-mode-defaults ()
    (subword-mode +1)
    (run-hooks 'conjure-interactive-lisp-coding-hook))

  (setq conjure-cider-repl-mode-hook 'conjure-cider-repl-mode-defaults)

  (setq cider-format-code-options
      '(("indents" (("org.me/foo" (("inner" 0)))))
        ("alias-map" (("me" "org.me")))))

  (add-hook 'cider-repl-mode-hook
            (lambda ()
              (run-hooks 'conjure-cider-repl-mode-hook)))

  (add-hook 'before-save-hook 'cider-format-buffer t t))

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
  (context 2)
  ;; add CMR specific indentation
  (are3 1)
  (are2 1))

(provide 'init-clojure)
;;; init-clojure.el ends here
