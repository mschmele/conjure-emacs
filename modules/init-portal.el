(require 'cider)
(require 'clojure-mode)
(require 'nrepl-dict)

;; Leverage an existing cider nrepl connection to evaluate portal.api functions
;; and map them to convenient key bindings.

;; def portal to the dev namespace to allow dereferencing via @dev/portal
(defun portal-open ()
  (interactive)
  (cider-nrepl-sync-request:eval
    "(do (ns dev) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))"))

(defun portal-clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun portal-quit ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close)"))

(defun portal-tap-function-arguments ()
  (interactive)
  (let* ((current-ns (clojure-find-ns))
         (this-var   (word-at-point))
         (this-fn    (cider-defun-at-point))
         (dict       (cider-resolve-var current-ns this-fn))
         (arglists   (nrepl-dict-get dict "arglists")))
    (message "Var %s/%s has arglists %s" current-ns this-fn arglists)))

(defun portal-inspect-expr (expr ns)
  "Evaluate EXPR in NS and inspect its value.
Interactively, EXPR is read from the minibuffer, and NS the
current buffer's namespace."
  (interactive (list (cider-read-from-minibuffer "Inspect expression: " (cider-sexp-at-point))
                     (cider-current-ns)))
  (setq cider-inspector--current-repl (cider-current-repl))
  (when-let* ((value (cider-sync-request:inspect-expr
                      expr ns
                      cider-inspector-page-size
                      cider-inspector-max-atom-length
                      cider-inspector-max-coll-size)))
    value))

;; (defun portal-tap-function-arguments ()
;;   "Get all function arguments at current expr and put em' into a tap>"
;;   (interactive)
;;   (setq value (portal-inspect-expr (cider-defun-at-point) (cider-current-ns)))

;;   (print value)
;;   ;;(concat "(tap>" value ")")
;;   )

;; NOTE: You do need to have portal on the class path and the easiest way I know
;; how is via a clj user or project alias.
(setq cider-clojure-cli-global-options "-A:portal")
(setq cider-clojurescript-cli-global-options "-A:portal")

(provide 'init-portal)
