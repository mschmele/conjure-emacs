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

;; NOTE: You do need to have portal on the class path and the easiest way I know
;; how is via a clj user or project alias.
(setq cider-clojure-cli-global-options "-A:portal")

(provide 'init-portal)