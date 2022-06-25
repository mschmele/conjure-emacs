;;; init-python.el --- python-mode initialization
;;; Commentary:
;;; Code:
(require 'electric)
(require 'init-programming)

(conjure-require-packages '(anaconda-mode))

(setq python-shell-interpreter "python3")

(when (boundp 'company-backends)
  (conjure-require-package 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(when (fboundp 'exec-path-from-shell-copy-env)
  (exec-path-from-shell-copy-env "PYTHONPATH"))

(defcustom conjure-python-mode-set-encoding-automatically nil
  "Non-nil values enable auto insertion of '# coding: utf-8' on python buffers."
  :type 'boolean
  :group 'conjure)

(defun conjure-python-mode-defaults ()
  "Defaults for Python programming."
  (subword-mode +1)
  (anaconda-mode +1)
  (eldoc-mode +1)
  (setq-local electric-layout-rules
              '((?: . (lambda ()
                        (and (zerop (first (syntax-ppss)))
                             (python-info-statement-starts-block-p)
                             'after)))))
  (when (fboundp #'python-imenu-create-flat-index)
    (setq-local imenu-create-index-function
                #'python-imenu-create-flat-index))
  (add-hook 'post-self-insert-hook
            #'electric-layout-post-self-insert-function nil 'local)
  (when conjure-python-mode-set-encoding-automatically
    (add-hook 'after-save-hook 'conjure-python-mode-set-encoding nil 'local)))

(setq conjure-python-mode-hook 'conjure-python-mode-defaults)

(add-hook 'python-mode-hook (lambda ()
                              (run-hooks 'conjure-python-mode-hook)))

(provide 'init-python)
;;; init-python.el ends here
