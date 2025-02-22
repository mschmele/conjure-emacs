;;; init-python.el --- python-mode initialization
;;; Commentary:
;;; Code:
(require 'electric)
(require 'init-programming)

(conjure-require-packages '(anaconda-mode))

(setq python-shell-interpreter "python3"
      python-shell-completion-native-enable t
      python-shell-completion-native-disabled-interpreters '("python"))

(when (boundp 'company-backends)
  (conjure-require-package 'company-anaconda)
  (add-to-list 'company-backends 'company-anaconda))

(defun conjure-python--encoding-comment-required-p ()
  (re-search-forward "[^\0-\177]" nil t))

(defun conjure-python--detect-encoding ()
  (let ((coding-system
         (or save-buffer-coding-system
             buffer-file-coding-system)))
    (if coding-system
        (symbol-name
         (or (coding-system-get coding-system 'mime-charset)
             (coding-system-change-eol-conversion coding-system nil)))
      "ascii-8bit")))

(defun conjure-python--insert-coding-comment (encoding)
  (let ((newlines (if (looking-at "^\\s *$") "\n" "\n\n")))
    (insert (format "# coding: %s" encoding) newlines)))

(defun conjure-python-mode-set-encoding ()
  "Insert a magic comment header with the proper encoding if necessary."
  (save-excursion
    (widen)
    (goto-char (point-min))
    (when (conjure-python--encoding-comment-required-p)
      (goto-char (point-min))
      (let ((coding-system (conjure-python--detect-encoding)))
        (when coding-system
          (if (looking-at "^#!") (beginning-of-line 2))
          (cond ((looking-at "\\s *#\\s *.*\\(en\\)?coding\\s *:\\s *\\([-a-z0-9_]*\\)")
                 ;; update existing encoding comment if necessary
                 (unless (string= (match-string 2) coding-system)
                   (goto-char (match-beginning 2))
                   (delete-region (point) (match-end 2))
                   (insert coding-system)))
                ((looking-at "\\s *#.*coding\\s *[:=]"))
                (t (conjure-python--insert-coding-comment coding-system)))
          (when (buffer-modified-p)
            (basic-save-buffer-1)))))))


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
