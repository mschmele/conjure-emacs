;;; init-emacs-lisp.el --- emacs-lisp mode initialization
;;; Commentary:
;;; Code:
(require 'init-lisp)
(require 'crux)

(conjure-require-packages '(elisp-slime-nav rainbow-mode))

(defun conjure-recompile-elc-on-save ()
  "Recompile elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p conjure-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun conjure-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'conjure-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun conjure-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun conjure-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'"
  (run-hooks 'conjure-lisp-coding-hook)
  (eldoc-mode +1)
  (conjure-recompile-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (conjure-conditional-emacs-lisp-checker))

(setq conjure-emacs-lisp-mode-hook 'conjure-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (run-hooks 'conjure-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

(defun conjure-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'conjure-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq conjure-ielm-mode-hook 'conjure-ielm-mode-defaults)

(add-hook 'ielm-mode-hook
          (lambda ()
            (run-hooks 'conjure-ielm-mode-hook)))

(with-eval-after-load "elisp-slime-nav"
  (diminish 'elisp-slime-nav-mode))

(with-eval-after-load "rainbow-mode"
  (diminish 'rainbow-mode))

(with-eval-after-load "eldoc"
  (diminish 'eldoc-mode))

(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command `eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

(provide 'init-emacs-lisp)
;;; init-clojure.el ends here
