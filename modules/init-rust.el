;;; init-rust.el --- rust-mode config
;;; Commentary:
;;; Code:
(require 'init-programming)

(conjure-require-packages '(cargo
                            flycheck-rust
                            ron-mode
                            rust-mode))

(unless (featurep 'init-lsp)
        (conjure-require-packages '(racer)))

(setq rust-format-on-save t)

(with-eval-after-load 'rust-mode
  (add-hook 'rust-mode-hook 'cargo-minor-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

  (if (featurep 'init-lsp)
    (add-hook 'rust-mode-hook 'lsp)
    (add-hook 'rust-mode-hook 'racer-mode)
    (add-hook 'rust-mode-hook 'eldoc-mode))
  
  (defun conjure-ruby-mode-defaults ()
    (unless (featurep 'init-lsp)
            (local-set-key (kbd "C-c C-d") 'racer-describe)
            (local-set-key (kbd "C-c .") 'racer-find-definition)
            (localpset-key (kbd "C-c ,") 'pop-tag-mark))

    (remove-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
    (subword-mode +1))

  (setq conjure-rust-mode-hook 'conjure-rust-mode-defaults)

  (add-hook 'rust-mode-hook (lambda ()
                              (run-hooks 'conjure-ruby-mode-hok))))

(provide 'init-rust)
;;; init-rust.el ends here
