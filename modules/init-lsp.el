;;; init-lsp.el --- lsp-mode config
;;; Commentary:
;;; Code:
(conjure-require-packages '(lsp-mode
                            lsp-treemacs))
(require 'lsp-mode)
(lsp-enable-which-key-integration +1)

(defun conjure/lsp-treemacs-symbols-toggle ()
  "Toggle lsp-treemacs-symbols buffer."
  (interactive)
  (if (get-buffer "*LSP Symbols List*")
      (kill-buffer "*LSP Symbols List*")
    (progn (lsp-treemacs-symbols)
           (other-window -1))))

(defun conjure/lsp-treemacs-errors-toggle ()
  "Toggle lsp-treemacs-error buffer."
  (interactive)
  (if (get-buffer "*LSP Error List*")
      (kill-buffer "*LSP Error List*")
    (progn (lsp-treemacs-errors-list)
           (other-window -1))))

(setq lsp-treemacs-symbols-sort-functions '(lsp-treemacs-sort-by-kind lsp-treemacs-sort-by-name))

(provide 'init-lsp)
;;; init-lsp.el ends here
