;;; init-conjure.el --- Custom Conjuration Config
;;; Commentary:
;;; Code:
(require 'easymenu)
(require 'imenu-anywhere)
(require 'crux)

(defvar conjure-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c o") 'crux-open-with)

    (define-key map (kbd "C-a") 'crux-move-beginning-of-line)
    (define-key map [(shift return)] 'crux-smart-open-line)
    (define-key map (kbd "M-o") 'crux-smart-open-line)
    (define-key map [(control shift return)] 'crux-smart-open-line-above)
    (define-key map [(control shift up)] 'move-text-up)
    (define-key map [(control shift down)] 'move-text-down)
    (define-key map [(meta shift up)] 'move-text-up)
    (define-key map [(meta shift down)] 'move-text-down)
    (define-key map (kbd "C-c n") 'crux-cleanup-buffer-or-region)
    (define-key map (kbd "C-c f") 'crux-recentf-find-file)
    (define-key map (kbd "C-M-z") 'crux-indent-defun)
    (define-key map (kbd "C-c u") 'crux-view-url)
    (define-key map (kbd "C-c e") 'crux-eval-and-replace)
    (define-key map (kbd "C-c s") 'crux-swap-windows)
    (define-key map (kbd "C-c D") 'crux-delete-buffer-and-file)
    (define-key map (kbd "C-c d") 'crux-duplicate-current-line-or-region)
    (define-key map (kbd "C-c M-d") 'crux-duplicate-and-comment-current-line-or-region)
    (define-key map (kbd "C-c r") 'crux-rename-buffer-and-file)
    (define-key map (kbd "C-c t") 'crux-visit-term-buffer)
    (define-key map (kbd "C-c k") 'crux-kill-other-buffers)
    (define-key map (kbd "C-c TAB") 'crux-indent-rigidly-and-copy-to-clipboard)
    (define-key map (kbd "C-c I") 'crux-find-user-init-file)
    (define-key map (kbd "C-c S") 'crux-find-shell-init-file)
    (define-key map (kbd "C-c i") 'imenu-anywhere)

    (when conjure-super-keybindings
      (define-key map (kbd "s-p") 'projectile-command-map))
    (define-key map (kbd "C-c p") 'projectile-command-map)

    (when conjure-super-keybindings
      (define-key map (kbd "s-r") 'crux-recentf-find-file)
      (define-key map (kbd "s-j") 'crux-top-join-line)
      (define-key map (kbd "s-k") 'crux-kill-whole-line)
      (define-key map (kbd "s-o") 'crux-smart-open-line-above)

      (define-key map (kbd "s-m m") 'magit-status)
      (define-key map (kbd "s-m j") 'magit-dispatch)
      (define-key map (kbd "s-m k") 'magit-file-dispatch)
      (define-key map (kbd "s-m l") 'magit-log-buffer-file)
      (define-key map (kbd "s-m b") 'magit-blame)

      (define-key map (kbd "s-/") 'hippie-expand))

    (easy-menu-define conjure-mode-menu map
      "Conjure's Menu"
      '("Conjure"
        ("Files"
         ["Open with..." crux-open-with]
         ["Re-Open as root" crux-reopen-as-root]
         ["Delete file and buffer" crux-delete-file-and-buffer]
         ["Rename buffer and file" crux-rename-buffer-and-file]
         ["Find init file" crux-find-user-init-file]
         ["Find custom file" crux-find-user-custom-file]
         ["Find shell config file" crux-find-shell-init-file])
        ("Buffers"
         ["Clean up buffer or region" crux-cleanup-buffer-or-region]
         ["Kill other buffers" crux-kill-other-buffers])
        ("Editing"
         ["Go to beginning of line" crux-move-beginning-of-line]
         ["Kill line" crux-smart-kill-line]
         ["Kill whole line" crux-kill-whole-line]
         ["Insert empty line below" crux-smart-open-line]
         ["Insert empty line above" crux-smart-open-line-above]
         ["Move up" move-text-up]
         ["Move down" move-text-down]
         ["Duplicate line or region" crux-duplicate-current-line-or-region]
         ["Indent rigidly and copy to clipboard" crux-indent-rigidly-and-copy-to-clipboard]
         ["Indent defun" crux-indent-defun]
         ["Insert date" crux-insert-date]
         ["Eval and replace" crux-eval-and-replace])
        ("Windows"
         ["Swap windows" crux-swap-windows])
        ("General"
         ["Visit term buffer" crux-visit-term-buffer]
         ["View URL" crux-view-url])))
    map)
  "Keymap for Conjure mode.")

(define-minor-mode conjure-mode
  "Minor mode to consolidate Emacs Conjure extensions.

\\{conjure-mode-map}"
  :lighter " Conjure"
  :keymap conjure-mode-map
  :global t)

(provide 'conjure-mode)
;;; conjure-mode.el ends here
