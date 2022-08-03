;;; init-org.el -- Org Mode Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(org
                            org-roam
                            org-roam-ui))
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(with-eval-after-load 'org
  (defun conjure-org-mode-defaults ()
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure . t)
       (shell . t)
       (ruby . t)
       (python . t))))

  (setq org-babel-clojure-backend 'cider)
  (setq conjure-org-mode-hook 'conjure-org-mode-defaults)

  (add-hook 'outline-mode-hook
            (lambda ()
              (run-hooks 'conjure-org-mode-hook))))

(require 'org-roam)
(setq org-roam-v2-ack t)

(unless (file-exists-p conjure-org-dir)
  (make-directory conjure-org-dir))

(setq org-roam-completion-everywhere t)
(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?\n\n* Related:"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("l" "programming language" plain
         "* Characteristics:\n\n- Family: %?\n- Inspired by:\n\n* Reference:\n\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(with-eval-after-load 'org-roam
  (defun conjure-org-roam-defaults ()
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t)

    (global-set-key (kbd "C-c m l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c m f") 'org-roam-node-find)
    (global-set-key (kbd "C-c m i") 'org-roam-node-insert)
    (global-set-key (kbd "C-c m c") 'org-roam-capture)
    (global-set-key (kbd "C-c m d d") 'org-roam-dailies-goto-today)
    (global-set-key (kbd "C-c m d y") 'org-roam-dailies-goto-yesterday)
    (global-set-key (kbd "C-c m d t") 'org-roam-dailies-goto-tomorrow)
    (global-set-key (kbd "C-c m d b") 'org-roam-dailies-goto-previous-note)
    (global-set-key (kbd "C-c m d c") 'org-roam-dailies-goto-date)

    (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

    (message "[conjure] Org-roam powering up..."))

  (setq conjure-org-roam-hook 'conjure-org-roam-defaults)

  (org-roam-setup)
  (add-hook 'after-init-hook (lambda ()
                               (run-hooks 'conjure-org-roam-hook))))

(provide 'init-org)
;;; init-org.el ends here
