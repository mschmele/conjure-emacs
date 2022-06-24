;;; init-org.el -- Org Mode Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(org org-bullets org-roam))
(require 'org)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(use-package org
  :hook (org-mode . (lambda ()
                      (linum-mode 0)
                      (setq truncate-lines nil)))
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (clojure . t)
     (shell . t)
     (ruby . t)
     (python . t)))
  :config
  (setq org-babel-clojure-backend 'cider))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :config
  (org-roam-setup)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?\n\n* Related:"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("l" "programming language" plain
      "* Characteristics:\n\n- Family: %?\n- Inspired by:\n\n* Reference:\n\n"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ("C-c n d d" . org-roam-dailies-find-today)
         ("C-c n d y" . org-roam-dailies-find-yesterday)
         ("C-c n d t" . org-roam-dailies-find-tomorrow)
         ("C-c n d b" . org-roam-dailies-find-previous-note)
         ("C-c n d c" . org-roam-dailies-find-date)
         :map org-mode-map
         ("C-M-i" . completion-at-point)))

(message "Loaded Org-Mode configs")
(provide 'init-org)
;;; init-org.el ends here
