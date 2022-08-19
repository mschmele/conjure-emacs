;;; init-org.el -- Org Mode Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(htmlize
                            org
                            org-bullets
                            org-roam
                            org-roam-ui
                            org-wild-notifier
                            ob-restclient
                            ox-reveal
                            restclient))
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(setq org-startup-with-inline-images t
      org-clock-persist 'history
      org-src-fontify-natively t
      org-todo-keywords
      '((sequence "TODO(t)"
                  "WAIT(w@/!)"
                  "ACTIVE(a@/!)"
                  "PROJ(p)"
                  "|"
                  "DONE(d!)"
                  "CANCELED(c@)")))

(org-clock-persistence-insinuate)

(with-eval-after-load 'org
  (defun conjure-org-mode-defaults ()

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure . t)
       (shell . t)
       (ruby . t)
       (python . t)
       (restclient . t))))

  (setq org-babel-clojure-backend 'cider)
  (setq conjure-org-mode-hook 'conjure-org-mode-defaults)

  (add-hook 'outline-mode-hook
            (lambda ()
              (run-hooks 'conjure-org-mode-hook))))

;; TODO configure with flag
(org-wild-notifier-mode)

(require 'org-roam)
(require 'org-roam-dailies)

(setq org-roam-v2-ack t)

(unless (file-exists-p conjure-org-dir)
  (make-directory conjure-org-dir))

(unless (file-exists-p conjure-org-roam-dir)
  (make-directory conjure-org-roam-dir))

(setq org-roam-completion-everywhere t
      org-roam-directory conjure-org-roam-dir
      org-roam-capture-templates
      '(("d" "Default" plain
         "%?\n\n* Related:"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "Meeting" entry
         "* MEETING with %? :MEETING:\n\n%U\n\n** Location\n\n** Attendees\n+ [X] Self\n\n** Agenda\n\n** Notes\n\n** Actions\n\n"
         :clock-in t :clock-resume t
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(defun org-roam-insert-node-immediate (arg &rest args)
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun conjure/org-roam-filter-by-tag (tag-name)
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun conjure/org-roam-list-notes-by-tag (tag-name)
  (mapcar #'org-roam-node-file
          (seq-filter
           (conjure/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun conjure/org-roam-refresh-agenda-list ()
  (interactive)
  (setq org-agenda-files (conjure/org-roam-list-notes-by-tag "Project")))

(defun conjure/org-roam-copy-todo-to-today ()
  (interactive)
  (let ((org-refile-keep t)
        (org-roam-dailies-capture-templates
         '(("t" "tasks" entry "%?"
            :if-new (file+head "%<%Y%m%d%H%M%S>.org" "#+title: ${title}\n" ("Tasks")))))
        (org-after-refile-insert-hook #'save-buffer)
        today-file
        pos)
    (save-window-excursion
      (org-roam-dailies--capture (current-time) t)
      (setq today-file (buffer-file-name))
      (setq pos (point)))
    (unless (equal (file-truename today-file)
                   (file-truename (buffer-file-name)))
      (org-refile nil nil (list "Tasks" today-file nil pos)))))

(add-to-list 'org-after-todo-state-change-hook
               (lambda ()
                 (when (equal org-state "DONE")
                   (conjure/org-roam-copy-todo-to-today))))

;; Build the agenda
(conjure/org-roam-refresh-agenda-list)

(with-eval-after-load 'org-roam
  (defun conjure-org-roam-defaults ()
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t)

    (global-set-key (kbd "C-c m l") 'org-roam-buffer-toggle)
    (global-set-key (kbd "C-c m f") 'org-roam-node-find)
    (global-set-key (kbd "C-c m i") 'org-roam-node-insert)
    (global-set-key (kbd "C-c m I") 'org-roam-insert-node-immediate)
    (global-set-key (kbd "C-c m c") 'org-roam-capture)
    (global-set-key (kbd "C-c m d d") 'org-roam-dailies-goto-today)
    (global-set-key (kbd "C-c m d y") 'org-roam-dailies-goto-yesterday)
    (global-set-key (kbd "C-c m d t") 'org-roam-dailies-goto-tomorrow)
    (global-set-key (kbd "C-c m d b") 'org-roam-dailies-goto-previous-note)
    (global-set-key (kbd "C-c m d c") 'org-roam-dailies-goto-date)

    (define-key org-mode-map (kbd "C-M-i") 'completion-at-point)

    (message "[conjure] Org-roam powering up..."))

  (org-roam-db-autosync-mode)
  (setq conjure-org-roam-hook 'conjure-org-roam-defaults)

  (org-roam-setup)
  (add-hook 'after-init-hook (lambda ()
                               (run-hooks 'conjure-org-roam-hook))))

(provide 'init-org)
;;; init-org.el ends here
