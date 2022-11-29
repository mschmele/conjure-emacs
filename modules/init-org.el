;;; init-org.el --- Org Mode Initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(htmlize
                            org
                            org-bullets
                            org-modern
                            org-roam
                            org-roam-ui
                            ob-restclient
                            ox-reveal
                            restclient))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

(require 'org)
(setq org-startup-with-inline-images t
      org-src-fontify-natively t
      org-todo-keywords
      '((sequence "TODO(t)"
                  "WAIT(w@/!)"
                  "ACTIVE(a@/!)"
                  "PROJ(p)"
                  "|"
                  "DONE(d!)"
                  "CANCELED(c@)")))

;; persist org-clock across session
(require 'org-clock)
(setq org-clock-persist 'history
      org-clock-persist-file (expand-file-name "org-clock-save.el" conjure-savefile-dir))
(org-clock-persistence-insinuate)

(with-eval-after-load 'org
  (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (clojure . t)
       (graphql . t)
       (shell . t)
       (ruby . t)
       (python . t)
       (restclient . t))))

(require 'org-roam)
(require 'org-roam-dailies)

(unless (file-exists-p conjure-org-dir)
  (make-directory conjure-org-dir))

(unless (file-exists-p conjure-org-roam-dir)
  (make-directory conjure-org-roam-dir))

(setq org-roam-completion-everywhere t
      org-roam-directory conjure-org-roam-dir
      org-roam-capture-templates
      '(("d" "Default" plain
         "%?\n"
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)
        ("m" "Meeting" entry
         "* MEETING with %? :MEETING:\n\n%U\n\n** Location\n\n** Attendees\n+ [X] Self\n\n** Agenda\n\n** Notes\n\n** Actions\n\n"
         :clock-in t :clock-resume t
         :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
         :unnarrowed t)))

(defun org-roam-insert-node-immediate (arg &rest args)
  "Insert a node without prompting for additional information.
Takes ARG and optionally ARGS as pass-thrus."
  (interactive "P")
  (let ((args (cons arg args))
        (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                  '(:immediate-finish t)))))
    (apply #'org-roam-node-insert args)))

(defun conjure/org-roam-filter-by-tag (tag-name)
  "Return a function that filters by TAG-NAME."
  (lambda (node)
    (member tag-name (org-roam-node-tags node))))

(defun conjure/org-roam-list-notes-by-tag (tag-name)
  "Return a list of nodes with TAG-NAME."
  (mapcar #'org-roam-node-file
          (seq-filter
           (conjure/org-roam-filter-by-tag tag-name)
           (org-roam-node-list))))

(defun conjure/org-roam-refresh-agenda-list ()
  "Filter org-agenda-files by Project."
  (interactive)
  (setq org-agenda-files (cons (expand-file-name "agenda.org" conjure-org-dir)
                               (conjure/org-roam-list-notes-by-tag "Project"))))

(defun conjure/org-roam-copy-todo-to-today ()
  "Move tasks accomplished today to tadays org-roam node."
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

(add-hook 'org-mode-hook (lambda ()
                           (electric-indent-local-mode -1)))
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

;; Build the agenda
(conjure/org-roam-refresh-agenda-list)

(setq org-roam-ui-open-on-start nil
      org-roam-ui-sync-theme t
      org-roam-ui-follow t
      org-roam-ui-update-on-save t)

(defun conjure-org-roam-defaults ()
  "Configure sensible defaults for `org-roam'."
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
                             (run-hooks 'conjure-org-roam-hook)))

;; Add org-babel support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (shell      . t)
   (sql        . t)
   (dot        . t)
   (gnuplot    . t)
   (clojure    . t)))
(setq org-src-window-setup         'current-window
      org-src-fontify-natively     t
      org-confirm-babel-evaluate   nil
      org-src-preserve-indentation t)

;; Use CIDER for evaluating clojure code blocks
(require 'cider)
(setq org-babel-clojure-backend 'cider)

(provide 'init-org)
;;; init-org.el ends here
