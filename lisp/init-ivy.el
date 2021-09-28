;;; init-ivy.el --- ivy completion
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish
  :bind(("C-s" . counsel-grep-or-swiper)
        :map ivy-minibuffer-map
        ("TAB" . ivy-alt-done)
        ("C-l" . ivy-alt-done)
        ("C-j" . ivy-next-line)
        ("C-k" . ivy-previous-line)
        :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map
        ("C-k" . ivy-previous-line)
        ("C-d" . ivy-reverse-i-search-kill))
  :config (ivy-mode 1))

(use-package all-the-icons-ivy-rich
  :defer 2
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after (ivy counsel)
  :init (ivy-rich-mode 1))

(use-package smex
  :defer 1
  :after counsel
  :config (smex-initialize))

;; Improve fuzzy searching in Ivy
(use-package flx
  :init
  (setq ivy-flx-limit 5000))

(provide 'init-ivy)
;;; init-ivy.el ends here
