;;; init-ivy.el -- Ivy initialization
;;; Commentary:
;;; Code:
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-K" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-count-format "(%d/%d) "
        ivy-use-virtual-buffers t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1)
  :config
  (setq ivy-format-function #'ivy-format-function-line))

(use-package all-the-icons-ivy
  :requires all-the-icons
  :after (ivy all-the-icons)
  :config (all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :requires all-the-icons
  :after (ivy-rich all-the-icons)
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-hydra
  :after ivy)

(provide 'init-ivy)
;;; init-ivy.el ends here
