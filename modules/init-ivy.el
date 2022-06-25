;;; init-ivy.el -- Ivy initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(all-the-icons
                            all-the-icons-ivy
                            all-the-icons-ivy-rich
                            counsel
                            ivy
                            ivy-posframe
                            ivy-rich
                            swiper))

(require 'ivy)
(require 'diminish)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)

(diminish 'ivy-mode)

(global-set-key "\C-s" 'swiper)

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-describe-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c a") 'counsel-ag)
(global-set-key (kbd "C-c l") 'counsel-locate)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; (use-package ivy
;;   :diminish
;;   :bind (("C-s" . swiper)
;;          :map ivy-minibuffer-map
;;          ("TAB" . ivy-alt-done)
;;          ("C-l" . ivy-alt-done)
;;          ("C-j" . ivy-next-line)
;;          ("C-k" . ivy-previous-line)
;;          :map ivy-switch-buffer-map
;;          ("C-K" . ivy-previous-line)
;;          ("C-l" . ivy-done)
;;          ("C-d" . ivy-switch-buffer-kill)
;;          :map ivy-reverse-i-search-map
;;          ("C-k" . ivy-previous-line)
;;          ("C-d" . ivy-reverse-i-search-kill))
;;   :init
;;   (ivy-mode 1)
;;   :config
;;   (setq ivy-count-format "(%d/%d) "
;;         ivy-use-virtual-buffers t))

;; (use-package ivy-rich
;;   :init
;;   (ivy-rich-mode 1)
;;   :config
;;   (setq ivy-format-function #'ivy-format-function-line))

;; (use-package all-the-icons-ivy
;;   :requires all-the-icons
;;   :after (ivy all-the-icons)
;;   :config (all-the-icons-ivy-setup))

;; (use-package all-the-icons-ivy-rich
;;   :requires all-the-icons
;;   :after (ivy-rich all-the-icons)
;;   :init (all-the-icons-ivy-rich-mode 1))

;; (use-package ivy-posframe
;;   :diminish
;;   :after ivy
;;   :init
;;   (ivy-posframe-mode)
;;   :config
;;   (setq ivy-posframe-parameters
;;         '((left-fringe . 8)
;;           (right-fringe . 8))
;;         ivy-posframe-height-alist
;;         '((swiper . 15)
;;           (t . 10))
;;         ivy-posframe-display-functions-alist
;;         '((complete-symbol . ivy-posframe-display-at-point)
;;           (counsel-describe-function . nil)
;;           (counsel-describe-variable . nil)
;;           (counsel-M-x . ivy-posframe-display-at-window-bottom-left)
;;           (t . ivy-posframe-display-at-frame-center))))

(provide 'init-ivy)
;;; init-ivy.el ends here
