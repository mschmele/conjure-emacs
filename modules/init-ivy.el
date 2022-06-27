;;; init-ivy.el --- Ivy initialization
;;; Commentary:
;;; Code:
(conjure-require-packages '(all-the-icons
                            all-the-icons-ivy-rich
                            amx
                            counsel
                            helpful
                            ivy
                            ivy-rich
                            ivy-posframe
                            swiper))

(require 'ivy)
(require 'amx)
(require 'diminish)

(ivy-mode 1)
(setq ivy-use-virtual-buffers t
      ivy-count-format "(%d/%d) ")

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

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)

(require 'ivy-posframe)
(setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8))
        ivy-posframe-height-alist
        '((swiper . 15)
          (t . 10))
        ivy-posframe-display-functions-alist
        '((complete-symbol . ivy-posframe-display-at-point)
          (counsel-describe-function . nil)
          (counsel-describe-variable . nil)
          (t . ivy-posframe-display-at-frame-center)))

(ivy-posframe-mode)
(diminish 'ivy-posframe-mode)

(all-the-icons-ivy-rich-mode 1)
(ivy-rich-mode 1)

(setq amx-backend 'auto
          amx-save-file (expand-file-name "amx-items" conjure-savefile-dir)
          amx-history-length 50
          amx-show-key-bindings nil)
(amx-mode 1)

(require 'counsel)
(require 'helpful)
(setq counsel-describe-function-function #'helpful-callable)
(setq counsel-describe-variable-function #'helpful-variable)

(global-set-key [remap describe-function] #'counsel-describe-function)
(global-set-key [remap describe-command] #'helpful-command)
(global-set-key [remap describe-variable] #'counsel-describe-variable)
(global-set-key [remap describe-key] #'helpful-key)

(provide 'init-ivy)
;;; init-ivy.el ends here
