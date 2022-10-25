;;; init-ivy.el --- Ivy initialization
;;; Commentary:
;;; Code:
(unless (executable-find "ag")
  (message "%s" "executable: ag not found!, counsel-ag will not work"))

(conjure-require-packages '(all-the-icons-ivy-rich
                            counsel
                            helpful
                            ivy
                            ivy-rich
                            swiper))

(require 'ivy)
(require 'diminish)

(setq ivy-use-virtual-buffers t
      ivy-height 15
      ivy-count-format "(%d/%d) "
      ivy-ignore-buffers '("\\` " "\\`\\*tramp/" "\\`\\*xref" "\\`\\*helpful "
                             "\\`\\*.+-posframe-buffer\\*" "\\` ?\\*company-.+\\*"))

(setq enable-recursive-minibuffers t)
(ivy-mode 1)

(require 'projectile)
(setq projectile-completion-system 'ivy)

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

(global-set-key (kbd "C-c c R") 'counsel-list-processes)

(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

(define-key ivy-minibuffer-map (kbd "TAB") 'ivy-alt-done)

;; Make Ivy a bit prettier
(require 'all-the-icons-ivy-rich)
(require 'ivy-rich)
(setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
(setq ivy-rich-path-style 'abbrev)

(all-the-icons-ivy-rich-mode)
(ivy-rich-mode 1)

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
