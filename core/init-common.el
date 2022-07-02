;;; init-common.el -- common functions
;;; Commentary:
;;; Code:
(defun conjure-abbreviate-file-path (file)
  "Abbreviates directory names of the FILE."
  (let ((tokens (cdr (split-string (file-relative-name file) "/"))))
    (string-join (append (mapcar (lambda (candidate)
                                   (substring candidate 0 1))
                                 (butlast tokens))
                         (last tokens)) "/")))

(provide 'init-common)
;;; init-common.el ends here
