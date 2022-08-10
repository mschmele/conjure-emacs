;;; init-common.el --- conjure functions
;;; Commentary:
;;; Code:
(defun conjure-abbreviate-file-path (file)
  "Abbreviates directory names of the FILE."
  (let ((tokens (cdr (split-string (file-relative-name file) "/"))))
    (string-join (append (mapcar (lambda (candidate)
                                   (substring candidate 0 1))
                                 (butlast tokens))
                         (last tokens)) "/")))

(defun conjure-pretty-print-xml-region (begin end)
  "Pretty-print XML in a sensible manner."
  (interactive "r")
  (save-excursion
      (nxml-mode)
      (goto-char begin)
      (while (search-forward-regexp "\>[ \\t]*\<" nil t)
        (backward-char) (insert "\n"))
      (indent-region begin end)))

(require 'ansi-color)
(defun display-ansi-colors ()
  "Colorize buffer containining ANSI color strings. "
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(provide 'init-common)
;;; init-common.el ends here
