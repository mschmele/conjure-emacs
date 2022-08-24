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
  "Pretty-print XML in region betwen BEGIN and END in a sensible manner."
  (interactive "r")
  (save-excursion
    (nxml-mode)
    (goto-char begin)
    (while (search-forward-regexp "\>[ \\t]*\<" nil t)
      (backward-char) (insert "\n"))
    (indent-region begin end)))

(require 'ansi-color)
(defun display-ansi-colors ()
  "Colorize buffer containining ANSI color strings."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(when *is-a-mac*
  (setq JAVA_BASE "/Library/Java/JavaVirtualMachines"))

(when *is-linux*
  (message "[conjure] Linux distro java switching not implemented yet"))

(defun switch-java--versions ()
  "Return a list of installed JDK."
  (seq-remove
   (lambda (a) (or (equal a "." (equal a ".."))))
   (directory-files JAVA_BASE)))

(defun switch-java--save-enf ()
  "Store original PATH and JAVA_HOME."
  (when (not (boundp 'SW_JAVA_PATH))
    (setq SW_JAVA_PATH (getenv "PATH")))
  (when (not (boundp 'SW_JAVA_HOME))
    (setq SW_JAVA_HOME (getenv "JAVA_HOME"))))

(defun switch-java ()
  "List installed JDKs and switch to one."
  (interactive)
  (switch-java--save-env)

  (let ((ver (completing-read "Which Java: " (seq-map-indexed (lambda (e i) (list e i)) (switch-java--versions))
                              nil t "")))
    ;; switch java version
    (setenv "JAVA_HOME" (concat JAVA_BASE "/" ver "/Contents/Home"))
    (setenv "PATH" (concat (concat (getenv "JAVA_HOME") "/bin/java")
                           ":" SW_JAVA_PATH)))
  ;; show the current version
  (switch-java-which-version?))

(defun switch-java-default ()
  "Restore default Java version."
  (interactive)
  (switch-java--save-env)

  ;; switch java version
  (setenv "JAVA_HOME" SW_JAVA_HOME)
  (setenv "PATH" SW_JAVA_PATH)

  ;; show version
  (switch-java-which-version?))

(defun switch-java-which-version? ()
  "Display the current Java version."
  (interactive)
  (message (concat "Java HOME: " (getenv "JAVA_HOME"))))

(provide 'init-common)
;;; init-common.el ends here
