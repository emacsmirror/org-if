;;; org-if-misc.el --- Miscellaneous functions for org-if-mode
;;; Commentary:
;;;
;;; Code:

(setq lexical-binding t) ; Enable lexical binding

(defun org-if-goto-first-heading ()
  "Go to the line containing the first major heading in the current buffer."
  (goto-char (point-min))
  (while (not (equal "* " (buffer-substring-no-properties (line-beginning-position)
                                                          (+ 2 (line-beginning-position)))))
    (forward-line 1)))

(provide 'org-if-misc)
;;; org-if-misc.el ends here
