;;; org-if-misc.el --- Miscellaneous functions for org-if-mode -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; This file contains miscellaneous functions for org-if-mode.

;;; Code:

(require 'cl)
(require 'cl-macs)

(defvar org-if-current-file
  nil
  "This is the current file when `org-if-active-mode' is enabled.")

(defvar org-if-current-env (make-hash-table)
  "Reference to current org-if environment.")

(defvar org-if-old-env nil "Reference to previous org-if environment.")

(defgroup org-if
  nil
  "Interactive Fiction Authoring System for Org-Mode."
  :group 'applications)

(defun org-if-goto-first-heading ()
  "Go to the line containing the first major heading in the current buffer."
  (goto-char (point-min))
  (while (not (equal "* "
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (+ 2 (line-beginning-position)))))
    (forward-line 1)))

(defun org-if-set-link-state (list)
  "Set the new state of `org-if-current-env' with values from LIST."
  (cl-labels ((helper (vars)
                    (when (not (null vars))
                      (let ((key (nth 0 vars))
                            (val (nth 1 vars)))
                        (puthash key (org-if-eval val) org-if-current-env))
                      (helper (nthcdr 2 vars)))))
    (if (evenp (length list))
        (helper list)
      (error (concat "Invalid parameters passed to `org-if-set-link-state': "
                     (prin1-to-string list))))))

(provide 'org-if-misc)
;;; org-if-misc.el ends here
