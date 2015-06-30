;;; org-if-active.el --- Active mode for org-if -*- lexical-binding: t -*-

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Keywords: if, org-if, org org-mode

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

;; This file defines the active minor mode for using the org-if system.

;;; Code:

(require 'ob-core)
(require 'org)
(require 'org-if-misc)
(require 'org-if-link)
(require 'org-if-interpreter)
(require 'outline)

(defvar org-if-old-load-languages
  nil
  "Container for value of `org-babel-load-languages'.")

(defvar org-if-began nil "Whether or not org-if-active has just been enabled.")

(defun org-if-hide-code ()
  "Hide all but the first two headings in org file."
  (org-if-goto-first-heading)
  (org-forward-heading-same-level 2)
  (narrow-to-region (point-min) (point)))

(defun org-if-confirm-babel-evaluate (lang body)
    "Replacement for `org-confirm-babel-evaluate' when mode is on.
This keeps babel from pestering the user with confirmation checks every time 
they visit a new file."
    (not (string= lang "org-if")))

(defun org-if-org-mode-hook ()
    "This is the `org-mode-hook' run by `org-if-active-mode'."
    (when (and org-if-current-file
               (get-file-buffer org-if-current-file))
      (kill-buffer (get-file-buffer org-if-current-file)))
    (setf org-if-current-file (file-truename buffer-file-name))
    (setf org-if-old-env   org-if-current-env)
    (show-all)
    (org-babel-execute-buffer)
    (when org-if-began
      (org-if-hide-code)
      (setf org-if-began nil))
    (set-buffer-modified-p nil))

;;;###autoload
(define-minor-mode org-if-active-mode
  "This mode toggles whether the org-if system is active."
  :init-value nil
  :lighter    " Org-IF Active"
  :global     t
  (if org-if-active-mode
    (progn
      (setf org-if-began t)
      (customize-set-variable 'org-confirm-babel-evaluate
                              (function org-if-confirm-babel-evaluate))
      (org-babel-do-load-languages
       'org-babel-load-languages
       '((org-if . t)))
      (add-hook 'org-mode-hook 'org-if-org-mode-hook)
      (add-hook 'org-follow-link-hook 'org-if-hide-code)
      (when (eq major-mode 'org-mode)
        (org-if-org-mode-hook)))
    (progn
      (setf org-if-began nil)
      (customize-set-variable 'org-confirm-babel-evaluate      t)
      (org-babel-do-load-languages
       'org-babel-load-languages
       org-if-old-load-languages)
      (remove-hook 'org-mode-hook 'org-if-org-mode-hook)
      (remove-hook 'org-follow-link-hook 'org-if-hide-code)
      (when (eq major-mode 'org-mode)
        (widen)))))

;;;###autoload
(defun activate-org-if ()
    "Activate org-if-active minor-mode."
    (interactive)
    (org-if-active-mode 1))

;;;###autoload
(defun deactivate-org-if ()
    "Deactivate org-if-active minor-mode."
    (interactive)
    (org-if-active-mode 0))

(provide 'org-if-active)
;;; org-if-active.el ends here
