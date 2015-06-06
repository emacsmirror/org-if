;;; org-if.el --- Interactive Fiction Authoring System for Org-Mode

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

;; This package helps you create Choose Your Own Adventure stye Interactive
;; Fiction using Emacs and Org-Mode.

;;; Code:

(setq lexical-binding t) ; Enable lexical binding

(require 'ob-org-if)
(require 'org-if-interpreter)
(require 'org-if-misc)
(require 'org-if-mode)

(defgroup org-if nil "Interactive Fiction Authoring System for Org-Mode.")

(defvar org-if-old-babel-evaluate
  nil
  "Container for value of `org-confirm-babel-evaluate'.")
(defvar org-if-old-elisp-link-function
  nil
  "Container for value of `org-confirm-elisp-link-function'.")
(defvar org-if-old-load-languages
  nil
  "Container for value of `org-babel-load-languages'.")

(defun org-if-hide-code ()
  "Hide all but the first two headings in org file."
  (interactive)
  (org-if-goto-first-heading)
  (org-forward-heading-same-level 2)
  (narrow-to-region (point-min) (point)))

(defun org-if-kill-all-other-org-buffers ()
  "Kill all other `org-mode' buffers except for the current buffer."
  (let* ((buflist (remove-if #'(lambda (b) (eq b (current-buffer)))
                             (buffer-list)))
         (bwithm  (mapcar #'(lambda (buf)
                              (cons (buffer-name buf)
                                    (with-current-buffer buf major-mode)))
                          buflist))
         (orgs    (remove-if #'null
                             (mapcar #'(lambda (buf)
                                         (when (eq (cdr buf) 'org-mode)
                                           (car buf)))
                                     bwithm))))
    (mapcar #'kill-buffer orgs)))

(defun org-if-confirm-babel-evaluate (lang body)
    "Replacement for `org-confirm-babel-evaluate' when mode is on.
This keeps babel from pestering the user with confirmation checks everytime they visit a new file."
    (not (string= lang "org-if")))

(defun org-if-confirm-elisp-link-function (lang body)
    "Replacement for `org-confirm-elisp-link-function' when mode is on.
This keeps babel from pestering the user with confirmation checks everytime they follow a link."
    (not (string= lang "org-if")))

(defun org-if-org-mode-hook ()
    "This is the `org-mode-hook' run by `org-if-active-mode'."
    (show-all)
    (org-babel-execute-buffer)
    (org-if-hide-code))

(define-minor-mode org-if-active-mode
  "This mode toggles whether the org-if system is active."
  :init-value nil
  :lighter    " Org-IF Active"
  :global     t
  :group      org-if
  (if org-if-active-mode
      (progn
        (setf org-if-old-babel-evaluate org-confirm-babel-evalute)
        (setf org-confirm-babel-evalute 'org-if-confirm-babel-evaluate)
        (setf org-if-old-elisp-link-function org-confirm-elisp-link-function)
        (setf org-confirm-elisp-link-function 'org-if-confirm-elisp-link-function)
        (setf org-if-old-load-languages org-babel-load-languages)
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((org-if . t)))
        (add-hook 'org-mode-hook 'org-if-org-mode-hook)
        (when (eq major-mode 'org-mode)
          (org-if-org-mode-hook)))
      (progn
        (setf org-confirm-babel-evalute org-if-old-babel-evaluate)
        (setf org-if-confirm-babel-evaluate nil)
        (setf org-confirm-elisp-link-function org-if-old-elisp-link-function)
        (setf org-if-confirm-elisp-link-function nil)
        (org-babel-do-load-languages
         'org-babel-load-languages
         org-if-old-load-languages)
        (remove-hook 'org-mode-hook 'org-if-org-mode-hook))))

(provide 'org-if)
;;; org-if.el ends here
