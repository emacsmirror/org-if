;;; org-if-active.el --- Active mode for org-if -*- lexical-binding: t -*-

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

;; This file defines the active minor mode for using the org-if system.

;;; Code:

(require 'ob-core)
(require 'org)
(require 'org-if-misc)
(require 'org-if-link)
(require 'org-if-interpreter)
(require 'outline)

(defvar org-if-began nil "Whether `org-if-active-mode' has just been enabled.
This variable lets `org-if-mode-hook' know if it should call `org-if-hide-code';
that hook should only hide code when `org-if-active-mode' was enabled in an
org buffer.  Otherwise, the `org-follow-link-hook' should hide the code.")

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
    ; First kill the previous buffer visited by org-if so it can be
    ; re-evaluated if the user returns to it.
    (when (and *org-if-current-file*
               (get-file-buffer *org-if-current-file*))
      (kill-buffer (get-file-buffer *org-if-current-file*)))
    (setf *org-if-current-file* (file-truename buffer-file-name))
    (setf *org-if-old-env*      (copy-hash-table *org-if-current-env*))
    (outline-show-all)
    (org-babel-execute-buffer)
    ; `org-if-hide-code' should usually be called from `org-follow-link-hook'.
    ; However, if `org-if-active-mode' is enabled in an org buffer,
    ; `org-if-hide-code' must be called manually.
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
      (org-if-reset-env)  ; Clear any data leftover from previous session
      (when (eq major-mode 'org-mode)
        (org-if-org-mode-hook)))
    (progn
      (setf org-if-began nil)
      (custom-reevaluate-setting 'org-confirm-babel-evaluate)
      (custom-reevaluate-setting 'org-babel-load-languages)
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

;;;###autoload
(defun toggle-org-if-active-mode ()
    "Toggle `org-if-active-mode'."
    (interactive)
    (if org-if-active-mode
        (deactivate-org-if)
        (activate-org-if)))

;;;###autoload
(defun org-if-save-and-quit ()
  "Save state of current org-if session in a file in `org-if-save-dir'.
Then quit."
  (interactive)
  (cl-labels ((parent-dir   (f)
                            (file-name-nondirectory
                             (directory-file-name (file-name-directory f))))
              (write-string (str fname)
                            (with-temp-buffer
                              (insert str)
                              (write-region (point-min)
                                            (point-max)
                                            fname
                                            nil))))
    (let* ((env-name  (parent-dir buffer-file-name))
           (file-name (concat (file-name-as-directory org-if-save-dir)
                              env-name))
           (state     (list *org-if-old-env*
                            *org-if-current-file*)))
      (when (not (file-directory-p org-if-save-dir))
        (make-directory org-if-save-dir))
      (write-string   (prin1-to-string state) file-name)
      (deactivate-org-if)
      (kill-buffer (current-buffer)))))

;;;###autoload
(defun org-if-restore ()
  "Restore state of `*org-if-current-env*' and `*org-if-current-file*' from save.
Also load last visited file."
  (interactive)
  (cl-labels ((read-from-file (path)
                              (with-temp-buffer
                                (insert-file-contents path)
                                (car (read-from-string (buffer-string)))))
              (parent-dir   (f)
                            (file-name-nondirectory
                             (directory-file-name (file-name-directory f)))))
    (let* ((game-name (parent-dir      buffer-file-name))
           (game-path (concat          (file-name-as-directory org-if-save-dir)
                                       game-name))
           (state     (read-from-file  game-path))
           (env       (nth             0 state))
           (file      (nth             1 state)))
      (when (get-file-buffer file)
        (kill-buffer file))
      ; Since the user needs to load one file from the proper directory before
      ; using Restore, this will prevent that file from appearing unevaluated.
      (when (eq major-mode 'org-mode)
        (kill-buffer (current-buffer)))
      ; I switch to the scratch buffer to activate org-if-active-mode
      ; because the current buffer may be an org buffer with org-if code
      ; and I do not want to run it.
      (switch-to-buffer (get-buffer-create "*scratch*"))
      (activate-org-if)
      (setf *org-if-current-env* env)
      (find-file file))))

(provide 'org-if-active)
;;; org-if-active.el ends here
