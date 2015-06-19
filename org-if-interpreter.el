;;; org-if-interpreter.el --- Interpreter for org-if language. -*- lexical-binding: t -*-

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

;;; This file contains the interpreter for the org-if language.

;;; Code:

(require 'org-if-misc)

(defun org-if-conditional (args)
    "Conditionally evaluate the ARGS."
    (cl-labels ((helper (as)
                        (when (not (null as))
                              (if (= (length as) 1)
                                  (org-if-eval (nth 0 as))
                                  (if (org-if-eval (nth 0 as))
                                      (org-if-eval (nth 1 as))
                                      (helper (nthcdr 2 as)))))))
      (if (>= (length args) 2)
          (helper args)
          (error (concat "Invalid arguments to if: "
                         (print1-to-string args))))))

(defun org-if-insert-message (args)
    "Insert message from ARGS into Text heading."
    (if (and (eq (length args) 1) (stringp (car args)))
      (save-excursion
        (org-if-goto-first-heading)
        (org-forward-heading-same-level 1)
        (open-line 1)
        (insert (concat (car args) "\n")))
      (error "Invalid arguments to print: " (prin1-to-string args))))

(defun org-if-insert-choice (args)
  "Insert link from ARGS into Choices heading."
  (let* ((link-path      (nth    0 args))
         (link-with-ext  (if (null (file-name-extension link-path))
                              (concat link-path ".org")
                              link-path))
         (link-full-path (expand-file-name (concat (file-name-directory buffer-file-name)
                                                   link-with-ext)))
         (link-desc      (nth    1 args))
         (link-state     (nthcdr 2 args)))
    (if (and (>= (length args) 2)  (zerop   (% (length args) 2))
             (stringp link-path)   (stringp link-desc)
             (or (consp link-state) (null link-state)))
        (progn
          (save-excursion
            (org-if-goto-first-heading)
            (org-forward-heading-same-level 2)
            (open-line 1)
            (insert (concat "[[if:"
                            link-full-path
                            (if (not (null link-state))
                                (prin1-to-string link-state)
                                "()")
                            "]["
                            link-desc
                            "]]\n"))))
        (error "Invalid arguments to choice: " (prin1-to-string args)))))

(defun org-if-reset-game (args)
  "Initialize game with ARGS."
  (if (null args)
      (clrhash org-if-current-env)
      (error "Invalid reset arguments: " (prin1-to-string args))))

(defun org-if-evlis (lst)
    "Evaluate every element of LST."
    (mapcar #'org-if-eval lst))

(defun org-if-apply (func args)
  "Call function FUNC with arguments ARGS.
Ensure function has NUM arguments."
 (apply func (org-if-evlis args)))

(defun org-if-eval (exp)
  "Evaluate expression EXP in `org-if-current-env'."
  (cond
   ((symbolp exp)            (let ((val (gethash exp
                                                 org-if-current-env)))
                               (if (not (null val))
                                   val
                                 (error (concat "Invalid symbol: " exp)))))
   ((atom    exp)            exp)
   ((eq (nth 0 exp) 'set)    (org-if-set-env     (cdr exp)))
   ((eq (nth 0 exp) 'if)     (org-if-conditional (cdr exp)))
   ((eq (nth 0 exp) '>)      (org-if-apply #'>   (cdr exp)))
   ((eq (nth 0 exp) '<)      (org-if-apply #'<   (cdr exp)))
   ((eq (nth 0 exp) '=)      (org-if-apply #'eq  (cdr exp)))
   ((eq (nth 0 exp) '+)      (org-if-apply #'+   (cdr exp)))
   ((eq (nth 0 exp) '-)      (org-if-apply #'-   (cdr exp)))
   ((eq (nth 0 exp) '*)      (org-if-apply #'*   (cdr exp)))
   ((eq (nth 0 exp) '/)      (org-if-apply #'/   (cdr exp)))
   ((eq (nth 0 exp) '>=)     (org-if-apply #'>=  (cdr exp)))
   ((eq (nth 0 exp) '<=)     (org-if-apply #'<=  (cdr exp)))
   ((eq (nth 0 exp) '!=)     (not (eq (org-if-eval (nth 1 exp))
                                      (org-if-eval (nth 2 exp)))))
   ((eq (nth 0 exp) 'print)  (org-if-insert-message (cdr exp)))
   ((eq (nth 0 exp) 'choice) (org-if-insert-choice  (cdr exp)))
   ((eq (nth 0 exp) 'reset)  (org-if-reset-game     (cdr exp)))
   (t (error (concat "Invalid expression: " (prin1-to-string exp))))))

(defun org-if-interpret (str)
  "Read & evaluate one or more S-Expressions from string STR."
  (let ((pos 0))
    (while (< pos (length str))
      (let* ((res (read-from-string str pos))
             (exp (car res)))
        (setq pos (cdr res))
        (print (org-if-eval exp))))))

(provide 'org-if-interpreter)
;;; org-if-interpreter ends here
