;;; org-if-interpreter.el --- Interpreter for org-if language.
;;; Commentary:

;;; Code:

(setq lexical-binding t) ; Enable lexical binding

(require 'org-if-misc)

(defvar org-if-current-game nil "Reference to current org-if game.")

(defun org-if-insert-message (args)
    "Insert message from ARGS into Text heading."
    (if (and (eq (length args) 1) (stringp (car args)))
      (save-excursion
        (org-if-goto-first-heading)
        (org-forward-heading-same-level 1)
        (open-line 1)
        (insert (car args)))
      (error "Invalid arguments to print: " (prin1-to-string args))))

(defun org-if-insert-choice (args)
    "Insert link from ARGS into Choices heading."
    (if (and (>= (length args) 2)   (<= (length args) 3)
             (stringp (nth 0 args)) (stringp (nth 1 args))
             (or (consp (nth 2 args)) (null (nth 2 args))))
      (save-excursion
        (org-if-goto-first-heading)
        (org-forward-heading-same-level 2)
        (open-line 1)
        (insert (concat "[[file:" (nth 0 args) "][" (nth 1 args) "]]")))
      (error "Invalid arguments to choice: " (prin1-to-string args))))

(defun org-if-apply (func args env num)
  "Call function FUNC with arguments ARGS in environment ENV.
Ensure function has NUM arguments."
  (if (eq (length args) num)
    (apply func
           (mapcar #'(lambda (arg)
                   (org-if-eval arg env)) args))
    (error (concat "Invalid function arguments: " (prin1-to-string args)))))

(defun org-if-init-game (args)
    "Initialize game with ARGS."
    (if (and (eq (length args) 1) (stringp (nth 0 args)))
        (let* ((game-name (nth 0 args))
               (game-sym  (intern (replace-regexp-in-string "\\W+" "_" game-name))))
          (eval `(defvar ,game-sym '(name ,game-name))))))

(defun org-if-eval (exp env)
  "Evaluate expression EXP in environment ENV."
  (cond
   ((symbolp exp)         (plist-get env exp))
   ((atom    exp)         exp)
   ((eq (nth 0 exp) 'set) (setq env (plist-put env
                                               (nth 1 exp)
                                               (org-if-eval (nth 2 exp) env))))
   ((eq (nth 0 exp) 'if)     (if (org-if-eval (nth 1 exp) env)
                               (org-if-eval (nth 2 exp) env)
                               (when (not (eq (nthcdr 2 exp) '()))
                                 (org-if-eval (nth 3 exp) env))))
   ((eq (nth 0 exp) '>)      (org-if-apply #'>   (cdr exp) env 2))
   ((eq (nth 0 exp) '<)      (org-if-apply #'<   (cdr exp) env 2))
   ((eq (nth 0 exp) '=)      (org-if-apply #'eq  (cdr exp) env 2))
   ((eq (nth 0 exp) '+)      (org-if-apply #'+   (cdr exp) env 2))
   ((eq (nth 0 exp) '-)      (org-if-apply #'-   (cdr exp) env 2))
   ((eq (nth 0 exp) '*)      (org-if-apply #'*   (cdr exp) env 2))
   ((eq (nth 0 exp) '/)      (org-if-apply #'/   (cdr exp) env 2))
   ((eq (nth 0 exp) '>=)     (org-if-apply #'>=  (cdr exp) env 2))
   ((eq (nth 0 exp) '<=)     (org-if-apply #'<=  (cdr exp) env 2))
   ((eq (nth 0 exp) '!=)     (not (eq (org-if-eval (nth 1 exp) env)
                                      (org-if-eval (nth 2 exp) env))))
   ((eq (nth 0 exp) 'print)  (org-if-insert-message (cdr exp)))
   ((eq (nth 0 exp) 'choice) (org-if-insert-choice  (cdr exp)))
   ((eq (nth 0 exp) 'init)   (org-if-init-game      (cdr exp)))
   (t (error (concat "Invalid expression: " (prin1-to-string exp))))))

(defun org-if-interp (str)
  "Read & evaluate one or more S-Expressions from string STR."
  (let ((pos 0)
        (env org-if-current-game))
    (while (< pos (length str))
      (let* ((res (read-from-string str pos))
             (exp (car res)))
        (setq pos (cdr res))
        (print (org-if-eval exp env))))))

(provide 'org-if-interpreter)
;;; org-if-interpreter ends here
