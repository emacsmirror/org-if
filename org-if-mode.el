;;; org-if-mode.el --- Major mode for ORG-IF

;; Copyright © 2015 Philip Woods

;; Author: Philip Woods <elzairthesorcerer@gmail.com>
;; Version: 0.1
;; Keywords: lang syntax

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

;; Syntax highlighting for ORG-IF code

;;; Code:

(setq lexical-binding t) ; Enable lexical binding

(require 'generic-x)

(define-generic-mode
  'org-if-mode
  '(";")
  '("choice" "if" "print" "reset" "set" "=" "+" "-" "*" "/"
    "<" "<=" "=" ">=" ">" "!=")
  nil
  '("\\.org-if$") ; For testing purposes only
  nil
  "Major mode for ORG-IF programming language.")

(provide 'org-if-mode)
;;; org-if-mode.el ends here
