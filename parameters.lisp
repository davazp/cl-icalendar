;; parameters.lisp -- Parameter list
;;
;; Copyrigth (C) 2012 David Vázquez
;;
;; This file is part of cl-icalendar.
;;
;; cl-icalendar is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-icalendar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-icalendar.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-icalendar)

;;; A parameter list is a list of the form
;;;
;;;    (PARAM-NAME1 PARAM-VALUE1 PARAM-NAME2 PARAM-NAME2 ...)
;;;
;;; PARAM-NAMEs are upcased strings. PARAM-VALUEs are strings or list
;;; of strings.

;;; Get the value of the parameter, whose name is given in the string
;;; designator PARAMETER, in the given PARAMETER-LIST.
(defun parameter (parameter parameter-list)
  (let ((name (string-upcase (string parameter))))
    (loop for (param value) on parameter-list by #'cddr
          when (string= name param) return value)))

#| See content-line.lisp about parameters reading and writting |#

;; parameters.lisp ends here
