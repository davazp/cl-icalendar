;; parameters.lisp -- Handle property and value's parameters
;;
;; Copyrigth (C) 2010 David Vázquez
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

(defclass parameter-table ()
  ((table
    :initform nil
    :type (or hash-table null)
    :accessor %parameter-table)))

(defun make-parameter-table ()
  (make-instance 'parameter-table))

(defgeneric parameter (parameter parameter-table)
  (:method (param table)
    (let ((param (string param))
          (table (%parameter-table table)))
      (values (and table (gethash param table))))))

(defgeneric (setf parameter) (new-value parameter parameter-table)
  (:method (new-value param table)
    (let ((param (string param))
          (table (%parameter-table table))
          (value (string new-value)))
      (when (null table)
        (setf (%parameter-table table) (make-hash-table :test #'equalp)))
      (setf (gethash param table) value))))

;;; Utility function. Check if the value of PARAM if VALUE in
;;; PARAM-TABLE. If PARAM-TABLE is NIL, return NIL.
(defun parameter= (param param-table value)
  (if (null param-table)
      nil
      (string-ci= (parameter param param-table) (string value))))

;; parameters.lisp ends here
