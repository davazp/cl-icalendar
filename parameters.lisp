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

(defun parameter-table (x)
  (etypecase x
    (list
     (make-parameter-table x))
    (parameter-table
     x)))

(defun make-parameter-table (&optional params)
  (let ((pt (make-instance 'parameter-table)))
    (loop for (param value) on params by #'cddr
          do (setf (parameter param pt) value)
          finally (return pt))))

(defgeneric parameter (parameter parameter-table)
  (:method (param table)
    (let ((param (string param))
          (table (%parameter-table table)))
      (and table (values-list (gethash param table))))))

(defgeneric (setf parameter) (new-value parameter parameter-table)
  (:method (new-value param parameter-table)
    (with-slots (table) parameter-table
      (let ((param (string param))
            (value
             (with-collect
               (dolist (value (mklist new-value))
                 (collect (string-upcase (string value)))))))
        (when (null table)
          (setf table (make-hash-table :test #'equalp)))
        (setf (gethash param table) value)))))

;; parameters.lisp ends here
