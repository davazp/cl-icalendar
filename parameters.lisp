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

(defun set-parameter (parameter parameter-table new-value)
  (with-slots (table) parameter-table
    (let ((param (string parameter))
          (value (mklist new-value)))
      (when (null table)
        (setf table (make-hash-table :test #'equalp)))
      (setf (gethash param table) value))))

(defun make-parameter-table (&optional params)
  (let ((pt (make-instance 'parameter-table)))
    (loop for (param value) on params by #'cddr
          do (set-parameter param pt value)
          finally (return pt))))


;;; Public functions

(defun parameter-table (x)
  (etypecase x
    (list
     (make-parameter-table x))
    (parameter-table
     x)))

(defun parameter-table-p (x)
  (typep x 'parameter-table))

(defun list-parameters (object)
  (let ((parameter-table (parameter-table object)))
    (if (null parameter-table)
        nil
        (with-collect
          (let ((table (%parameter-table parameter-table)))
            (if (hash-table-p table)
                (do-hash-table (key value)
                    table
                  (collect key)
                  (collect value))
                nil))))))

(defun parameter (parameter x)
  (let ((parameter-table (parameter-table x)))
    (let ((parameter (string parameter))
          (table (%parameter-table parameter-table)))
      (and table (values (gethash parameter table))))))

(defun parameter-values (parameter parameter-table)
  (values-list (parameter parameter parameter-table)))


;; parameters.lisp ends here
