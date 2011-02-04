;; parameters.lisp -- Parameter's table
;;
;; Copyrigth (C) 2010,2011 David Vázquez
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

;;; Parameter-table class. Indeed, we provide the ability of use lists
;;; as parameter-table designators.
(defclass parameter-table ()
  ((table
    :initform (make-hash-table :test #'equalp)
    :reader %parameter-table)))

(defun set-parameter (parameter parameter-table new-value)
  (let ((param (string parameter))
        (value (mklist new-value))
        (table (%parameter-table parameter-table)))
    (setf (gethash param table) value)))

(defun make-parameter-table (&optional params)
  (let ((pt (make-instance 'parameter-table)))
    (loop for (param value) on params by #'cddr
          do (set-parameter param pt value)
          finally (return pt))))


;;; Public functions

;;; Return the parameter-table designated by X.
(defun parameter-table (x)
  (etypecase x
    (list
     (make-parameter-table x))
    (parameter-table
     x)))

;;; Check if X is a parameter table object.
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

;;; Return as multiple values the list of values of PARAMETER in the
;;; parameter-table designated by the object X.
(defun parameter (parameter x)
  (let ((parameter-table (parameter-table x)))
    (let ((parameter (string parameter))
          (table (%parameter-table parameter-table)))
      (values-list (gethash parameter table)))))

;; parameters.lisp ends here
