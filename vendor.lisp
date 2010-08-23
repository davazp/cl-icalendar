;; vendor.lisp
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

;;; A vendor identifies an use of the iCalendar library. This is
;;; intended to keep the extensions and deviations from the standard
;;; that the client application use. In particular, the vendor keeps a
;;; translation table from string to Lisp objects and specify as
;;; iCalendar objects are mapped in Lisp ones.
(defclass vendor ()
  ((name
    :initarg :name
    :type string
    :initform (required-arg)
    :reader vendor-name)
   (translate-table
    :type hash-table
    :initform (make-hash-table :test #'eq)
    :reader translate-table)))

(defvar *vendors*
  (make-hash-table :test #'eq))

(defmethod initialize-instance :after ((vendor vendor) &rest initargs)
  (declare (ignore initargs))
  (with-slots (name) vendor
    (string-upcase name)))

(defmethod print-object ((x vendor) stream)
  (print-unreadable-object (x stream :type t)
    (write-string (vendor-name x) stream)))

(defun create-vendor (name)
  (setf (gethash name *vendors*)
        (make-instance 'vendor :name (string name))))

(defun find-vendor (name)
  (values (gethash name *vendors*)))

;;; The default vendor. It is the default value of functions which
;;; require a vendor argument.
(defvar *vendor*
  (create-vendor 'standard-vendor))


(defun translate (entity kind)
  (let ((kindtable (gethash kind (translate-table *vendor*))))
    (if kindtable
        (gethash (string-upcase entity) kindtable)
        (values nil nil))))

(defun set-translate (entity kind value)
  (let ((table (translate-table *vendor*)))
    (let ((kindtable
           (or (gethash kind table)
               (setf (gethash kind table) (make-hash-table :test #'equal)))))
      (setf (gethash (string-upcase entity) kindtable) value))))

(defsetf translate (entity kind) (value)
  `(set-translate ,entity ,kind ,value))

;;; vendor.lisp ends here
