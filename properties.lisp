;; properties.lisp --
;;
;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010 David Vázquez
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

;;;; Property definition

(defclass property-definition ()
  ((name
    :initform (required-arg)
    :type symbol
    :initarg :name
    :reader property-definition-name)
   (x-property-p
    :type boolean
    :initform nil
    :reader property-definition-x-property-p)
   (type
    :initform (required-arg)
    :initarg :type
    :reader property-definition-type)
   (default-type
    :initform nil
    :initarg :default-type
    :reader property-definition-default-type)
   (required
    :initform nil
    :initarg :required
    :reader property-definition-required-p)
   ;; Default value if property is not specified.
   (default
    :initarg :default
    :reader property-definition-default)
   ;; Number of times which the property must be specified.
   (count
    :initform 1
    :type (or null (integer 1 *))
    :initarg :count
    :reader property-definition-count)
   (multiple-instance-p
    :initform nil
    :type boolean
    :initarg :multiple-instance-p
    :reader property-definition-multiple-instance-p)
   (parameter-list
    :initform nil
    :type list
    :initarg :parameter-list
    :reader property-definition-parameter-list)
   (allow-x-parameters-p
    :initform nil
    :type boolean
    :initarg :allow-x-parameter-p
    :reader property-definition-allow-x-parameters-p)))

(defmethod initialize-instance :after
    ((prop property-definition) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  ;; Signal an error if the value of the :type property option is not
  ;; a subtype of ical-value.
  (unless (subtypep (property-definition-type prop) 'ical-value)
    (error "The type of the property ~a is not a subtype of ~a."
           (property-definition-name prop)
           'ical-value)))

(defmethod print-object ((object property-definition) stream)
  (print-unreadable-object (object stream :type t)
    (princ (property-definition-name object) stream)))

(defun make-property-from-definition (definition)
  (declare (list definition))
  (let ((property-name (car definition))
        (property-options (cdr definition)))
    (apply #'make-instance 'property-definition
           :name property-name
           property-options)))


;;;; Properties

(defclass property ()
  ((class
    :initform (required-arg)
    :initarg :class
    :reader property-class)
   (parameters
    :initform nil
    :initarg :parameters
    :accessor property-parameters)
   (value
    :initarg :value
    :accessor property-value)))

;;; Signal a type-error if the type of value is wrong.
(defmethod (setf property-value) :around (value (prop property))
  (let ((prop-type (property-definition-type (property-class prop))))
    (if (typep value prop-type)
        (call-next-method)
        (error 'type-error
               :expected-type prop-type
               :datum value))))

(defun make-property (class)
  (declare (property-definition class))
  (make-instance 'property :class class))

;; properties.lisp ends here
