;;; property.lisp --- Properties
;;
;; Copyright (C) 2012 David Vazquez
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

(defun find-property-class (name)
  (translate-to-lisp name :property))

;;;; Property protocol

;;; Make an uninitialized instance of a property, with the given
;;; PARAMETERS. The value of the property can be initialized with the
;;; generic function `initialize-property'.
(defgeneric allocate-property (property-class parameters))

;;; Return a single iCalendar type. Initializing a property with a
;;; property value which belongs this type, is granted to validate.
(defgeneric allocated-property-type (property))

;;; Initialize the value of an allocated property to VALUE. This value
;;; must satisfy the type returned by `allocated-proprety-type'.
(defgeneric initialize-property (property value))

;;; Make an instance of a property. PROPERTY can be a property class,
;;; or a string. If it is a string, the property class whose name is
;;; this string is considered. PARAMETERS is a list of parameters as
;;; described in parameters.lisp. VALUE is an iCalendar value.
(defgeneric make-property (property parameters value)
  (:method (property-class parameters value)
    (let ((prop (allocate-property property-class parameters)))
      (initialize-property prop value)
      (validate-property prop)
      prop))
  (:method ((property string) parameters value)
    (let ((property-class (find-property-class property)))
      (make-property property-class parameters value))))


;;; Make a property instance from a content line.
(defun property-from-content-line (property-name parameters string)
  (let* ((property-class (find-property-class property-name))
         (property (allocate-property property-class parameters))
         (type (allocated-property-type property))
         (value (unlist (parse-values string type parameters))))
    (initialize-property property value)
    (validate-property property)
    property))

(defun write-property* (property stream)
  (let ((value (property-value property))
        (type (allocated-property-type property)))
    (write-content-line* (property-name property)
                         (property-parameters property)
                         (format-values (mklist value) type)
                         stream)))

;;; Write PROPERTY in a content line to STREAM.
(defun write-property (property stream)
  (write-property* property stream)
  (terpri stream))

;;;; Property validation protocol

;;; Validate the property. The default method validate the parameters
;;; with `validate-property-parameters' and the value with
;;; `validate-property-value'.
(defgeneric validate-property (property))

;;; Validate the value of PROPERTY. The default method checks that the
;;; value of the property satisfies the iCalendar type returned by the
;;; generic function `allocated-property-type'.
(defgeneric validate-property-value (property))

;;; Validate the parameters of property. The default method validates
;;; each parameter calling `validate-property-parameter'.
(defgeneric validate-property-parameters (property))

;;; Validate a parameter in a property. The default method uses the
;;;`property-validated-parameters', `property-allow-x-parameters-p'
;;; and `property-allow-other-parameters-p' generic functions to get
;;; information about the validation.
(defgeneric validate-property-parameter (property parameter-name parameter-value))

;; Return a list of string, which are the names of the valid
;; parameters for instances of this property.
(defgeneric property-validated-parameters (property)
  (:method-combination append))

;; If it returns NIL, the property does not allow x-parameters, so it
;; will not validate if one is present.
(defgeneric property-allow-x-parameters-p (property))

;;; If it is NIL, the property class does not allow unknown
;;; parameters, so it will not validate if one is present.
(defgeneric property-allow-other-parameters-p (property))


;;; Base property class

(defclass property ()
  ((name
    :initarg :name
    :initform (required-arg)
    :type string
    :reader property-name)
   (parameters
    :initarg :parameters
    :type list
    :reader property-parameters)
   (value
    :initarg :value
    :reader property-value)))

(defmethod allocate-property (property-class parameters)
  (make-instance property-class :parameters parameters))

(defmethod initialize-property ((allocated-property property) value)
  (setf (slot-value allocated-property 'value) value))

(defmethod print-object ((prop property) stream)
  (print-unreadable-object (prop stream)
    (write-property* prop stream)))

(defmethod property-validated-parameters append ((x property))
  nil)

(defmethod property-allow-x-parameters-p ((x property))
  t)

(defmethod property-allow-other-parameters-p ((x property))
  nil)

(defmethod validate-property-value ((property property))
  (let ((value (property-value property))
        (type (allocated-property-type property)))
    (unless (typep value type)
      (error "The value ~a is not of type ~a" value type))))

(defmethod validate-property-parameter ((property property) parameter-name parameter-value)
  (let ((parameters (property-validated-parameters property)))
    (or (find parameter-name parameters :test #'string-ci=)
        (and (property-allow-x-parameters-p property)
             (string-prefix-p "X-" parameter-name))
        (property-allow-other-parameters-p property)
        (error "The parameter ~a with value ~a is not allowed in property ~a"
               parameter-name parameter-value (property-name property)))))

(defmethod validate-property-parameters ((prop property))
  (loop for (param-name param-value) on (property-parameters prop) by #'cddr
        do (validate-property-parameter prop param-name param-value)))

(defmethod validate-property ((prop property))
  (validate-property-parameters prop)
  (validate-property-value prop))


;;; Abstract properties classes


;;; Properties with a composed type
(defclass multiple-type-property (property)
  nil)

(defgeneric property-default-type (property))
(defgeneric property-types (property))

(defgeneric property-parameter-value (property)
  (:method ((property multiple-type-property))
    (let ((value-param (parameter "VALUE" (property-parameters property))))
      (if value-param
          (translate-to-lisp value-param :type)
          (property-default-type property)))))

(defmethod property-validated-parameters append ((x multiple-type-property))
  '("VALUE"))

(defmethod allocated-property-type ((property multiple-type-property))
  (property-parameter-value property))

(defmethod validate-property-parameters ((prop multiple-type-property))
  (let ((value-param (property-parameter-value prop))
        (validated-types (property-types prop)))
    (unless (find value-param validated-types)
      (error "The VALUE parameter was ~a, but one of ~a was expected."
             value-param validated-types)))
    (call-next-method))


;;; Properties which allow multiple values in the same property
(defclass multiple-value-property (property)
  nil)

(defmethod validate-property-value ((prop multiple-value-property))
  (let ((values (mklist (property-value prop)))
        (type (allocated-property-type prop)))
    (dolist (value values)
      (unless (typep value type)
        (error "The value ~a is not of type ~a" value type)))))


;;; Standard properties

(defmacro define-property (real-name &key
                           type default-type (multiple-value-p nil) (parameters nil)
                           (allow-x-parameters-p t) (allow-other-parameters-p nil))
  (check-type real-name string)
  (check-type multiple-value-p boolean)
  (check-type allow-x-parameters-p boolean)
  (check-type allow-other-parameters-p boolean)
  (let* ((superclasses '(property))
         (name (gensym "PROPERTY"))
         (types (mklist type))
         (multiple-type-p (< 1 (length types))))
    ;; Add some superclasses according to the options
    (when multiple-type-p
      (push 'multiple-type-property superclasses))    
    (when multiple-value-p
      (push 'multiple-value-property superclasses))
    ;; Expansion
    `(progn
       ;; Property class and registration
       (defclass ,name (,@superclasses) nil
         (:default-initargs :name ,real-name))
       (register-translation (find-class ',name) ,real-name :property)
       ;; Validation of parameters
       ,@(unless (null parameters)
                 `((defmethod property-validated-parameters append ((,(gensym) ,name))
                     ',parameters)))
       ;; Validation of types
       ,@(if multiple-type-p
             `((defmethod property-default-type ((,(gensym) ,name))
                 ',default-type)
               (defmethod property-types ((,(gensym) ,name))
                ',types))
            `((defmethod allocated-property-type ((,(gensym) ,name))
                ',(unlist types)))))))


;;; property.lisp ends here
