;; components.lisp --- iCalendar's component functionality
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

;;; Metaclass of component classes
(defclass component-class (standard-class)
  ((subcomponents
    :initarg :subcomponents
    :type list
    :initform nil
    :reader component-class-subcomponents)))

(defmethod validate-superclass ((c component-class) (superclass standard-class))
  t)

;;; A particular instance of a property, with a list of parameters and
;;; a list of associated values.
(defstruct property-impl
  parameters
  values)

;;; Superclass of all component classes. Therefore, the behaviour of
;;; this class is inherited by all them. We implement the property and
;;; subcomponents artillery here.
(defclass component-object ()
  ((properties
    :type hash-table
    :initform (make-hash-table :test #'equalp)
    :accessor component-properties)
   (subcomponents
    :type list
    :initform nil
    :accessor component-subcomponents))
  (:metaclass component-class))

(defmethod initialize-instance ((class component-class) &rest initargs
                                &key direct-superclasses &allow-other-keys)
  (let ((superclasses (copy-list direct-superclasses)))
    ;; If no specified spuerclasses of CLASS is a subclass of
    ;; component-object, then we add it to the list fo
    ;; superclasses. So, it seems as the default superclass.
    (unless (some (rcurry #'subclassp 'component-object) direct-superclasses)
      (nconc superclasses (list (find-class 'component-object))))
    (apply #'call-next-method class :direct-superclasses superclasses initargs)))


;;; Identify iCalendar properties with CL slots. So we integrate the
;;; system of components into CLOS. The slot value will be computed
;;; from the property-table present in the class.
;;; (The extensibility is wonderful)
(defclass property-definition (standard-slot-definition)
  (;; If the TYPE parameter in a property instance is not set, then we
   ;; consider it is a representation for a object of type
   ;; DEFAULT-TYPE.
   (default-type
    :initarg :type
    :reader property-definition-default-type)
   ;; Describe the number of values that a property can take. If it is
   ;; NIL, a unique value is allowed. If it is T, then multiple-value
   ;; is allowed. If it is an integer, then it is the exactly the
   ;; number of values that the property must take.
   (multiple-value
    :type (or boolean (integer 0 *))
    :reader property-definition-multiple-value)
   ;; The minimum number of properties which be belong to this
   ;; definition in a given component.
   (instances-min
    :type (integer 0 *)
    :reader property-definition-instances-min)
   ;; The maximum number of properties which be belong to this
   ;; definition in a given component.
   (instances-max
    :type (or null (integer 0 *))
    :reader property-definition-instances-max)))

(defclass direct-property-definition
    (property-definition standard-direct-slot-definition)
  nil)

(defclass effective-property-definition
    (property-definition standard-effective-slot-definition)
  nil)

(defmethod direct-slot-definition-class ((x component-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'direct-property-definition))

(defmethod effective-slot-definition-class ((x component-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'effective-property-definition))

(defmethod finalize-inheritance ((class component-class))
  (flet ((compute-subcomponents (class)
           ;; Return the effective list of subcomponents allowed by
           ;; the class. It is computed appending the inherited
           ;; ':subcomponents' option from superclasses to the
           ;; :subcomponents class's option.
           (with-slots (subcomponents) class
             (let ((effective-subcomponents subcomponents))
               (dolist (c (class-direct-superclasses class))
                 (when (subclassp (class-of c) (find-class 'component-class))
                   (nconc effective-subcomponents (component-class-subcomponents c))))
               effective-subcomponents))))
    (unless (class-finalized-p class)
      (setf (slot-value class 'subcomponents)
            (compute-subcomponents class)))
    (call-next-method)))



(defgeneric property-value (component property-name)
  (:method ((component component-object) property-name)
    ))

(defgeneric (setf property-value) (new-value component property-name)
  (:method (new-value (component component-object) property-name)
    nil))

(defgeneric property-param (component property-name parameter-name)
  (:method ((component component-object) property-name parameter-name)
    nil))

(defgeneric (setf property-param) (new-value component property-name parameter-name)
  (:method (new-value (component component-object) property-name parameter-name)
    nil))

(defmethod slot-value-using-class :around
    ((class component-class)
     (instance component-object)
     (prop effective-property-definition))
  (call-next-method))

(defmethod (setf slot-value-using-class) :around
    (new-value (class component-class)
               (instance component-object)
               (prop effective-property-definition))
  (call-next-method))


;;; Like `defclass', but the metaclass must be a subclass of
;;; component-class, which is, indeed, the default metaclass.
(defmacro defcomponent (name super-components slots &rest options)
  (let ((metaclass (cadr (assoc :metaclass options))))
    (cond
      (metaclass
       (unless (subclassp metaclass 'component-class)
         (error "The :metaclass option must specify a submetaclass of component-class."))
       `(defclass ,name ,super-components
          ,slots
          ,@options))
      (t
       `(defcomponent ,name ,super-components
          ,slots
          (:metaclass component-class)
          ,@options)))))


;;; components.ends here
