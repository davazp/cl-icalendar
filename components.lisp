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
    :accessor component-subcomponents)))

(defclass property ()
  ((name
    :type string
    :initarg :name
    :initform (required-arg)
    :reader property-name)
   (parameters
    :type list
    :initarg :parameters
    :initform nil
    :accessor property-parameters)
   (value
    :initarg :value
    :type ical-value
    :accessor property-value)
   (previous
    :type (or null property)
    :accessor %previous-property)
   (next
    :type (or null property)
    :accessor %next-property)))

(defgeneric add-property (component property-name values &rest parameters)
  (:method ((c component-object) pname values &rest params)
    (let ((pname (string pname))
          (ptable (component-properties c))
          (values (mklist values))
          (strparams (mapcar #'string params)))
      (dolist (value values)
        (let* ((next (gethash pname ptable))
               (property
                (make-instance 'property
                               :name pname
                               :parameters strparams
                               :value (format-value value))))
          (nilf (%previous-property property))
          (setf (%next-property property) next)
          (and next (setf (%previous-property next) property))
          (setf (gethash pname ptable) property))))))

(defgeneric find-property (property-name component)
  (:method (pname (c component-object))
    (values (gethash (string pname) (component-properties c)))))

(defgeneric delete-property (property component)
  (:method ((property symbol) (component component-object))
    (remhash (string property) (component-properties component)))
  (:method ((property property) (component component-object))
    (with-slots (name previous next) property
      (cond
        (previous
         (setf (%next-property previous) next)
         (setf (%previous-property next) previous))
        (t
         (let ((ptable (component-properties component)))
           (when next
             (nilf (%previous-property next)))
           (setf (gethash name ptable) next))))
      (nilf previous next)
      (values))))

(defmacro %do-property-1 ((property &key name) component &body code)
  (once-only (name component)
    `(do ((,property
           (gethash (string ,name) (component-properties ,component))
           (%next-property ,property)))
         ((null ,property))
       ,@code)))

(defmacro %do-property-all ((property) component &body code)
  (with-gensyms (iterator morep key)
    (once-only (component)
      `(let ((,component ,component))
         (with-hash-table-iterator (,iterator (component-properties ,component))
           (loop
            (multiple-value-bind (,morep ,key)
                (,iterator)
              (unless ,morep (return))
              (do-property (,property :name ,key)
                  ,component
                ,@code))))))))

(defmacro do-property ((property &key (name nil namep)) component &body code)
  (check-type property symbol)
  (if namep
      `(%do-property-1 (,property :name ,name) ,component
         ,@code)
      `(%do-property-all (,property) ,component
         ,@code)))

(defgeneric count-property (component &optional property-name)
  (:method ((component component-object) &optional pname)
    (let ((count 0))
      (if pname
          (do-property (property :name pname) component
            (incf count))
          (do-property (property) component
            (incf count)))
      count)))


;;;; Compatibility CLOS Layer
;;; 
;;; The component-object provides a close abstraction to the described
;;; one in the RFC5545 document about components and; properties.
;;; However, it does not provide a pleasant abstraction to the user in
;;; order to handle them.
;;;
;;; Thereforce, in order to provide that abstraction, we build a layer
;;; of compatibility upon CLOS, using the Meta-Object Protocol
;;; (MOP). The main result is the component-class metaobject, which is
;;; an extension to the default standard-class, which could be
;;; extended by the user too.
;;; 
;;; The classes which are instances of component-class are always
;;; subclasses of component-object. A new allocation method named
;;; :property is avalaible for the slots; in fact, this is the default
;;; allocation method for component-class' instances. The user could
;;; override it to the standard :instance or :class methods yet. There
;;; some special options for property-allocated slots. See
;;; property-definition class.
;;;
;;; The usual operations on a slot have special effects on
;;; property-allocated slots, modifying the property table of the
;;; component in a hopefully intuitive and predictible way. Indeed the
;;; property-allocated slots know about property metainformation as
;;; type, default-type, single and multiple-valued properties, so they
;;; do more intensive error-checking to help to create well-formed
;;; iCalendar objects.

;;; Metaclass of component classes
(defclass component-class (standard-class)
  ((subcomponents
    :initarg :subcomponents
    :type list
    :initform nil
    :reader component-class-subcomponents)))

;; Return the effective list of subcomponents allowed by the class.
(defgeneric compute-subcomponents (class))

(defmethod validate-superclass
    ((c component-class) (superclass standard-class))
  t)

(defmethod print-object ((class component-class) stream)
  (print-unreadable-object (class stream :type t)
    (write (class-name class) :stream stream)))

;;; The following couple of routines define the default superclass for
;;; the component-class metaclass. They were written by Pascal
;;; Costanza and taken from
;;; http://www.cliki.net/MOP%20design%20patterns
(defmethod initialize-instance :around
  ((class component-class) &rest initargs &key direct-superclasses)
  ;(declare (dynamic-extent initargs))
  (if (loop for class in direct-superclasses thereis (subtypep class 'component-object))
      ;; 'component-object is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'component-object is not one of the superclasses, so we have to add it
      (apply #'call-next-method class
             :direct-superclasses (append direct-superclasses (list (find-class 'component-object))) initargs)))

(defmethod reinitialize-instance :around
    ((class component-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
  ;(declare (dynamic-extent initargs))
  (if direct-superclasses-p
      ;; if direct superclasses are explicitly passed this is exactly
      ;; like above
      (if (loop for class in direct-superclasses thereis (subtypep class 'component-object))
          (call-next-method)
          (apply #'call-next-method class
                 :direct-superclasses (append direct-superclasses (list (find-class 'component-object))) initargs))
      ;; if direct superclasses are not explicitly passed
      ;; we _must_ not change anything
      (call-next-method)))

(defmethod finalize-inheritance :after ((class component-class))
  (with-slots (subcomponents) class
    (setf subcomponents (compute-subcomponents class))))

(defmethod compute-subcomponents ((class component-class))
  ;; Compute the effective list of subcomponents allowed by the class.
  ;; It is computed appending the inherited subcomponents from
  ;; superclasses to the :subcomponents class' option.
  (with-slots (subcomponents) class
    (let ((effective-subcomponents subcomponents))
      (dolist (c (class-direct-superclasses class))
        (when (typep c 'component-class)
          (nconc effective-subcomponents
                 ;; FIXME: SBCL 1.0.39 signals an error if we use the
                 ;; reader component-class-subcomponents here, when
                 ;; the system try to finalize
                 ;; standard-component-object, as result of redefine
                 ;; component-class.
                 (slot-value c 'subcomponents)
                 ;;(component-class-subcomponents c)
                 )))
      effective-subcomponents)))


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
    :initform nil
    :reader property-definition-multiple-value))
  (:default-initargs :allocation :property))

(defmethod initialize-instance :after ((pdefinition property-definition) &rest initargs)
  (declare (ignore initargs))
  ;; Do some error-checking in order to validate the property definition.
  (unless (subtypep (property-definition-default-type pdefinition)
                    (slot-definition-type pdefinition))
    (error "The type of the slot ~a must be a subtype of ICAL-VALUE."
           (slot-definition-name pdefinition)))
  (unless (subtypep (slot-definition-type pdefinition) 'ical-value)
    (error "The default type of the slot ~a must be a subtype of its type."
           (slot-definition-name pdefinition))))

(defclass direct-property-definition
    (property-definition standard-direct-slot-definition)
  nil)

(defclass effective-property-definition
    (property-definition standard-effective-slot-definition)
  ;; The value processed by the the :initform option is stored in this
  ;; slot, since as we use a :property allocation,
  ;; slot-value-using-class cannot be used.
  ((initform-value :accessor initform-value)))

(defmethod direct-slot-definition-class
    ((x component-class) &rest initargs &key (allocation :property) &allow-other-keys)
  (declare (ignore initargs))
  (if (eq allocation :property)
      (find-class 'direct-property-definition)
      (call-next-method)))

(defmethod effective-slot-definition-class
    ((x component-class) &rest initargs &key (allocation :property) &allow-other-keys)
  (declare (ignore initargs))
  (if (eq allocation :property)
      (find-class 'effective-property-definition)
      (call-next-method)))

;;;; Specialize the four operations on CLOS slots

;;; It is T when we are processing a :initform, NIL otherwise.
;;; It is used to initialize the slots without create the property in the component.
(defvar *initializing-property-slot-p* nil)

(defmethod shared-initialize :around ((inst component-object) slot-names &rest initargs)
  (declare (ignore slot-names initargs))
  (let ((*initializing-property-slot-p* t))
    (call-next-method)))

(defmethod slot-value-using-class
    ((class component-class)
     (instance component-object)
     (prop effective-property-definition))
  (let ((values
         (with-collect
           (do-property (prop :name (slot-definition-name prop))
               instance
             (collect (property-value prop))))))
    (if (zerop (length values))
        ;; There are not properties. So we use call-next-method in
        ;; order to signal an error, or return the :initform if it is
        ;; present.
        (initform-value prop)
        (if (property-definition-multiple-value prop)
            values
            (car values)))))

(defmethod (setf slot-value-using-class)
    (new-value
     (class component-class)
     (instance component-object)
     (prop effective-property-definition))
  (if *initializing-property-slot-p*
      ;; We are processing the initialization of a slot by a :initform
      ;; option. If the property is multiple-valued, then we make sure
      ;; it is a list object.
      (if (property-definition-multiple-value prop)
          (setf (initform-value prop) (mklist new-value))
          (setf (initform-value prop) new-value))
      ;; Add the property to the property table of the component.
      ;; IDEA: default parameters by property?
      (let ((name (slot-definition-name prop)))
        (delete-property name instance)
        (add-property instance name new-value))))

(defmethod slot-boundp-using-class
    ((class component-class)
     (object component-object)
     (slotd effective-property-definition))
  (or (not (zerop (count-property object (slot-definition-name slotd))))
      ;; Maybe the slot was initialized by the :initform. This value
      ;; is stored in initform-value slot, so we check if it is bound.
      (slot-boundp slotd 'initform-value)))

(defmethod slot-makunbound-using-class
    ((class component-class)
     (object component-object)
     (slotd effective-property-definition))
  (delete-property (slot-definition-name slotd) object)
  (slot-makunbound slotd 'initform-value))


;;; Like `defclass', but the metaclass must be a subclass of
;;; component-class, which is, indeed, the default metaclass.
(defmacro defcomponent (name super-components slots &rest options)
  (let ((metaclass (second (assoc :metaclass options))))
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
