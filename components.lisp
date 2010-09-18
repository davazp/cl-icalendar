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
(defclass component ()
  ((name
    :initarg :name
    :type string
    :accessor component-name)
   (%properties
    :type hash-table
    :initform (make-hash-table :test #'equalp)
    :accessor component-properties)
   (%subcomponents
    :type list
    :initform nil
    :accessor component-subcomponents)))

;;; Class for undefined components.
(defclass unknown-component (component)
  nil)

(defclass property ()
  ((name
    :type string
    :initarg :name
    :initform (required-arg)
    :reader property-name)
   ;; Parameter table of the property. In order to keep memory, this
   ;; could be NIL, specifying no parameters are avalaible. Likewise,
   ;; note NIL is a parameter-table designator accepted by the
   ;; `parameter-table' function.
   (parameters
    :type (or parameter-table null)
    :initarg :parameters
    :initform nil
    :accessor property-parameters)
   (value
    :initarg :value
    :type ical-value
    :accessor property-value)
   ;; Private slots
   (previous
    :type (or null property)
    :accessor %previous-property)
   (next
    :type (or null property)
    :accessor %next-property)))

(defun make-property (name parameters value)
  (make-instance 'property
                 :name name
                 :parameters parameters
                 :value value))


;;;; Generic-function component interface

(defgeneric add-property (component property-name values &optional parameters))
(defgeneric find-property (property-name component))
(defgeneric delete-property (property component))
(defgeneric count-property (component &optional property-name))

(defmethod add-property ((c component) pname values &optional params)
  (let ((pname (string pname))
        (ptable (component-properties c))
        (values (mklist values))
        (params (and params (parameter-table params))))
    (dolist (value values)
      (let* ((next (gethash pname ptable))
             (property (make-property pname params value)))
        (nilf (%previous-property property))
        (setf (%next-property property) next)
        (and next (setf (%previous-property next) property))
        (setf (gethash pname ptable) property)))))

(defmethod find-property (pname (c component))
  (values (gethash (string pname) (component-properties c))))

(defmethod delete-property (property (component component))
  (remhash (string property) (component-properties component)))

(defmethod delete-property ((property property) (component component))
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
    (values)))

(defmacro %do-property-1 ((property &key name) component &body code)
  (once-only (name component)
    `(do ((,property
           (gethash (string ,name) (component-properties ,component))
           (%next-property ,property)))
         ((null ,property))
       ,@code)))

(defmacro %do-property-all ((property) component &body code)
  (with-gensyms (key value)
    (once-only (component)
      `(let ((,component ,component))
         (do-hash-table (,key ,value)
             (component-properties ,component)
           (do-property (,property :name ,key)
               ,component
             ,@code))))))

(defmacro do-property ((property &key (name nil namep)) component &body code)
  (check-type property symbol)
  (if namep
      `(%do-property-1 (,property :name ,name) ,component
         ,@code)
      `(%do-property-all (,property) ,component
         ,@code)))

(defmacro do-subcomponents ((subcomponent) component &body code)
  `(dolist (,subcomponent (component-subcomponents ,component))
     ,@code))

(defmethod count-property ((component component) &optional pname)
  (let ((count 0))
    (if pname
        (do-property (property :name pname) component
          (incf count))
        (do-property (property) component
          (incf count)))
    count))


;;;; Compatibility CLOS Layer
;;;
;;; The component provides a thin abstraction about components and
;;; properties. However, it does not provide a pleasant abstraction to
;;; the user in order to handle them.
;;;
;;; Therefore, in order to provide that abstraction, we build a layer
;;; of compatibility upon CLOS, using the Meta-Object Protocol
;;; (MOP). The main result is the component-class metaobject, which is
;;; an extension to the default standard-class, that could be extended
;;; by the user too.
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

(defclass component-object (component)
  nil)

;;; Like make-instance. Return an instance of a class without
;;; initializing property-allocated slots.
(defvar *initializing-component* nil)
(defun make-uninitialized-component (class)
  (let ((*initializing-component* t))
    (make-instance class)))

;;; KLUDGE: Slot initialization order is undefined. However, we need
;;; to make sure COMPONENT and PROPERTIES slots are initialized before
;;; property-allocated slots. So, we initialize them with a :before
;;; initialize-instance method. This should work since :initform
;;; option are not applied if the slot is bound.
(defmethod initialize-instance :before ((inst component-object) &rest initargs)
  (declare (ignore initargs))
  (with-slots (%subcomponents %properties)
      inst
    (nilf %subcomponents)
    (setf %properties (make-hash-table :test #'equalp))))

;;; Metaclass of component classes
(defclass component-class (standard-class)
  (;; Components which are allowed to appear as subcomponents.
   (subcomponents
    :initarg :subcomponents
    :type list
    :initform nil
    :reader component-class-subcomponents)))

;;; Return the effective list of subcomponents allowed by the class.
(defgeneric compute-subcomponents (class))

(defmethod validate-superclass
    ((c component-class) (superclass standard-class))
  t)

(defprinter (class component-class)
  (prin1 (class-name class)))

;;; The following couple of routines define the default superclass for
;;; the component-class metaclass. They were written by Pascal
;;; Costanza and taken from
;;; http://www.cliki.net/MOP%20design%20patterns

(defmethod initialize-instance :around
  ((class component-class) &rest initargs &key direct-superclasses)
  (if (loop for class in direct-superclasses thereis (subtypep class 'component-object))
      ;; 'component-object is already one of the (indirect) superclasses
      (call-next-method)
      ;; 'component-object is not one of the superclasses, so we have to add it
      (apply #'call-next-method class
             :direct-superclasses (append direct-superclasses (list (find-class 'component-object))) initargs)))

(defmethod reinitialize-instance :around
    ((class component-class) &rest initargs &key (direct-superclasses '() direct-superclasses-p))
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
  (with-slots (table subcomponents) class
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
(defclass pdefinition (standard-slot-definition)
  (;; If the TYPE parameter in a property instance is not set, then we
   ;; consider it is a representation for a object of type
   ;; DEFAULT-TYPE.
   (default-type
    :initarg :default-type
    :reader pdefinition-default-type)
   ;; If it is T, then multiple-value is allowed. Otherwise, a error
   ;; is signaled if multiple values are set to the property.
   (multiple-value-p
    :initarg :multiple-value-p
    :type boolean
    :initform nil
    :reader pdefinition-multiple-value-p)
   ;; Can multiple-values be inlined in the same property?
   (inline-multiple-value-p
    :initarg :inline-multiple-value-p
    :type boolean
    :initform t
    :reader pdefinition-inline-multiple-value-p)
   ;; The default value if no property is specified.
   (default-value
    :type ical-value
    :initarg :default-value
    :reader pdefinition-default-value))
  (:default-initargs :allocation :property))

(defmethod initialize-instance :after
    ((pdefinition pdefinition) &rest initargs &key (type nil typep))
  (declare (ignore initargs type))
  ;; Default-type is the effective type by default.
  (unless (slot-boundp pdefinition 'default-type)
    (setf (slot-value pdefinition 'default-type)
          (slot-definition-type pdefinition)))
  ;; Do some error-checking in order to validate the property definition.
  (unless typep
    (error "The :type option must be specified for the slot ~a."
           (slot-definition-name pdefinition)))
  (when (and (slot-boundp pdefinition 'default-value)
             (not (typep (pdefinition-default-value pdefinition)
                         (slot-definition-type pdefinition))))
    (error "The type of the :default-value option of the property ~a is wrong."
           (slot-definition-name pdefinition)))
  (when (and (slot-boundp pdefinition 'default-type)
             (not (subtypep (pdefinition-default-type pdefinition)
                            (slot-definition-type pdefinition))))
    (error "The default type of the slot ~a must be a subtype of its type."
           (slot-definition-name pdefinition)))
  (unless (subtypep (slot-definition-type pdefinition) 'ical-value)
    (error "The type of the slot ~a must be a subtype of ICAL-VALUE."
           (slot-definition-name pdefinition))))

(defclass direct-pdefinition
    (pdefinition standard-direct-slot-definition)
  nil)

(defclass effective-pdefinition
    (pdefinition standard-effective-slot-definition)
  nil)

(defmethod direct-slot-definition-class
    ((x component-class) &rest initargs &key (allocation :property) &allow-other-keys)
  (declare (ignore initargs))
  (if (eq allocation :property)
      (find-class 'direct-pdefinition)
      (call-next-method)))

(defmethod effective-slot-definition-class
    ((x component-class) &rest initargs &key (allocation :property) &allow-other-keys)
  (declare (ignore initargs))
  (if (eq allocation :property)
      (find-class 'effective-pdefinition)
      (call-next-method)))

(defmethod compute-effective-slot-definition ((c component-class) name dslots)
  (let ((eslot (call-next-method))
        (dslot (find name dslots :key #'slot-definition-name)))
    (when (typep eslot 'effective-pdefinition)
      (flet (;; Copy from the class TO to the class FROM the slot SLOT.
             (copy-slot (slot)
               (when (slot-boundp dslot slot)
                 (setf (slot-value eslot slot)
                       (slot-value dslot slot)))))
        (copy-slot 'default-type)
        (copy-slot 'multiple-value-p)
        (copy-slot 'inline-multiple-value-p)
        (copy-slot 'default-value)))
    eslot))


;;;; Specialize the four operations on CLOS slots

(defmethod slot-value-using-class
    ((class component-class)
     (instance component-object)
     (prop effective-pdefinition))
  (with-collectors (values)
    (do-property (prop :name (slot-definition-name prop)) instance
      (collect-values (property-value prop)))
    (if (null values)
        (pdefinition-default-value prop)
        (if (pdefinition-multiple-value-p prop)
            values
            (car values)))))

(defmethod (setf slot-value-using-class)
    (new-value
     (class component-class)
     (instance component-object)
     (prop effective-pdefinition))
  (unless *initializing-component*
    ;; Add the property to the property table of the component.
    ;; IDEA: default parameters by property?
    (let ((name (slot-definition-name prop)))
      (delete-property name instance)
      (add-property instance name new-value))))

(defmethod slot-boundp-using-class
    ((class component-class)
     (object component-object)
     (slotd effective-pdefinition))
  (or (not (zerop (count-property object (slot-definition-name slotd))))
      (slot-boundp slotd 'default-value)))

(defmethod slot-makunbound-using-class
    ((class component-class)
     (object component-object)
     (slotd effective-pdefinition))
  (delete-property (slot-definition-name slotd) object))


;;; Like `defclass', but the metaclass must be a subclass of
;;; component-class, which is, indeed, the default metaclass.
(defmacro defcomponent (name super-components slots &rest options)
  (check-type name symbol)
  (check-type super-components list)
  (check-type slots list)
  (let ((metaclass (second (assoc :metaclass options))))
    (cond
      (metaclass
       (unless (subclassp metaclass 'component-class)
         (error "The :metaclass option must specify a submetaclass of component-class."))
       (let ((not-found (gensym))
             (default-initargs
              (cdr (find :default-initargs options :key #'first)))
             (other-options
              (remove :default-initargs options :key #'first)))
         ;; If the :name slot is not specified in the default-initargs
         ;; option, then we add it correctly.
         (when (eq (getf default-initargs :name not-found) not-found)
           (setf default-initargs (list* :name (string name) default-initargs)))
         `(progn
            (setf (translate ',name :component)
                  (defclass ,name ,super-components
                    ,slots
                    (:default-initargs ,@default-initargs)
                    ,@other-options)))))
      (t
       `(defcomponent ,name ,super-components
          ,slots
          (:metaclass component-class)
          ,@options)))))


;;;; Components' input and ouptut

(defmacro do-property-slot ((slot class) &body body)
  (check-type slot symbol)
  `(dolist (,slot (class-slots ,class))
     (when (typep ,slot 'effective-pdefinition)
       ,@body)))

(defun read-component-header (stream)
  (multiple-value-bind (begin-mark params component-name)
      (read-content-line stream)
    (unless (string-ci= "BEGIN" begin-mark)
      (%parse-error "A BEGIN:<COMPONENT-NAME> was expected, but '~a' was found."
                    (write-content-line-to-string begin-mark params component-name)))
    component-name))

(defun finalize-read-component (component)
  (do-property-slot (slot (class-of component))
    (let ((slot-name (slot-definition-name slot))
          (slot-type (slot-definition-type slot))
          (default-type (pdefinition-default-type slot)))
      (do-property (prop :name slot-name) component
        (with-slots ((params parameters) value) prop
          (let (explicit-type
                effective-type)
            (when (typep value 'unknown-value)
              (when params
                (aif (parameter-values "VALUE" params)
                     (setf explicit-type (translate it :type))
                     (setf explicit-type nil)))
              (when (and explicit-type (not (subtypep explicit-type slot-type)))
                (%parse-error "explicit type is not a subtype of the type."))
              (setf effective-type (or explicit-type default-type))
              (let* ((literal (unknown-value-string value)))
                (setf value (parse-value literal effective-type params))))))))))

;;; TODO: More error-checking!
(defun read-component-1 (component-name stream vendor)
  (multiple-value-bind (class foundp)
      (let ((*vendor* vendor))
        (translate component-name :component))
    (let* ((component-class (if foundp class (find-class 'unknown-component)))
           (component (make-uninitialized-component component-class)))
      (setf (component-name component) component-name)
      ;; Read properties and fill the component.
      (loop for cl = (multiple-value-list (read-content-line stream))
            for (cl-name cl-params cl-value) = cl
            for value = (make-unknown-value cl-value)
            until (string-ci= cl-name "END")
            if (string-ci= cl-name "BEGIN") do
               (push (read-component-1 cl-value stream vendor)
                     (component-subcomponents component))
            else do
               (add-property component cl-name value cl-params)
            finally
            (unless (string-ci= component-name cl-value)
              (%parse-error "...")))
      ;; Parse property-allocated slot values.
      (finalize-read-component component)
      component)))

(defun read-component (stream &optional (vendor *vendor*))
  (let ((cname (read-component-header stream)))
    (read-component-1 cname stream vendor)))


;;; The generic function write-component is the entry-point to a set
;;; of generic-functions which make up a simple protocol. This allows
;;; the on-the-fly generation of iCalendar data, without need of keep
;;; all subcomponents at same time in memory.

(defgeneric write-component (component stream))
(defgeneric write-component-properties (component stream))
(defgeneric write-component-subcomponents (component stream))

(defmethod write-component ((component component) stream)
  (let ((cname (component-name component)))
    (write-content-line "BEGIN" nil cname stream)
    (write-component-properties component stream)
    (write-component-subcomponents component stream)
    (write-content-line "END" nil cname stream)))

(defmethod write-component-properties ((component component) stream)
  (let ((written (make-hash-table :test #'eq)))
    (flet (;; Mark a property as written.
           (mark (x) (setf (gethash x written) t))
           ;; Test if a property has been written.
           (writtenp (x) (values (gethash x written))))
      ;; Write some special known properties.
      (do-property-slot (slot (class-of component))
        (when (and (pdefinition-multiple-value-p slot)
                   (pdefinition-inline-multiple-value-p slot))
          (with-collectors (values)
            (do-property (prop :name (slot-definition-name slot)) component
              (when (null (property-parameters prop))
                (mark prop)
                (collect-values (property-value prop))))
            (unless (null values)
              (let ((name (slot-definition-name slot))
                    (value (format-values values)))
                (write-content-line name nil value stream))))))
      ;; Write the rest of properties
      (do-property (prop) component
        (with-slots (name parameters value) prop
          (unless (writtenp prop)
            (let ((value (format-value value parameters)))
              (write-content-line name parameters value stream))))))))

(defmethod write-component-subcomponents ((component component) stream)
  (do-subcomponents (subc) component
    (write-component subc stream)))

;;; components.ends here
