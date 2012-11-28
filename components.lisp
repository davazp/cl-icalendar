;; components.lisp --- iCalendar's component
;;
;; Copyrigth (C) 2012 David Vázquez
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

;;; Return the component class for the name COMPONENT-NAME.
(defun find-component-class (component-name)
  (or (translate-to-lisp component-name :component)
      (and (x-name-p component-name) (find-class 'x-component))))

;;; Base component.
(defclass component ()
  ((parent
    :initarg :parent
    :initform nil
    :reader component-parent)
   ;; A string which is the name of the component. This slot is
   ;; redundant because in standard components, we can do a reverse
   ;; translation with lisp-to-translate really.
   (name
    :initarg :name
    :initform (required-arg "Missing component name")
    :reader component-name)
   ;; Table of properties in the component. It is a hash table where
   ;; keys are property name strings and values are the list of
   ;; properties properties with that name.
   (property-table
    :initform (make-hash-table :test #'equalp)
    :reader property-table)
   ;; List of subcomponents in the property.
   (subcomponents
    :initform nil
    :accessor subcomponents)))

(defmacro do-property ((propertyvar component &optional result) &body body)
  "Iterate across the properties in a component."
  (with-gensyms (ptable key value)
    `(let ((,ptable (property-table ,component)))
       (do-hash-table (,key ,value) ,ptable
         ;; Do not use dolist here in order to keep visible the
         ;; implicit block of do-hash-table.
         (map nil (lambda (,propertyvar) ,@body) ,value)
         ,result))))

(defmacro do-components ((componentvar component &optional result) &body body)
  "Iterate across the subcomponents in a component."
  `(dolist (,componentvar (subcomponents ,component) ,result)
     ,@body))

;;;; Initialization protocol

(defgeneric allocate-component (component-class name)
  (:method (component-class name)
    (make-instance component-class :name name))
  (:documentation "Make an empty instance of a component class."))

(defgeneric initialize-component (component &rest initargs)
  (:method ((component component) &rest initargs)
    (declare (ignorable initargs)))
  (:documentation "Initialize a component with a set of convenient
  default properties and subcomponents."))

(defun make-component (name &rest initargs)
  "Make a instance of the component whose whose name is NAME."
  (let* ((component-class (find-component-class name))
         (component (allocate-component component-class name)))
    (apply #'initialize-component component initargs)
    component))


;;;; Subcomponents and properties protocol

;; Return the list of properties in component with name CLASS.
(defmacro properties-with-name (name component)
  `(gethash ,name (property-table ,component)))

(defgeneric add-property-to-component (property component)
  (:method ((property property) (component component))
    (let ((name (property-name property)))
      (push property (properties-with-name name component))
      property))
  (:documentation "Add PROPERTY to COMPONENT."))

(defgeneric delete-property-from-component (property component)
  (:method ((property property) (component component))
    (let ((pname (property-name property)))
      (symbol-macrolet ((plist (properties-with-name pname component)))
        (setf plist (delete property plist)))))
  (:documentation "Delete PROPERTY from COMPONENT."))

(defgeneric add-subcomponent-to-component (subcomponent component)
  (:method ((subcomponent component) (component component))
    (setf (slot-value subcomponent 'parent) component)
    (push subcomponent (subcomponents component))
    subcomponent)
  (:documentation "Add SUBCOMPONENT to COMPONENT."))

(defgeneric delete-subcomponent-from-component (subcomponent component)
  (:method ((subcomponent component) (component component))
    (setf (subcomponents component) (delete subcomponent (subcomponents component))))
  (:documentation "Delete SUBCOMPONENT from COMPONENT."))


;;; Validation protocol

(defgeneric validate-component (component)
  (:method ((component component))
    (do-property (prop component)
      (validate-property prop)
      (validate-property-in-component component prop))
    (do-components (comp component)
      (validate-component comp)
      (validate-subcomponent-in-component component comp))
    (validate-property-constrains component))
  (:documentation "Validate a component. By default, call to the
generic functions:

    o `validate-property-in-component'
    o `validate-property-constrains'
    o `validate-subcomponent-in-component'
"))

(defgeneric invalid-subcomponent (component subcomponent)
  (:method ((comp component) (subcomp component))
    (error "The component ~a is not a valid subcomponent for ~a" subcomp comp)))

(defgeneric invalid-property (component property)
  (:method ((comp component) (prop property))
    (error "The property ~a is not a valid in the component ~a" prop comp)))

(defgeneric validate-subcomponent-in-component (component subcomponent)
  (:method ((comp component) (subcomp component))
    (invalid-subcomponent comp subcomp))
  (:documentation "Validate SUBCOMPONENT as subcomponent of COMPONENT."))

(defgeneric validate-property-in-component (component property)
  (:method ((comp component) (prop property))
    (invalid-property comp prop))
  (:documentation "Validate PROPERTY as property of COMPONENT."))

(defgeneric validate-property-constrains (component)
  (:method ((comp component)) nil)
  (:documentation "Validate the constrains between different
properties in the same component. It is used, for example, to avoid
multiple instances of some properties in a component. But also to
implement more complicated constrains. See the `property-constrains'
macro facility."))


;;;; Standard components

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Return the property names in the constrain RULE.
  (defun property-constrains-names (rule)
    (cond
      ((stringp rule)
       (list rule))
      ((and (listp rule) (neq (car rule) 'quote))
       (delete-duplicates
        (reduce #'append (mapcar #'property-constrains-names rule))
        :test #'string-ci=))))
  ;; Replace the property names in the constrain RULE by the values in
  ;; the alist ENVIRONMENT. This function is internal.
  (defun replace-property-names-with-vars (environment rule)
    (cond
      ((stringp rule)
       (let ((var (cdr (assoc rule environment :test #'string-ci=))))
         (or var (error "Missing the  '~a' symbol in ~a" rule environment))))
      ((and (listp rule) (eq (car rule) 'quote) (stringp (cadr rule)))
       (cadr rule))
      ((and (listp rule) (neq (car rule) 'quote))
       (map1 #'replace-property-names-with-vars environment rule))
      (t
       rule))))

;;; Execute the property constrains in BODY against
;;; COMPONENT. Property constrains is like usual Lisp code, but
;;; literal strings are evaluated to the list of properties with the
;;; same name in COMPONENT, unless they are quoted. Also, useful local
;;; functions and macros are available.
(defmacro property-constrains (component &body body)
  (let* ((compvar (gensym "COMPONENT"))
         ;; OPTIMIZATION: The success of the constrain does not
         ;; depends on the toplevel strings. However they are useful
         ;; to specify more-than-once properties, though.
         (body (remove-if #'stringp body))
         (pnames (property-constrains-names body))
         (pvars (mapcar #'gensym pnames))
         (environment (mapcar #'cons pnames pvars)))
    `(let ((,compvar ,component))
       (declare (ignorable ,compvar))
       (let ,(loop for name in pnames
                   for var in pvars
                   collect `(,var (query-property ,compvar ,name t)))
         ;; Local macros for property constrain rules
         (macrolet ((name (property)
                      (car (rassoc property ',environment)))
                    (required (&rest properties)
                      (progn-expand (property properties)
                        `(if ,property
                             t
                             (error "The property `~a' is missing in the component ~a."
                                    (name ,property) ,',compvar))))
                    (once (&rest properties)
                      (progn-expand (property properties)
                        `(if (second ,property)
                             (error "The property `~a' cannot appear more than once in the component ~a."
                                    (name ,property) ,',compvar)
                             t)))
                    (required-once (&rest properties)
                      `(progn
                         (required ,@properties)
                         (once ,@properties))))
           ,@(replace-property-names-with-vars environment body))))))



(defclass standard-component (component)
  nil)

(defmethod validate-property-in-component
    ((component standard-component) (property x-property)))

;;; Extension components
(defclass x-component (component)
  nil)

(defmethod validate-subcomponent-in-component
    ((component x-component) subcomponent))

(defmethod validate-property-in-component
    ((component x-component) property))


;;; CLHS says: "If a defclass form appears as a top level form, the
;;; compiler must make the class name be recognized as a valid type
;;; name in..." So we have to define the classes separately.
(defmacro declare-component (name &optional abstract-components)
  `(defclass ,name (,@abstract-components standard-component) nil))

;;; Define a component.
(defmacro define-component (name options &body body)
  `(define-component-1 ,name
       ,@(loop for (option . value) in options append `(,option ,value))
     :properties ,body))

(defmacro define-component-1 (name &key
                              (icalname (string name))
                              (allow-x-components nil given-allow-x-components)
                              (allow-x-properties t given-allow-x-properties)
                              subcomponents
                              properties)
  `(progn
     (register-translation (find-class ',name) ,icalname :component)
     ;; Subcomponents validation
     ,@(if given-allow-x-components
           `((defmethod validate-subcomponent-in-component ((component ,name) (subcomponent x-component))
               ,(or allow-x-components `(invalid-subcomponent component subcomponent)))))
     ,@(loop for comp-class in subcomponents collect
                `(defmethod validate-subcomponent-in-component
                     ((comp ,name) (subcomp ,comp-class))))
     ;; Properties validation
     ,@(if given-allow-x-properties
           `((defmethod validate-property-in-component ((component ,name) (property x-property))
               ,(or allow-x-properties `(invalid-property component property)))))
     ;; Declare known properties
     ,@(loop for pname in (property-constrains-names properties)
             for property-class = (or (find-property-class pname) (error "Unknown property name ~a" pname))
             for property = (class-name property-class)
             collect
                `(defmethod validate-property-in-component
                     ((component ,name)
                      (property ,property))))
     ;; Property constrains
     (defmethod validate-property-constrains ((,name ,name))
       (property-constrains ,name
         ,@properties))))


;;; VALARM

(defun action (component)
  (let ((action (query-property component "ACTION")))
    (cdr (assoc action '(("AUDIO" . :audio)
                         ("EMAIL" . :email)
                         ("DISPLAY" . :display))
                :test #'string-ci=))))

(declare-component valarm)

(defun valarm-audio-constrains (component)
  (property-constrains component
    (required "ATTACH")))

(defun valarm-display-constrains (component)
  (property-constrains component
    (required "DESCRIPTION")))

(defun valarm-email-constrains (component)
  (property-constrains component
    (required "DESCRIPTION" "SUMMARY")))

(define-component valarm ()
  (required-once "ACTION" "TRIGGER")
  (unless (eql (and "DURATION" t) (and "REPEAT" t))
    (error '"If the DURATION or REPEAT property appears in the component ~a,
then the other must so." valarm))
  ;; Declare possible properties
  "ATTACH" "DESCRIPTION" "SUMMARY" "ATTENDEE"
  ;; Dispatch constrains according to the action
  (ecase (action valarm)
    (:audio
     (valarm-audio-constrains valarm))
    (:display
     (valarm-display-constrains valarm))
    (:email
     (valarm-email-constrains valarm))))


(defun generate-new-uuid ()
  (with-output-to-string (o)
    (print-object (uuid:make-v4-uuid) o)))

;;; VTODO
(declare-component vtodo)
(define-component vtodo
    ((:subcomponents valarm))
  (required "DTSTAMP" "UID")
  (once "CLASS" "COMPLETED" "CREATED" "DESCRIPTION" "DTSTART" "GEO"
        "LAST-MODIFIED" "LOCATION" "ORGANIZER" "PERCENT-COMPLETE" "PRIORITY"
        "RECURRENCE-ID" "SEQUENCE" "STATUS" "SUMMARY" "URL" "RRULE")
  (once "DUE" "DURATION")
  (when (and "DUE" "DURATION")
    (error '"DUE and DURATION properties MUST NOT occur in ~a at same time." vtodo))
  "ATTACH" "ATTENDEE" "CATEGORIES" "COMMENT"
  "CONTACT" "EXDATE" "REQUEST-STATUS" "RELATED-TO"
  "RESOURCES" "RDATE")


;;; VEVENT
(declare-component vevent)

(defprinter (event vevent)
  (let ((summary (query-property event "SUMMARY")))
    (if summary
        (prin1 (property-value summary))
        (write-string ":NO-SUMMARY"))))

(define-component vevent
    ((:subcomponents valarm))
  (required-once "DTSTAMP" "UID")
  (let ((parent (component-parent vevent)))
    (unless (or (null parent) (query-property parent '"METHOD"))
      (required "DTSTART")))
  (once "CLASS" "CREATED" "DESCRIPTION" "GEO" "LAST-MODIFIED"
        "LOCATION" "ORGANIZER" "PRIORITY" "SEQUENCE" "STATUS"
        "SUMMARY" "TRANSP" "URL" "RECURRENCE-ID" "RRULE")
  (once "DTEND" "DURATION")
  (when (and "DTEND" "DURATION")
    (error '"DTEND and DURATION properties MUST NOT occur in ~a at same time." vevent))
  "ATTACH" "ATTENDEE" "CATEGORIES" "COMMENT"
  "CONTACT" "EXDATE" "REQUEST-STATUS" "RELATED-TO"
  "RESOURCES" "RDATE")

(defmethod initialize-component ((component vevent) &key
                                 (uid (generate-new-uuid))
                                 (dtstamp (get-universal-time)))
  (add-new-property component "UID" nil uid)
  (add-new-property component "DTSTAMP" nil dtstamp))


;;; VJOURNAL
(declare-component vjournal)
(define-component vjournal ()
  (required-once "DTSTAMP" "UID")
  (once "CLASS" "CREATED" "DTSTART"
        "LAST-MODIFIED" "ORGANIZER" "RECURRENCE-ID" "SEQUENCE"
        "STATUS" "SUMMARY" "URL" "RRULE")

  "ATTACH" "ATTENDEE" "CATEGORIES" "COMMENT"
  "CONTACT" "DESCRIPTION" "EXDATE" "RELATED-TO" "RDATE"
  "REQUEST-STATUS")

;;; VFREEBUSY
(declare-component vfreebusy)
(define-component vfreebusy ()
  (required-once "DTSTAMP" "UID")
  (once "CONTACT" "DTSTART" "DTEND" "ORGANIZER" "URL")
  "ATTENDEE" "COMMENT" "FREEBUSY" "REQUEST-STATUS")


;;; VTIMEZONE
(declare-component vtimezone)
(declare-component standard)
(declare-component daylight)

(define-component standard ()
  (required-once "DTSTART" "TZOFFSETTO" "TZOFFSETFROM")
  (once "RRULE")
  "COMMENT" "RDATE" "TZNAME")

(define-component daylight ()
  (required-once "DTSTART" "TZOFFSETTO" "TZOFFSETFROM")
  (once "RRULE")
  "COMMENT" "RDATE" "TZNAME")

(define-component vtimezone
    ((:subcomponents standard daylight))
  (required-once "TZID")
  (once "LAST-MODIFIED" "TZURL"))

;;; VCALENDAR

(defclass vcalendar (standard-component)
  ((uid-table
    :initform (make-hash-table :test #'equalp)
    :reader uid-table)))

(define-component vcalendar
    ((:allow-x-components . t)
     (:subcomponents vtodo vevent vjournal vfreebusy vtimezone))
  (required "PRODID" "VERSION")
  (once "CALSCALE" "METHOD"))

(defmethod add-subcomponent-to-component :after
    ((event vevent) (component vcalendar))
  (let ((uid (query-property event "UID")))
    (when uid
      (setf (gethash (property-value uid) (uid-table component)) event))))


(defmethod initialize-component ((component vcalendar) &key
                                 (version "2.0")
                                 (prodid "-//cl-icalendar v0.0//EN")
                                 (method "PUBLISH"))
  (add-new-property component "PRODID" nil prodid)
  (add-new-property component "VERSION" nil version)
  (when method
    (add-new-property component "METHOD" nil method)))

(defun read-vcalendar (stream)
  (let ((component (read-component stream)))
    (check-type component vcalendar)
    component))

;;; Other components

(defclass unknown-component (component) nil)

(defmethod validate-component ((x unknown-component)))

(defmethod allocate-component ((component-class null) name)
  (make-instance 'unknown-component :name name))


;;;; Aplication programmer Interface (API)

(defun add-property (component name parameters value)
  "Add the property named NAME, with the given PARAMETERS and VALUE to
the list of properties of COMPONENT."
  (let ((property (make-property name parameters value)))
    (add-property-to-component property component)))

(defun add-new-property (component name parameters value)
  "Like `add-property', but only add the property if there is not any
property with the same name in the component already."
  (unless (query-property component name)
    (add-property component name parameters value)))

(defun query-property (component property-name &optional all-p)
  "Return a property of COMPONENT with name PROPRETY-NAME. If ALL-P is
present, return the list of all the properties with this name
instead. The behaviour is undefined if the returned list is modified
destructively."
  (let ((list (gethash property-name (property-table component))))
    (if all-p list (first list))))

(defun delete-property (component property)
  "Delete PROPERTY of the table of properties of COMPONENT."
  (delete-property-from-component property component))

(defun read-object (stream)
  (multiple-value-bind (name params value) (read-content-line stream)
    (cond
      ((string-ci= name "BEGIN")
       (check-type params null)
       (let* ((component-class (find-component-class value))
              (component (allocate-component component-class value)))
         (loop with begin-mark = value
               for (object end-mark) = (multiple-value-list (read-object stream))
               while object do
               (typecase object
                 (property (add-property-to-component object component))
                 (component (add-subcomponent-to-component object component)))
               finally
               (unless (string-ci= begin-mark end-mark)
                 (error "A END:~:@(~a~) was expected, but it found a END:~a"
                        begin-mark end-mark)))
         component))
      ((string-ci= name "END")
       (check-type params null)
       (values nil value))
      (t
       (let ((property (property-from-content-line name params value)))
         property)))))

(defun read-component (stream)
  "Read a component or property from STREAM."
  (let ((object (read-object stream)))
    (check-type object component)
    object))

(defun write-component (component stream)
  "Write COMPONENT to STREAM."
  (let ((name (component-name component)))
    (write-content-line "BEGIN" nil name stream)
    (do-property (prop component)
      (write-property prop stream))
    (dolist (comp (subcomponents component))
      (write-component comp stream))
    (write-content-line "END" nil name stream)))

;;; components.lisp ends here
