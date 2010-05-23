;; components.lisp --- iCalendar components
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

(defvar *component-classes*
  (make-hash-table :test #'eq))

(defclass component-class ()
  ((name
    :initform (required-arg)
    :type symbol
    :initarg :name
    :reader component-name)
   (allow-x-properties-p
    :initform nil
    :type boolean
    :initarg :allow-x-properties-p
    :reader component-allow-x-properties-p)
   (subcomponents
    :initform nil
    :type list
    :initarg :subcomponents
    :reader component-subcomponents)
   (property-list
    :initform nil
    :type list
    :initarg :property-list
    :reader component-property-list)))

(defmethod print-object ((object component-class) stream)
  (print-unreadable-object (object stream :type t)
    (princ (component-name object) stream)))

(defclass property-class ()
  ((name
    :initform (required-arg)
    :type symbol
    :initarg :name
    :reader property-name)
   (type
    :initform t
    :initarg :type
    :reader property-type)
   (default-type
    :initform nil
    :initarg :default-type
    :reader property-default-type)      
   (required
    :initform nil
    :initarg :required
    :reader property-required-p)
   (default
    :initarg :default
    :reader property-default)
   (count
    :initform 1
    :type (or null (integer 1 *))
    :initarg :count
    :reader property-count)
   (multiple-instance-p
    :initform nil
    :type boolean
    :initarg :multiple-instance-p
    :reader property-multiple-instance-p)
   (parameter-list
    :initform nil
    :type list
    :initarg :parameter-list
    :reader property-parameter-list)
   (allow-x-parameters-p
    :initform nil
    :type boolean
    :initarg :allow-x-parameter-p
    :reader property-allow-x-parameters-p)))

(defun make-property-from-definition (definition)
  "Parse a property definition."
  (let ((property-name (car definition))
        (property-options (cdr definition)))
    (apply #'make-instance 'property-class
           :name property-name
           property-options)))

(defmacro define-component (name supercomponents properties &rest options)
  (declare (ignorable supercomponents options))
  (declare (symbol name))
  `(setf (gethash ',name *component-classes*)
         (apply #'make-instance 'component-class
                :name ',name
                :property-list
                (mapcar #'make-property-from-definition ',properties)
                ',(with-collecting
                   (dolist (option options)
                     (let ((option-name (car option))
                           (option-values (cdr option)))
                       (collect option-name)
                       (collect option-values)))))))


;;;; Standard component's definitions

(define-component vcalendar ()
  ((proid
    :required t
    :type text)
   (version
    :required t
    :type text
    :default "2.0")
   (calscale
    :type text
    :default "GREGORIAN")
   (method
    :type text))
  ;; Options
  (:allow-x-properties-p . t)
  (:subcomponents vevent vtodo vjourunal vfreebusy vtimezone))


(define-component vtodo ()
  ((dtstamp
    :required t
    :type datetime)
   (uid
    :required t
    :type text)
   (class
    :type text)
   (completed
    :type datetime)
   (created
    :type datetime)
   (description
    :type text)
   (dtstart
    :default-type datetime
    :type (or datetime date))
   (geo
    :count 2
    :type float)
   (last-modified
    :type datetime)
   (location
    :type text)
   (organizer
    :type cal-address)
   (percent-complete
    :type (integer 0 100))
   (priority
    :type (integer 0 9))
   (recurrence-id
    :default-type datetime
    :type (or datetime date))
   (sequence
    :type (integer 0 *))
   (status
    :type text)
   (summary
    :type text)
   (url
    :type uri)
   (rrule
    :type recur)
   (due
    :default-type datetime
    :type (or datetime date))
   (duration
    :type duration)
   (attach
    :default-type uri
    :type binary)
   (attendee
    :type cal-address)
   (categories
    :type text)
   (comment
    :type text)
   (contact
    :type text)
   (exdate
    :default-type datetime
    :type (or datetime date))
   (request-status
    :type text)
   (related-to
    :type text)
   (resources
    :type text
    :count nil)
   (rdate
    :default-type datetime
    :type (or datetime date period)))
  ;; Options
  (:allow-x-properties-p . t)
  (:subcomponents valarm))

;;; components.ends here
