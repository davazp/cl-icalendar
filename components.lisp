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
  (make-hash-table :test #'equalp))

(defclass component-class ()
  ((name
    :initform (required-arg)
    :type string
    :initarg :name
    :reader component-name)
   (allow-x-properties-p
    :initform t
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

(defun find-component (cname)
  (values (gethash cname *component-classes*)))

(defmacro define-component (name supercomponents properties &rest options)
  (declare (ignorable supercomponents options))
  (declare (symbol name))
  `(setf (gethash (string ',name) *component-classes*)
         (apply #'make-instance 'component-class
                :name (string ',name)
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
  (:subcomponents valarm))


(define-component vevent ()
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
  (:subcomponents valarm))


;;; components.ends here
