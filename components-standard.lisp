;; components-standard.lisp --- Standard iCalendar's component definitions
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

(defclass standard-component () nil
  (:metaclass component-class))

(defclass x-component () nil
  (:metaclass component-class))

;;; We use abstract classes here in order to reduce the redundant of
;;; common properties between components.

(define-standard-component vcalendar ()
  ((prodid
    :initarg :prodid
    :type text)
   (version
    :initarg :version
    :type text)
   (calscale
    :initarg :calscale
    :type text
    :initform "GREGORIAN")
   (method
    :initarg :method
    :type text))
  (:subcomponents vtodo vjournal vevent vfreebusy vtimezone))

;;; Recurrence components
(define-abstract-component recurrence-base ()
  ((exdate
    :initarg :exdate
    :type (or datetime date)
    :default-type datetime
    :multiple-value-p t)
   (rdate
    :initarg :rdate
    :type (or datetime date period)
    :default-type datetime
    :multiple-value-p t)
   (rrule
    :initarg :rrule
    :type recur)))

;;; Commentable components
(define-abstract-component comment-base ()
  ((comment
    :initarg :comment
    :type text
    :multiple-value-p t)))

;;; Common properties to TODOs, EVENTs, JOURNALs and FREEBUSYs.
(define-abstract-component item-base (comment-base)
  ((attendee
    :initarg :attendee
    :type cal-address
    :multiple-value-p t)
   (contact
    :initarg :contact
    :type text
    :multiple-value-p t)
   (dtend
    :initarg :dtend
    :type (or datetime date)
    :default-type datetime)
   (dtstamp
    :initarg :dtstamp
    :type datetime)
   (organizer
    :initarg :organizer
    :type cal-address)
   (request-status
    :initarg :request-status
    :type text
    :multiple-value-p t)
   (url
    :initarg :url
    :type uri)
   (uid
    ;;:required t
    :initarg :uid
    :type text)))

;;; Common properties between EVENTs, TODOs and JOURNALs.
(define-abstract-component etj-item-base (item-base recurrence-base)
  ((attach
    :initarg :attach
    :type (or binary uri)
    :default-type uri
    :multiple-value-p t)
   (categories
    :initarg :categories
    :type text
    :multiple-value-p t)
   (class
    :initarg :class
    :type text)
   (created
    :initarg :created
    :type datetime
    :initform (now))
   (dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (last-modified
    :initarg :last-modified
    :type datetime)
   (recurrence-id
    :initarg :recurrence-id
    :type (or datetime date)
    :default-type datetime)
   (related-to
    :initarg :related-to
    :type text
    :multiple-value-p t)
   (status
    :initarg :status
    :type text)
   (summary
    :initarg :summary
    :type text)))

;;; Common properties between EVENTs and TODOs.
(define-abstract-component et-item-base (etj-item-base)
  ((description
    :initarg :description
    :type text)
   (duration
    :initarg :duration
    :type duration)
   (geo
    :initarg :geo
    :type geo)
   (location
    :initarg :location
    :type text)
   (priority
    :initarg :priority
    :default-value 0
    :type integer)
   (resources
    :initarg :resources
    :type text
    :multiple-value-p t)
   (sequence
    :initarg :sequence
    :type integer)))

(defprinter (c etj-item-base)
  (if (slot-boundp c 'summary)
      (prin1 (slot-value c 'summary))
      (princ "no summary")))

(define-standard-component vevent (et-item-base)
  ((transp
    :initarg :transp
    :type text)))

(define-standard-component vtodo (et-item-base)
  ((completed
    :initarg :completed
    :type datetime)
   (due
    :initarg :due
    :type (or datetime date)
    :default-type datetime)
   (percent-complete
    :initarg :percent-complete
    :type integer)))

(define-standard-component vjournal (etj-item-base)
  (;; TODO: Multiple times
   (description
    :initarg :description
    :type text)))

(define-standard-component vfreebusy (item-base)
  ((dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (freebusy
    :initarg :freebusy
    :type period)))

(define-standard-component valarm ()
  ((action
    :initarg :action
    :type text)
   (repeat
    :initarg :repeat
    :type integer)
   (trigger
    :initarg :trigger
    :type (or duration datetime)
    :default-type duration)
   ;; TODO: Once
   (attach
    :initarg :attach
    :type (or binary uri)
    :default-type uri)
   (attendee
    :initarg :attendee
    :type cal-address)
   (description
    :initarg :description
    :type text)
   (duration
    :initarg :duration
    :type duration)
   (summary
    :initarg :summary
    :type text)))

(define-abstract-component ds-base (recurrence-base comment-base)
  ((dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (tzname
    :initarg :tzname
    :type text)
   (tzoffsetfrom
    :initarg :tzoffsetfrom
    :type utc-offset)
   (tzoffsetto
    :initarg :tzoffsetto
    :type utc-offset)))

(define-standard-component vdaylight (ds-base)
  nil
  (:default-initargs :name "DAYLIGHT"))

(define-standard-component vstandard (ds-base)
  nil
  (:default-initargs :name "STANDARD"))

(define-standard-component vtimezone ()
  ((last-modified
    :initarg :last-modified
    :type datetime)
   (tzid
    :initarg :tzid
    :type text)
   (tzurl
    :initarg :tzurl
    :type uri)))

;;; components-standard.lisp ends here
