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

(defcomponent vcalendar ()
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
(defclass recurrence-base ()
  ((exdate
    :initarg :exdate
    :type (or datetime date)
    :default-type datetime)
   (rdate
    :initarg :rdate
    :type (or datetime date period)
    :default-type datetime)
   (rrule
    :initarg :rrule
    :type recur))
  (:metaclass component-class))

;;; Commentable components
(defclass comment-base ()
  ((comment
    :initarg :comment
    :type text))
  (:metaclass component-class))

;;; Common properties to TODOs, EVENTs, JOURNALs and FREEBUSYs.
(defclass item-base (comment-base)
  ((attendee
    :initarg :attendee
    :type cal-address)
   (contact
    :initarg :contact
    :type text)
   (dtstamp
    :initarg :dtstamp
    :type datetime)
   (organizer
    :initarg :organizer
    :type cal-address)
   (url
    :initarg :url
    :type uri)
   (uid
    ;;:required t
    :initarg :uid
    :type text))
  (:metaclass component-class))

;;; Common properties between EVENTs, TODOs and JOURNALs.
(defclass etj-item-base (item-base)
  ((attach
    :initarg :attach
    :type (or binary uri)
    :default-type uri)
   (categories
    :initarg :categories
    :type text)
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
    :type text)
   (status
    :initarg :status
    :type text)
   (summary
    :initarg :summary
    :type text))
  (:metaclass component-class))

;;; Common properties between EVENTs and TODOs.
(defclass et-item-base (etj-item-base)
  ((description
    :initarg :description
    :type text)
   (duration
    :initarg :duration
    :type duration)
   (geo
    :initarg :geo
    :type float)
   (location
    :initarg :location
    :type text)
   (priority
    :initarg :priority
    :default-value 0
    :type integer)
   (resources
    :initarg :resources
    :type text)
   (sequence
    :initarg :sequence
    :type integer))
  (:metaclass component-class))

(defcomponent vevent (et-item-base)
  ((transp
    :initarg :transp
    :type text)))

(defcomponent vtodo (et-item-base)
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

(defcomponent vjournal (etj-item-base)
  (;; TODO: Multiple times
   (description
    :initarg :description
    :type text)))

(defcomponent vfreebusy (item-base)
  ((dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (freebusy
    :initarg :freebusy
    :type period)))

(defcomponent valarm ()
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

(defclass ds-base (recurrence-base comment-base)
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
    :type utc-offset))
  (:metaclass component-class))

(defcomponent daylight (ds-base) nil)
(defcomponent standard (ds-base) nil)

(defcomponent vtimezone ()
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
