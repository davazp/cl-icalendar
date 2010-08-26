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

(defcomponent vcalendar (standard-component)
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
  (:subcomponents vtodo vjournal))

(defcomponent vtodo (standard-component)
  ((dtstamp
    ;;:required t
    :initarg :dtstamp
    :type datetime)
   (uid
    ;;:required t
    :initarg :uid
    :type text)
   (class
    :initarg :class
    :type text)
   (completed
    :initarg :completed
    :type datetime)
   (created
    :initarg :created
    :type datetime)
   (description
    :initarg :description
    :type text)
   (dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (geo
    :initarg :geo
    :type float)
   (last-modified
    :initarg :last-modified
    :type datetime)
   (location
    :initarg :location
    :type text)
   (organizer
    :initarg :organizer
    :type cal-address)
   (percent-complete
    :initarg :percent-complete
    :type integer)
   (priority
    :initarg :priority
    :default-value 0
    :type integer)
   (recurrence-id
    :initarg :recurrence-id
    :type (or datetime date)
    :default-type datetime)
   (sequence
    :initarg :sequence
    :type integer)
   (status
    :initarg :status
    :type text)
   (summary
    :initarg :summary
    :type text)
   (url
    :initarg :url
    :type uri)
   (rrule
    :initarg :rrule
    :type recur)
   (due
    :initarg :due
    :type (or datetime date)
    :default-type datetime)
   (duration
    :initarg :duration
    :type duration)
   (attach
    :initarg :attach
    :type (or binary uri)
    :default-type uri)
   (attendee
    :initarg :attendee
    :type cal-address)
   (categories
    :initarg :categories
    :type text)
   (comment
    :initarg :comment
    :type text)
   (contact
    :initarg :contact
    :type text)
   (exdate
    :initarg :exdate
    :type (or datetime date)
    :default-type datetime)
   (request-status
    :initarg :request-status
    :type text)
   (related-to
    :initarg :related-to
    :type text)
   (resources
    :initarg :resources
    :type text)
   (rdate
    :initarg :rdate
    :type (or datetime date period)
    :default-type datetime))
  ;; Options
  (:subcomponents valarm))


(defcomponent vevent ()
  ((dtstamp
    ;;:required t
    :initarg :dtstamp
    :type datetime)
   (uid
    ;;:required t
    :initarg :uid
    :type text)
   (dtstart
    :initarg :dtstart
    :type (or datetime date)
    :default-type datetime)
   (class
    :initarg :class
    :type text)
   (created
    :initarg :created
    :type datetime)
   (description
    :initarg :description
    :type text)
   (geo
    :initarg :geo
    :type float)
   (last-modified
    :initarg :last-modified
    :type datetime)
   (location
    :initarg :location
    :type text)
   (organizer
    :initarg :organizer
    :type cal-address)
   (priority
    :initarg :priority
    :default-value 0
    :type integer)
   (sequence
    :initarg :sequence
    :type integer)
   (status
    :initarg :status
    :type text)
   (summary
    :initarg :summary
    :type text)
   (transp
    :initarg :transp
    :type text) 
   (url
    :initarg :url
    :type uri)
   (recurrence-id
    :initarg :recurrence-id
    :type (or datetime date)
    :default-type datetime)
   (dtend
    :initarg :dtend
    :default-type datetime
    :type (or datetime date)) 
   (duration
    :initarg :duration
    :type duration)
   (attach
    :initarg :attach
    :type (or binary uri)
    :default-type uri)
   (attendee
    :initarg :attendee
    :type cal-address)
   (categories
    :initarg :categories
    :type text)
   (comment
    :initarg :comment
    :type text)
   (contact
    :initarg :contact
    :type text)
   (exdate
    :initarg :exdate
    :type (or datetime date)
    :default-type datetime)
   (request-status
    :initarg :request-status
    :type text)
   (related-to
    :initarg :related-to
    :type text)
   (resources
    :initarg :resources
    :type text)
   (rdate
    :initarg :rdate
    :type (or datetime date period)
    :default-type datetime))
  (:subcomponents valarm))


;;; components-standard.lisp ends here
