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

(defclass standard-component-object () nil
  (:metaclass component-class))

(defclass x-component-object () nil
  (:metaclass component-class))

(defcomponent vcalendar ()
  ((prodid
    :type text)
   (version
    :type text)
   (calscale
    :default-value "GREGORIAN"
    :type text)
   (method
    :type text))
  (:subcomponents vtodo vjournal))

(defcomponent vtodo ()
  ((dtstamp
    ;:required t
    :type datetime)
   (uid
    ;:required t
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
    :type (or datetime date)
    :default-type datetime )
   (geo
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
    :type (or datetime date)
    :default-type datetime)
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
    :type (or datetime date)
    :default-type datetime)
   (duration
    :type duration)
   (attach
    :type (or binary uri)
    :default-type uri)
   (attendee
    :type cal-address)
   (categories
    :type text)
   (comment
    :type text)
   (contact
    :type text)
   (exdate
    :type (or datetime date)
    :default-type datetime)
   (request-status
    :type text)
   (related-to
    :type text)
   (resources
    :type text)
   (rdate
    :type (or datetime date period)
    :default-type datetime))
  ;; Options
  (:subcomponents valarm))


(defcomponent vjournal (standard-component-object)
  nil)


;;; components-standard.lisp ends here
