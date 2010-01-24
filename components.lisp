;; components.lisp -- Component definitions from RFC 5545.
;;
;; Copyrigth (C) 2010 Mario Castelán Castro
;;
;; This file is part of cl-icalendar
;;
;; cl-icalendar is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-icalendar qis distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-icalendar.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-icalendar)

(defcomponent vcalendar
    (&required
     prodid
     version
     
     &optional-once
     calscale
     method
     
     &optional-once
     xprop
     iana-prop))

(defcomponent vevent
    (&required
     dtstamp
     uid
     
     &optional-once
     dtstart
     class
     created
     description
     geo
     last-mod
     location
     organizer
     priority
     seq
     status
     summary
     transp
     url
     recurid
     
     &optional-multi
     rrule ; optional but SHOULD appear only once at most
     attach
     attendee
     categories
     comment
     contact
     exdate
     rstatus
     related
     resources
     rdate
     x-prop
     iana-prop))

;; cl-icalendar.lisp ends here
