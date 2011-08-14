;; package.lisp
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

;;; Package definition

(defpackage :cl-icalendar
  (:nicknames :icalendar :ical)
  (:use :c2cl :trivial-gray-streams)
  (:shadow #:time #:standard)
  ;; If it is running on SBCL, lock the package.
  #+sbcl (:lock t)
  ;; Exported symbols. It could be mantained for each file separately,
  ;; however I think it will help to keep the API stable so.
  (:export #:make-folding-stream
           #:with-folding-stream
           ;; Data types
           #:ical-value
           #:x-ical-value
           #:uri
           #:utc-offset
           #:format-value
           #:parse-value
           ;; Boolean
           #:booleanp
           ;; Text
           #:text
           #:textp
           #:text
           #:text-language
           #:make-text
           ;; Datetime
           #:make-datetime
           #:datetimep
           #:datetime=
           #:datetime<
           #:datetime<=
           #:datetime>
           #:datetime>=
           #:datetime+
           #:datetime-
           ;; Date
           #:make-date
           #:datep
           #:date=
           #:date<
           #:date<=
           #:date>
           #:date>=
           #:date+
           #:date-
           #:date-day-of-week
           #:date-day-of-year
           #:date-week-of-year
           ;; Time
           #:make-time
           #:timep
           #:time=
           #:time<
           #:time<=
           #:time>
           #:time>=
           #:time+
           #:time-
           ;; Duration
           #:make-duration
           #:duration
           #:durationp
           #:duration-days
           #:duration-hours
           #:duration-minutes
           #:duration-seconds
           #:duration-backward-p
           ;; Period
           #:period
           #:periodp
           ;; Binary
           #:binary
           #:read-binary-from-stream
           #:read-binary-from-file
           #:write-binary-to-stream
           #:write-binary-to-file
           #:binary-length
           ;; Recur
           #:recur
           #:recur-instance-p
           ;; Components
           #:open-vcalendar
           #:save-vcalendar
           #:read-vcalendar
           #:write-vcalendar
           #:component-name
           #:component-subcomponents
           #:do-subcomponents
           #:do-property
           #:vcalendar
           #:vevent
           #:vtodo
           #:vjournal
           #:vfreebusy
           #:valarm
           #:vdaylight
           #:vstandard
           #:vtimezone))

;;; package.lisp ends here
