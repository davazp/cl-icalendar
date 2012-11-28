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
  (:use :common-lisp :trivial-gray-streams)
  (:shadow #:time #:standard :method)
  ;; If it is running on SBCL, lock the package.
  #+sbcl (:lock t)

  ;; Exported symbols. It could be mantained for each file separately,
  ;; however I think it will help to keep the API stable so.

  ;; Data types
  (:export #:ical-value
           #:x-ical-value
           #:utc-offset
           #:format-value
           #:format-values
           #:parse-value
           #:parse-values
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
           #:make-period
           #:period-start
           #:period-end
           #:period-duration
           ;; Binary
           #:binary
           #:read-binary-from-stream
           #:read-binary-from-file
           #:write-binary-to-stream
           #:write-binary-to-file
           ;; Recur
           #:recur
           #:recur-instance-p
           #:do-recur-instances
           #:list-recur-instances)

  ;; Properties and components
  (:export #:make-component
           #:add-property
           #:query-property
           #:delete-property)

  ;; Calendars
  (:export #:open-vcalendar
           #:save-vcalendar
           #:vcalendar
           #:valarm
           #:vtodo
           #:vevent
           #:vjournal
           #:vfreebusy
           #:vtimezone
           #:standard
           #:daylight))

;;; package.lisp ends here
