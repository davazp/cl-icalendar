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
  (:use :cl :trivial-gray-streams)
  (:shadow #:time)
  (:export ;; Basic types
           #:format-boolean
           #:format-float
           #:parse-boolean
           #:parse-float
           ;; Text
           #:text
           #:textp
           #:text
           #:text-language
           #:make-text
           #:format-text
           #:parse-text
           ;; Datetime
           #:make-datetime
           #:datetimep
           #:date-day
           #:date-month
           #:date-year
           #:time-hour
           #:time-minute
           #:time-second
           #:datetime=
           #:datetime<
           #:datetime<=
           #:datetime>
           #:datetime>=
           #:datetime+
           #:datetime-
           #:parse-datetime
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
           #:parse-date
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
           #:parse-time
           ;; Duration
           #:make-duration
           #:duration
           #:durationp
           #:duration-days
           #:duration-hours
           #:duration-minutes
           #:duration-seconds
           #:duration-backward-p
           #:format-duration
           #:parse-duration))

;;; package.lisp ends here
