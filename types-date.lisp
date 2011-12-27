;; types-date.lisp ---
;;
;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010, 2011 David Vázquez
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

;;; Dates

(defvar *weekday-names*
  #("MO" "TU" "WE" "TH" "FR" "SA" "SU"))

(deftype date () '(integer 0))
(register-ical-value date)

(defun encode-date (date month year)
  (encode-universal-time 0 0 0 date month year))

(defmethod format-value (date (type (eql 'date)) &optional params)
  (declare (ignore params))
  (decoded-universal-time (:date d :month m :year y) date
    (format nil "~4,'0d~2,'0d~2,'0d" y m d)))

(defmethod parse-value (string (type (eql 'date)) &optional params)
  (declare (ignore params))
  (unless (= (length string) 8)
    (%parse-error "parse error."))
  (let ((d (parse-unsigned-integer string :start 6 :end 8))
        (m (parse-unsigned-integer string :start 4 :end 6))
        (y (parse-unsigned-integer string :start 0 :end 4)))
    (encode-date d m y)))


;;; types-date.lisp ends here
