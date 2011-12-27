;; types-datetime.lisp ---
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

;;; Datetimes

(deftype datetime () '(integer 0))
(register-ical-value datetime :name "DATE-TIME")

(defun encode-datetime (day month year hour minute second)
  (+ (encode-date day month year)
     (encode-time hour minute second)))

(defmethod format-value (datetime (type (eql 'datetime)) &optional params)
  (declare (ignore params))
  (decoded-universal-time (:year y :month m :date d :hour h :minute min :second s)
      datetime
    (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d" y m d h min s)))

(defmethod parse-value (string (type (eql 'datetime)) &optional params)
  (declare (ignore params))
  (flet ((ill-formed () (%parse-error "Bad datetime format.")))
    (when (< (length string) 9)
      (ill-formed))
    (let ((string-date (subseq string 0 8))
          (string-time (subseq string 9)))
      (unless (char= (elt string 8) #\T)
        (ill-formed))
      (let ((date (parse-value string-date 'date))
            (time (parse-value string-time 'time)))
        (+ date time)))))


;;; types-datetime.lisp ends here
