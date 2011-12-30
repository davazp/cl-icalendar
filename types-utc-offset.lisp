;; types-utc-offset.lisp ---
;;
;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010 David Vázquez
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

(deftype utc-offset ()
  'integer)

(register-ical-value utc-offset)

(defun utc-offset-hour (utc-offset)
  (values (truncate (abs utc-offset) 3600)))

(defun utc-offset-minute (utc-offset)
  (values (truncate (mod (abs utc-offset) 3600) 60)))

(defun utc-offset-second (utc-offset)
  (mod (abs utc-offset) 60))

(defun utc-offset-negative-p (utc-offset)
  (minusp utc-offset))

(defmethod format-value (x (type (eql 'utc-offset)) &optional params)
  (declare (ignore params))
  (declare (type (integer -43200 43200) x))
  (format nil "~:[+~;-~]~2,'0d~2,'0d~@[~2,'0d~]"
          (utc-offset-negative-p x)
          (utc-offset-hour x)
          (utc-offset-minute x)
          (let ((seconds (utc-offset-second x)))
            (if (zerop seconds) nil seconds))))

(defmethod parse-value (string (type (eql 'utc-offset)) &optional params)
  (declare (ignore params))
  (let* ((sign (elt string 0))
         (hour (parse-unsigned-integer string :start 1 :end 3))
         (minute (parse-unsigned-integer string :start 3 :end 5))
         (second (ecase (length string)
                   (5 0)
                   (7 (parse-unsigned-integer string :start 5 :end 7)))))
    (let ((value (+ (* 3600 hour) (* 60 minute) second)))
      (check-ical-type hour (integer 0 12))
      (check-ical-type minute (integer 0 59))
      (check-ical-type second (integer 0 59))
      (cond
        ((char= sign #\+))
        ((char= sign #\-)
         (when (zerop value)
           (%parse-error "The value -0000 or -000000 are not valid utc-offsets."))
         (setf value (- value)))
        (t (%parse-error "Bad sign.")))
      value)))


;;; types-utc-offset.lisp ends here
