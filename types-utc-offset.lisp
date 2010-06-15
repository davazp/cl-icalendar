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

(defclass utc-offset ()
  ((offset
    :initform (required-arg)
    :initarg :offset
    :reader %utc-offset)))

(define-predicate-type utc-offset utc-offset-p)

(defgeneric utc-offset-hour (utc-offset)
  (:method ((x utc-offset))
    (abs (idiv (%utc-offset x) 3600))))

(defgeneric utc-offset-minute (utc-offset)
  (:method ((x utc-offset))
    (mod (idiv (%utc-offset x) 60) 60)))

(defgeneric utc-offset-second (utc-offset)
  (:method ((x utc-offset))
    (mod (%utc-offset x) 60)))

(defgeneric utc-offset-negative-p (utc-offset)
  (:method ((x utc-offset))
    (< (%utc-offset x) 0)))

(defmethod print-object ((x utc-offset) stream)
  (print-unreadable-object (x stream :type t)
    (write-string (format-value x) stream)))

(defmethod format-value ((x utc-offset) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~:[+~;-~]~2,'0d~2,'0d~@[~2,'0d~]"
          (utc-offset-negative-p x)
          (utc-offset-hour x)
          (utc-offset-minute x)
          (let ((seconds (utc-offset-second x)))
            (if (zerop seconds) nil seconds))))

(defmethod parse-value (string (type (eql 'utc-offset)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (let* ((sign (elt string 0))
         (hour (parse-unsigned-integer string :start 1 :end 3))
         (minute (parse-unsigned-integer string :start 3 :end 5))
         (second))
    (ecase (length string)
      (5 (setf second 0))
      (7 (setf second (parse-unsigned-integer string :start 5 :end 7))))
    (cond
      ((string= sign "+")
       (check-ical-type hour (integer 0 59))
       (make-instance 'utc-offset :offset (+ (* 3600 hour) (* 60 minute) second)))
      ((string= sign "-")
       (let ((value (- (+ (* 3600 hour) (* 60 minute) second))))
         (if (zerop value)
             (%parse-error "-0000 or -000000 are not valid utc-offset data type.")
             (make-instance 'utc-offset :offset value))))
      (t
       (%parse-error "Bad sign.")))))


;;; types-utc-offset.lisp ends here
