;; types-time.lisp ---
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

;;; Time

(defconstant +seconds-per-day+ 86400)

(deftype time () `(mod ,+seconds-per-day+))
(register-ical-value time)

(defun encode-time (hour minute second)
  (encode-universal-time second minute hour 1 1 1900 0))

(defmethod format-value (time (type (eql 'time)) &optional params)
  (declare (ignore params))
  (decoded-universal-time (:hour h :minute m :second s) time
    (format nil "~2,'0d~2,'0d~2,'0d" h m s)))

(defmethod parse-value (string (type (eql 'time)) &optional params)
  (declare (ignore params))
  (encode-time
   (parse-unsigned-integer string :start 0 :end 2)
   (parse-unsigned-integer string :start 2 :end 4)
   (parse-unsigned-integer string :start 4 :end 6)))


;;; types-time.lisp ends here
