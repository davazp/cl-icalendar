;; components.lisp --- iCalendar's component
;;
;; Copyrigth (C) 2010,2011 David Vázquez
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

;;; Special anonymous iCalendar type to keep values of unknown type.
(defclass unknown-value ()
  ((value
    :initarg :value
    :initform (required-arg)
    :type string
    :reader unknown-value)))

(defmethod parse-value (value (type (eql nil)) &optional params)
  (declare (ignore params))
  (make-instance 'unknown-value :value value))

(defmethod format-value ((x unknown-value) &optional params)
  (declare (ignore params))
  (unknown-value x))

(defprinter (x unknown-value)
  (write (unknown-value x)))


;;; components.ends here
