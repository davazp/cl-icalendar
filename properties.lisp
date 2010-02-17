;; properties.lisp --
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

(defvar *properties* (make-hash-table :test #'equal))

(defclass property ()
  ((types :accessor property-types
	  :documentation "A list of the allowed types (As strings) in
	  this property, the first is the default"
	  :initarg :types)
   (validator :accessor property-validator
	      :initarg :validator)))

(defgeneric default-type (x))

(defmethod default-type ((self property))
  (first (property-types self)))

(defmethod default-type ((property-name string))
  (default-type (gethash property-name *properties*)))

(defmacro define-property (name
			   types
			   &key
			   (validator (constantly t)))
  `(setf (gethash ,name *properties*)
	 (make-instance 'property
			:types ',types
			:validator ,validator)))

(defun process-property (content-line)
  (with-slots (name params value) content-line
    (let* ((property (gethash name *properties*))
	   (type (default-type property))
	   (value (parse-value value type)))
      (funcall (property-validator type) value)
      (values value name))))

;; properties.lisp ends here
