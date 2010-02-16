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

;; A property consist of a list of valid types, the first is the
;; default and no more are supported currently
(defclass property ()
  ((types)))

(defun default-type (property-name)
  (first (slot-value (gethash property-name *properties*) 'types)))

(defmacro define-property (name types)
  `(let ((new-property (make-instance 'property)))
     (setf (slot-value new-property 'types) ',types)
     (setf (gethash ,name *properties*) new-property)))

(defun process-property (content-line)
  (let* ((name (content-line-name content-line))
	 (parameters (content-line-params content-line))
	 (value (content-line-value content-line))
	 (type (loop for i in parameters
		     for param-name = (first i)
		     for param-value = (second i)
		     do (when (string= param-name "VALUE")
			  (return param-value))
		     finally (return (default-type name)))))
    (values (parse-value value type)
	    name)))

(define-property "DTSTART" (datetime))
(define-property "FOO" (boolean))

(when nil
  (with-input-from-string (str "DTSTART:19980403T120000
")
    (process-property (read-content-line str))) ; Give a Unable to
						; display error
						; condition, why?
  (with-input-from-string (str "FOO:TRUE
")
    (process-property (read-content-line str))) ; T, "FOO", correct
  )

;; properties.lisp ends here
