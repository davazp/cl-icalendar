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

(defclass property-type ()
  ((types :accessor %types
	  :documentation "A list of the allowed types (As strings) in
	  this property, the first is the default"
	  :initarg :types)
   (multival-p :accessor %multival-p
	       :initarg :multival-p)))



(defgeneric default-type (x))

(defmethod default-type ((self property-type))
  (first (%types self)))

(defmethod default-type ((property-name string))
  (default-type (gethash property-name *properties*)))

(defmacro define-property (name
			   types
			   multival-p)
  "Define a property type"
  `(setf (gethash ,name *properties*)
	 (make-instance 'property-type
			:types ',types
			:multival-p ,multival-p)))

(defun parse-property-name (string)
  (or (gethash string *properties*)
      (error "Undefined property")))

(defun process-property (content-line)
  (declare (content-line content-line))
  "Process a content-line property and return it value (or values in a
list, if a multivalue property)"
  (with-slots (name params value) content-line
    (let* ((property (parse-property-name name))
	   (type (default-type property))
	   (value (if (%multival-p property)
		      (parse-values value type)
		      (parse-value value type))))
      (values value name))))


;;; Property database

;; Calendar properties
(define-property "CALSCALE" ("TEXT") nil)
(define-property "METHOD" ("TEXT") nil)
(define-property "PRODID" ("TEXT") nil)
(define-property "VERSION" ("TEXT") nil)

;; Component properties
(define-property "ATTACH" ("URI" "BINARY") nil)
(define-property "CATEGORIES" ("TEXT") t)
(define-property "CLASS" ("TEXT")  nil)
(define-property "COMMENT" ("TEXT") nil)
(define-property "DESCRIPTION" ("TEXT") nil)
(define-property "GEO" ("FLOAT") t) ; Must be 2 comma separated values
(define-property "LOCATION" ("TEXT") nil)
(define-property "PERCENT-COMPLETE" ("INTEGER") nil)
(define-property "PRIORITY" ("INTEGER") nil) ; 0 to 9 (Inclusive),
(define-property "RESOURCES" ("TEXT") nil)
(define-property "STATUS" ("TEXT") nil)
(define-property "SUMMARY" ("TEXT") nil)
(define-property "COMPLETED" ("DATE-TIME") nil)
(define-property "DTEND" ("DATE-TIME" "DATE") nil)
(define-property "DUE" ("DATE-TIME" "DATE") nil)
(define-property "DTSTART" ("DATE-TIME" "DATE") nil)
(define-property "DURATION" ("DURATION") nil)
(define-property "FREEBUSY" ("PERIOD") t)
(define-property "TRANSP" ("TEXT") nil)
(define-property "TZID" ("TEXT") nil)
(define-property "TZNAME" ("TEXT") nil)
(define-property "TZOFFSETFROM" ("UTC-OFFSET") nil)
(define-property "TZOFFSETTO" ("UTC-OFFSET") nil)
(define-property "TZURL" ("URI") nil)
(define-property "ATTENDEE" ("CAL-ADDRESS") nil)
(define-property "CONTACT" ("TEXT") nil)
(define-property "ORGANIZER" ("CAL-ADRESS") nil)
(define-property "RECURRENCE-ID" ("DATE-TIME" "DATE") nil)
(define-property "RELATED-TO" ("TEXT") nil)
(define-property "URL" ("URI") nil)
(define-property "UID" ("TEXT") nil)
(define-property "EXDATE" ("DATE-TIME" "DATE") t)
(define-property "RDATE" ("DATE-TIME" "DATE" "PERIOD") t)
(define-property "RRULE" ("RECUR") nil)
(define-property "ACTION" ("TEXT") nil)
(define-property "REPEAT" ("INTEGER") nil)
(define-property "TRIGGER" ("DURATION" "DATE-TIME") nil) ; UTC
(define-property "CREATED" ("DATE-TIME") nil)
(define-property "DTSTAMP" ("DATE-TIME") nil)
(define-property "LAST-MODIFIED" ("DATE-TIME") nil)
(define-property "SEQUENCE" ("INTEGER") nil)

;; properties.lisp ends here
