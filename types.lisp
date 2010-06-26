;; types.lisp ---
;;
;;     This file implements the iCalendar data types described in the
;;     RFC5545 and provide a plataform in order to add new data types.
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

;;; This file contains code which implements the values that iCalendar
;;; properties can take. We provide a data type specifier,
;;; constructor, accessors, and utilities functions for each one of
;;; them. Indeed, we provide 3 common functions in order to turn these
;;; objects to strings and vice versa.

(deftype ical-value ()
  '(or
    boolean integer float text binary
    uri cal-address utc-offset date time date-time
    duration period recur
    ;; x-type??
    ;; A (satisfies x-typep) be placed here soon.
    ))

;;; Like `check-type' but it signals an error with %parse-error.
(defmacro check-ical-type (place type)
  (with-gensyms (vplace)
    `(let ((,vplace ,place))
       (unless (typep ,vplace ',type)
         (%parse-error "The ~a is not a ~a type." ',place ',type)))))

;;; Generic functions
(defgeneric format-value (value &rest params &key &allow-other-keys))
(defgeneric format-values (values &rest params &key &allow-other-keys))
(defgeneric parse-value (string type &rest params &key &allow-other-keys))
(defgeneric parse-values (string type &rest params &key &allow-other-keys))

(defun lookup-type (string)
  (find-symbol (string-upcase string) :cl-icalendar))

(defmethod parse-value (string (typestring string) &rest params &key &allow-other-keys)
  (let ((type (lookup-type typestring)))
    (if (null type)
        (no-applicable-method 'parse-value string typestring)
        (apply #'parse-value string type params))))

;;; Wrapper for both standard as user-defined format-value and
;;; parse-value methods. The :around methods are used to implement
;;; global parameters.

;;; I am not sure if it is allowed by the iCalendar specification to
;;; use the encoding parameter on all types. I assume it is
;;; so. Otherwise it should be a method for BINARY data type.
;;; -- DVP

(defmethod format-value :around (string &rest params &key (encoding :8bit) &allow-other-keys)
  (declare (ignore string params))
  (ecase encoding
    (:base64
     (base64:string-to-base64-string (call-next-method)))
    (:8bit
     (call-next-method))))

(defmethod parse-value :around (string type &rest params &key (encoding :8bit) &allow-other-keys)
  (ecase encoding
    (:base64
     (apply #'call-next-method (base64:base64-string-to-string string) type params))
    (:8bit
     (call-next-method))))


;;; Multiple-value versions

(defmethod parse-values (string (typestring string) &rest params &key &allow-other-keys)
  (apply #'parse-values string (lookup-type typestring) params))

(defmethod parse-values (string (type symbol) &rest params &key &allow-other-keys)
  (labels (;; Find the position of the separator character (,) from
           ;; the character at START position.
           (position-separator (start)
             (let ((position (position #\, string :start start)))
               (if (and (integerp position)
                        (< 0 position)
                        (char= #\\ (char string (1- position))))
                   (position-separator (1+ position))
                   position))))
    ;; Collect values
    (loop for start = 0 then (1+ end)
          for end = (position-separator start)
          for sub = (subseq string start end)
          collect (apply #'parse-value sub type params)
          while end)))

(defmethod format-values (objects &rest params &key &allow-other-keys)
  (flet ((format-value* (x)
           (apply #'format-value x params)))
    (join-strings (mapcar #'format-value* objects) #\,)))


;;;; Boolean

(define-predicate-type boolean)

(defmethod format-value ((bool (eql 't)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  "TRUE")

(defmethod format-value ((bool (eql 'nil)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  "FALSE")

(defmethod parse-value (string (type (eql 'boolean)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (cond
    ((string-ci= string "TRUE")  t)
    ((string-ci= string "FALSE") nil)
    (t
     (%parse-error "~a is not a boolean data type." string))))


;;;; Integer

(defmethod format-value ((n integer) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~a" n))

(defmethod parse-value (string (type (eql 'integer)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (values (parse-integer string)))


;;;; Float

(defmethod format-value ((x float) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~f" x))

(defmethod parse-value (string (type (eql 'float)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (let ((sign 1)                        ; the sign
        (x 0)                           ; integer part
        (y 0))                          ; fractional part
    (with-input-from-string (in string)
      ;; Read sign
      (case (peek-char nil in)
        (#\+
         (read-char in))
        (#\-
         (setf sign -1)
         (read-char in)))

      ;; Read integer part
      (let ((istring (read-until in (complement #'digit-char-p) nil nil)))
        (setf x (parse-integer istring)))

      ;; Read fractinal part (if any)
      (let ((dot (read-char in nil)))
        (unless (null dot)
          (unless (char= dot #\.)
            (%parse-error "Bad formed float."))

          (let ((fstring (read-until in (complement #'digit-char-p) nil nil)))
            (setf y (/ (float (parse-integer fstring))
                       (expt 10 (length fstring))))))))

    (* sign (+ x y))))


;;;; URI

(defclass uri ()
  ((uri
    :type string
    :initarg :uri
    :reader uri)))

(define-predicate-type uri)

(defun make-uri (uri)
  (declare (string uri))
  (make-instance 'uri :uri uri))


;;;; Cal-address

(defclass cal-address (uri)
  nil)

(define-predicate-type cal-address)

(defun make-cal-address (uri)
  (declare (string uri))
  (make-instance 'cal-address :uri uri))


;; User-defined iCalendar data types



;;; types.lisp ends here
