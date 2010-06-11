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


;;;; Binary

(defclass binary ()
  ;; FIXME: It would be nice to use a stream indeed of an usb8 vector
  ;; here, so we could treat with binary objects on demand, without to
  ;; load whole file in memory. However, cl-base64 does not seem
  ;; support encoding from streams. Indeed, we should load whole
  ;; base64 string before decoding anyway, and we does not hope big
  ;; files in an iCalendar file.
  ((content
    :initarg :content
    :accessor binary-content)))

(define-predicate-type binary)

(defmethod print-object ((object binary) stream)
    (print-unreadable-object (object stream :type t :identity t)
      (format stream ":SIZE ~a BYTES" (length (binary-content object)))))

(defun read-binary-from-stream (stream)
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (content (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t)))
    ;; Read from STREAM sequences into buffer and fill content. When
    ;; content is exhausted, we will adjust it.
    (loop for index = 0 then (+ index nbytes)
          for nbytes = (read-sequence buffer stream)
          until (zerop nbytes)
          when (< (- (array-dimension content 0) index) nbytes)
            do (adjust-array content (* 2 (array-dimension content 0)))
          do (replace content buffer :start1 index :end1 (+ index nbytes))
          finally (adjust-array content index))
    ;; Finally return a binary instance.
    (make-instance 'binary :content content)))

(defun read-binary-from-file (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (read-binary-from-stream in)))

(defun write-binary-to-stream (binary stream)
  (write-sequence (binary-content binary) stream)
  (values))

(defun write-binary-to-file (binary pathname &key if-exists)
  (with-open-file (out pathname
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists if-exists)
    (write-binary-to-stream binary out)))

(defun binary-length (binary)
  (length (binary-content binary)))

(progn
  ;; The binary data value must be written as a base64-encoded
  ;; sequence. Therefore, the ENCODING=BAS64 parameter should be
  ;; present. We don't check this here; indeed, we trust in the caller
  ;; (property code basically) will do the right thing.
  (defmethod format-value ((x binary) &rest params &key encoding &allow-other-keys)
    (declare (ignore params encoding))
    (let ((bytes (binary-content x)))
      (base64:usb8-array-to-base64-string bytes)))

  (defmethod parse-value (string (type (eql 'binary)) &rest params &key encoding &allow-other-keys)
    (declare (ignore params encoding))
    (make-instance 'binary :content (base64:base64-string-to-usb8-array string))))


;;;; Text

(defclass text* ()
  ((language
    :initarg :language
    :initform nil
    :reader text-language)
   (string
    :initarg :text
    :reader text)))

(deftype text ()
  '(or string text*))

(define-predicate-type text)

(defmethod print-object ((x text*) stream)
  (print-unreadable-object (x stream)
    (format stream "TEXT :LANG ~a ~w"
            (text-language x)
            (text x))))

(defmethod text ((x string))
  x)

(defmethod text-language ((x string))
  nil)

(defun make-text (string &optional language)
  (if language
      (make-instance 'text* :text string :language language)
      string))

(defmethod format-value ((text string) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (with-input-from-string (in text)
    (with-output-to-string (out)
      (loop for ch = (read-char in nil)
            while ch
            do
         (cond
           ((char= ch #\newline)
            (write-char #\\ out)
            (write-char #\n out))
           ((char= ch #\\)
            (write-char #\\ out)
            (write-char #\\ out))
           ((char= ch #\,)
            (write-char #\\ out)
            (write-char #\, out))
           ((char= ch #\;)
            (write-char #\\ out)
            (write-char #\; out))
           (t
            (write-char ch out)))))))


(defmethod format-value ((text text*) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format-value (text text)))


(defmethod parse-value (text (type (eql 'text)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (let ((string (text text)))
    (with-input-from-string (in string)
      (with-output-to-string (out)
        (loop for ch = (read-char in nil)
              while ch
              do
           (write-char (if (char/= ch #\\)
                           ch
                           (ecase (read-char in nil)
                             (#\\ #\\)
                             (#\; #\;)
                             (#\, #\,)
                             (#\N #\newline)
                             (#\n #\newline)))
                 out))))))

;;;; Period

(defclass period ()
  ((start
    :initarg :start
    :reader period-start)
   (end
    :initarg :end
    :reader period-end)))

(define-predicate-type period)

(defun make-period (start end)
  (make-instance 'period :start start :end end))

(defgeneric duration-of (x)
  (:method ((x period))
    ;; FIXME: Use days component of duration if possible.
    (make-duration
     :seconds (- (seconds-from-1900 (period-start x))
                 (seconds-from-1900 (period-end x))))))

(defmethod format-value ((p period) &rest params &key &allow-other-keys)
  (declare (ignore params))
  ;; TODO: We should write down in the class `period' if the user
  ;; specifies a duration or a end datetime, in order to format it so.
  (concatenate 'string
               (format-value (period-start p))
               "/"
               (format-value (period-end p))))

(defmethod parse-value (string (type (eql 'period)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (destructuring-bind (start end)
      (split-string string "/")
    (list start end)
    (let ((dstart (parse-value start 'datetime)))
      (list dstart end)
      (make-period dstart
                   (if (char= (char end 0) #\P)
                       (datetime+ dstart (parse-value end 'duration))
                       (parse-value end 'datetime))))))



;;;; UTC-Offset

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


;;;; Cal-address

;;; TODO: Do ir!
(defclass cal-address ()
  nil)

;;;; URI

;;; TODO: Do it!
(defclass uri ()
  nil)

;;; types.lisp ends here
