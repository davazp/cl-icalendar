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

(deftype ical-value ()
  '(or
    boolean integer float text          ; binary
    uri cal-address utc-offset date time date-time
    duration period recur
    ;; x-type??
    ;; A (satisfies x-typep) be placed here soon.
    ))


;;; This file contains code which implements the values that iCalendar
;;; properties can take. We provide a data type specifier,
;;; constructor, accessors, and utilities functions for each one of
;;; them. Indeed, we provide 3 common functions in order to turn these
;;; objects to strings and vice versa.

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
  (labels ( ;; Find the position of the separator character (,) from
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
  (join-strings (mapcar (lambda (x)
                          (apply #'format-value x params))
                        objects)
                #\,))


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
     (error "~a is not a boolean data type." string))))


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
            (error "Bad formed float."))

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
    ;; Read from STREAM sequences into buffer and fill buffer. When
    ;; buffer is exhausted, we will adjust it.
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Time data types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Date

(deftype day   () '(integer 1 31))
(deftype month () '(integer 1 12))
(deftype year  () '(integer 1900))

(defclass date ()
  ((;; Number of days before of the beginning of 1900.
    datestamp
    :initarg :datestamp
    :reader datestamp)))

(define-predicate-type date)

(defmethod print-object ((x date) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d-~2,'0d-~4,'0d"
            (date-day x)
            (date-month x)
            (date-year x))))

;;; Part of the following code is based in the file time.lisp of the
;;; SBCL system.
(defvar *days-in-month*
  '(0 31 28 31 30 31 30 31 31 30 31 30 31))

(defvar *days-in-month-leap-year*
  '(0 31 29 31 30 31 30 31 31 30 31 30 31))

(defvar *days-before-month*
  #.(let ((reversed-result nil)
          (sum 0))
      (push 0 reversed-result)
      (dolist (days-in-month '(31 28 31 30 31 30 31 31 30 31 30 31))
        (push sum reversed-result)
        (incf sum days-in-month))
      (coerce (nreverse reversed-result) 'simple-vector)))

(defvar *days-before-month-leap-year*
  #.(let ((reversed-result nil)
          (sum 0))
      (push 0 reversed-result)
      (dolist (days-in-month '(31 29 31 30 31 30 31 31 30 31 30 31))
        (push sum reversed-result)
        (incf sum days-in-month))
      (coerce (nreverse reversed-result) 'simple-vector)))


(defun leap-years-before (year)
  (declare (year year))
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
          (truncate years 100))
       (truncate (+ years 300) 400))))

(defun leap-years-between (start end)
  (- (leap-years-before end)
     (leap-years-before start)))

(defun leap-year-p (year)
  (declare (year year))
  (and (divisiblep year 4)
       (or (not (divisiblep year 100))
           (divisiblep year 400))))

(defun make-date (day month year)
  (declare (day day) (month month) (year year))
  (let ((days-from-1900
         (+ (* 365 (- year 1900))
            (if (> month 2)
                (leap-years-before (1+ year))
                (leap-years-before year))
            (elt *days-before-month* month)
            (1- day))))
    (make-instance 'date :datestamp days-from-1900)))

(defun make-date-offset-year (year day)
  (declare (year year))
  (make-instance 'date :datestamp (+ (* 365 (- year 1900))
                                     (leap-years-before year)
                                     day)))

;; Compute the first day of the first week in the given year.  Return
;; it as a offset in days since january 1th.

;; TODO: Implement wkst /= 0
;; TODO: Write tests

(defun year-first-week (year)
  ;; DOW=day of week
  (let ((first-day-dow (mod (+ (- year 1900)
                               (leap-years-before year))
                            7)))
    (if (> first-day-dow 3)
        (- 7 first-day-dow 7)
        (- 7 first-day-dow))))

;;; Accessors

(defgeneric date-year (x)
  (:method ((x date))
    (let ((stamp (datestamp x)))
      (loop for t1 from (truncate stamp 365) downto 0
            for t2 = (+ (* 365 t1) (leap-years-before (+ 1900 t1)))
            for t3 = (- stamp t2)
            while (< t3 0)
            finally (return (values (+ 1900 t1) t3))))))

(defgeneric date-month (x)
  (:method ((x date))
    (multiple-value-bind (year rem)
        (date-year x)
      (let* ((accumulative-days
              (if (leap-year-p year)
                  *days-before-month-leap-year*
                  *days-before-month*)))
        ;; Find the last month whose accumulative days is smaller than
        ;; the remaining of the year.
        (loop for i from (1- (length accumulative-days)) downto 0
              as t1 = (elt accumulative-days i)
              as t2 = (- rem t1)
              while (< t2 0)
              finally (return (values i t2)))))))

;; Start from 0
(defun %date-week (x)
  (floor (- (date-day-of-year x) (year-first-week (date-year x)))
         7))

(defgeneric date-week (x)
  (:method ((x date))
    (1+ (%date-week x))))

(defgeneric date-day (x)
  (:method ((x date))
    (multiple-value-bind (ign rem)
        (date-month x)
      (declare (ignore ign))
      (1+ rem))))

;; Begins in 0 for Monday
(defgeneric date-day-of-week (x)
  (:method ((x date))
    (mod (datestamp x) 7)))

;;; Begins in 0
(defgeneric date-day-of-year (x)
  (:method ((x date))
    (- (datestamp x)
       (datestamp (make-date 1 1 (date-year x))))))

(define-transitive-relation date= (x y)
  (= (datestamp x) (datestamp y)))

(define-transitive-relation date< (x y)
  (< (datestamp x) (datestamp y)))

(define-transitive-relation date<= (x y)
  (<= (datestamp x) (datestamp y)))

(define-transitive-relation date> (x y)
  (> (datestamp x) (datestamp y)))

(define-transitive-relation date>= (x y)
  (>= (datestamp x) (datestamp y)))

(defun date+ (date durspec)
  (let* ((dur (duration durspec))
         (days (%duration-days dur))
         (secs (%duration-seconds dur)))
    (unless (zerop secs)
      (error "The duration ~a is not multiple of days" dur))
    (make-instance 'date :datestamp (+ (datestamp date) days))))

(defun date- (date durspec)
  (let* ((dur (duration durspec))
         (days (%duration-days dur))
         (secs (%duration-seconds dur)))
    (unless (zerop secs)
      (error "The duration ~a is not multiple of days" dur))
    (make-instance 'date :datestamp (- (datestamp date) days))))

(defmethod format-value ((date date) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~4,'0d~2,'0d~2,'0d"
          (date-year date)
          (date-month date)
          (date-day date)))

(defmethod parse-value (string (type (eql 'date)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (unless (= (length string) 8)
    (error "parse error."))
  (make-date (parse-unsigned-integer string :start 6 :end 8)
             (parse-unsigned-integer string :start 4 :end 6)
             (parse-unsigned-integer string :start 0 :end 4)))


;;; Time

(defclass time ()
  ((timestamp
    :initarg :timestamp
    :reader timestamp)))

(defun make-time (hour minute second)
  (make-instance 'time :timestamp (+ (* hour 3600) (* minute 60) second)))

(define-predicate-type time)

(defmethod print-object ((x time) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d:~2,'0d:~2,'0d"
            (time-hour   x)
            (time-minute x)
            (time-second x))))

(defgeneric time-hour (x)
  (:method ((x time))
    (truncate (timestamp x) 3600)))

(defgeneric time-minute (x)
  (:method ((x time))
    (mod (truncate (timestamp x) 60) 60)))

(defgeneric time-second (x)
  (:method ((x time))
    (mod (timestamp x) 60)))

(define-transitive-relation time= (x y)
  (= (timestamp x) (timestamp y)))

(define-transitive-relation time< (x y)
  (< (timestamp x) (timestamp y)))

(define-transitive-relation time<= (x y)
  (<= (timestamp x) (timestamp y)))

(define-transitive-relation time> (x y)
  (> (timestamp x) (timestamp y)))

(define-transitive-relation time>= (x y)
  (>= (timestamp x) (timestamp y)))

(defun time+ (time durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (let ((tstamp (+ (timestamp time) sec)))
      (unless (zerop day)
        (error "The duration cannot specify a number of days"))
      (values (make-instance 'time :timestamp (mod tstamp 86400))
              (- (truncate tstamp 86400)
                 (if (< tstamp 0) 1 0))))))

(defun time- (time durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (let ((tstamp (- (timestamp time) sec)))
      (unless (zerop day)
        (error "The duration cannot specify a number of days"))
      (values (make-instance 'time :timestamp (mod tstamp 86400))
              (- (truncate tstamp 86400)
                 (if (< tstamp 0) 1 0))))))

(defmethod format-value ((time time) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~2,'0d~2,'0d~2,'0d"
          (time-hour   time)
          (time-minute time)
          (time-second time)))

(defmethod parse-value (string (type (eql 'time)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (unless (or (= (length string) 6)
              (= (length string) 7))
    (error "parse error."))
  (make-time (parse-unsigned-integer string :start 0 :end 2)
             (parse-unsigned-integer string :start 2 :end 4)
             (parse-unsigned-integer string :start 4 :end 6)))


;;;; Datetime

(deftype date-time () 'datetime)

(defclass datetime ()
  ((datetimestamp
    :initarg :datetimestamp
    :accessor datetimestamp)))

(define-predicate-type datetime)

;;; TODO: The TZONE argument will be implemented when the module
;;; components is ready.
(defun make-datetime (day month year hour minute second &optional tzone)
  (declare (ignore tzone))
  (make-instance 'datetime
                 :datetimestamp (encode-universal-time second minute hour day month year)))

(defmethod print-object ((x datetime) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
            (date-day x)
            (date-month x)
            (date-year x)
            (time-hour x)
            (time-minute x)
            (time-second x))))

(defmethod date-day ((x datetime))
  (nth-value 3 (decode-universal-time (datetimestamp x))))

(defmethod date-day-of-week ((x datetime))
  (nth-value 6 (decode-universal-time (datetimestamp x))))

(defmethod date-month ((x datetime))
  (nth-value 4 (decode-universal-time (datetimestamp x))))

(defmethod date-year ((x datetime))
  (nth-value 5 (decode-universal-time (datetimestamp x))))

(defmethod time-hour ((x datetime))
  (nth-value 2 (decode-universal-time (datetimestamp x))))

(defmethod time-minute ((x datetime))
  (nth-value 1 (decode-universal-time (datetimestamp x))))

(defmethod time-second ((x datetime))
  (nth-value 0 (decode-universal-time (datetimestamp x))))


;;; Relational functions

(define-transitive-relation datetime= (x y)
  (= (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime< (x y)
  (< (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime<= (x y)
  (<= (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime> (x y)
  (> (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime>= (x y)
  (>= (datetimestamp x) (datetimestamp y)))

;; Compositional functions

;; Assume hour = 60 min = 3600 s
(defun datetime+ (datetime durspec)
  (let* ((%year (date-year datetime))
         (month (date-month datetime))
         (%day-offset (+ (if (leap-year-p %year)
                             (elt *days-before-month-leap-year* month)
                             (elt *days-before-month* month))
                         (1- (date-day datetime))))
         (%second-offset (+ (* 60 (time-minute datetime))
                            (* 3600 (time-hour datetime)))))
    (multiple-value-bind (time overflow-days)
        (time+ (make-instance 'time :timestamp %second-offset)
               (make-instance 'duration :seconds (%duration-seconds durspec)))
      (setf %second-offset (timestamp time))
      (incf %day-offset overflow-days)
      (let ((date (make-date-offset-year %year (+ %day-offset (%duration-days durspec)))))
        (make-datetime (date-day date)
                       (date-month date)
                       (date-year date)
                       (time-hour time)
                       (time-minute time)
                       (time-second time))))))

(defun datetime- (datetime durspec)
  (datetime+ datetime (duration-inverse durspec)))

;; TODO:  Implement this for rest orders
(defun duration-in (to from order)
  (ecase order
    (:seconds (- (datetimestamp to) (datetimestamp from)))
    (:days
       (- (datestamp (make-date (date-day to)
                                (date-month to)
                                (date-year to)))
          (datestamp (make-date (date-day from)
                                (date-month from)
                                (date-year from)))))
    (:month
       (+ (* 12 (duration-in to from :years))
          (- (date-month to) (date-month from))))
    (:years (- (date-year to) (date-year from)))))

;;; Parser

(defmethod format-value ((dt datetime) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil
          "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
          (date-year dt)
          (date-month dt)
          (date-day dt)
          (time-hour dt)
          (time-minute dt)
          (time-second dt)))

(defmethod parse-value (string (type (eql 'datetime)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  ;; TODO: Handling timezones
  (let ((string-date (subseq string 0  8))
        (string-time (subseq string 9 15)))
    (flet ((ill-formed ()
             (error "Bad datetime format.")))
      (unless (char= (elt string 8) #\T)
        (ill-formed))
      (let ((date (parse-value string-date 'date))
            (time (parse-value string-time 'time)))
        (make-datetime (date-day    date)
                       (date-month  date)
                       (date-year   date)
                       (time-hour   time)
                       (time-minute time)
                       (time-second time))))))


;;;; Duration data type

(defclass duration ()
  ((days
    :initarg :days
    :initform 0
    :reader %duration-days)
   (seconds
    :initarg :seconds
    :initform 0
    :reader %duration-seconds)))

(define-predicate-type duration)

(defun make-duration (&key (days 0) (hours 0) (minutes 0) (seconds 0) backward-p)
  (declare (type (integer 0 *) days hours minutes seconds))
  (make-instance 'duration
                 :days (if backward-p (- days) days)
                 :seconds (* (if backward-p -1 1)
                             (+ (* hours 3600)
                                (* minutes 60)
                                seconds))))

(defun duration (durspec)
  (etypecase durspec
    (duration durspec)
    (string (parse-value durspec 'duration))))


;;; Accessor for duration designators

(defgeneric duration-days (duration)
  (:method ((x duration))
    (abs (%duration-days x))))

(defgeneric duration-hours (duration)
  (:method ((x duration))
    (idiv (abs (%duration-seconds x)) 3600)))

(defgeneric duration-minutes (duration)
  (:method ((x duration))
    (mod (abs (idiv (%duration-seconds x) 60)) 60)))

(defgeneric duration-seconds (duration)
  (:method ((x duration))
    (mod (abs (%duration-seconds x)) 60)))

(defgeneric duration-backward-p (duration)
  (:method ((x duration))
    (< (%duration-seconds x) 0)))

(defmethod duration-days ((x string))
  (duration-days (duration x)))

(defmethod duration-hours ((x string))
  (duration-hours (duration x)))

(defmethod duration-minutes ((x string))
  (duration-minutes (duration x)))

(defmethod duration-seconds ((x string))
  (duration-seconds (duration x)))

(defmethod duration-backward-p ((x string))
  (duration-backward-p (duration x)))

(defgeneric duration-inverse (duration)
  (:method ((d1 duration))
    (make-instance 'duration
                   :days    (- (%duration-days    d1))
                   :seconds (- (%duration-seconds d1)))))

;;; Printer
(defvar *print-duration-abbrev* nil)

(defmethod print-object ((x duration) stream)
  (print-unreadable-object (x stream :type t)
    (let* ((component-names
            (if *print-duration-abbrev*
                '("d"   "h"    "m"      "s")
                '("day" "hour" "minute" "second")))
           (output
            (loop for c in component-names
                  for n in (list (duration-days    x)
                                 (duration-hours   x)
                                 (duration-minutes x)
                                 (duration-seconds x))
                  unless (zerop n)
                    collect n and collect c)))
      (cond
        ((null output)
         (format stream "empty duration"))
        (*print-duration-abbrev*
         (format stream "~{~d~a~^ ~}" output))
        (t
         (format stream "~{~d ~a~2:*~p~*~#[~;~; and ~:;, ~]~}" output)))

      (when (duration-backward-p x)
        (format stream " to BACKWARD")))))


;;; Return a string which stand for DURSPECS in the format described
;;; in the RFC5545 section 3.3.6.
(defmethod format-value ((dur duration) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (let ((days       (duration-days dur))
        (hours      (duration-hours dur))
        (minutes    (duration-minutes dur))
        (seconds    (duration-seconds dur))
        (backward-p (duration-backward-p dur)))
    (with-output-to-string (out)
      (format out "~:[~;-~]" backward-p)
      (format out "P")
      (cond
        ((and (zerop (%duration-seconds dur))
              (zerop (%duration-days dur)))
         (format out "T0S"))
        ((and (zerop (%duration-seconds dur))
              (divisiblep (%duration-days dur) 7))
         (format out "~aW" (/ days 7)))
        (t
         (unless (zerop days)
           (format out "~aD" days))
         (cond
           ((= 0 hours minutes seconds))
           ((and (zerop minutes)
                 (and (not (zerop hours))
                      (not (zerop seconds))))
            (format out "T~aH~aM~aS" hours minutes seconds))
           (t
            (format out "T~[~:;~:*~aH~]~[~:;~:*~aM~]~[~:;~:*~aS~]"
                    hours
                    minutes
                    seconds))))))))


;;; Parse a duration according to the format which is described in the
;;; RFC5545 section 3.3.6.
(defmethod parse-value (string (type (eql 'duration)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (with-input-from-string (in string)
    (flet ( ;; Get a token from IN
           (get-token ()
             (let ((ch (peek-char nil in nil)))
               (cond
                 ((null ch)
                  nil)
                 ((digit-char-p ch)
                  (values
                    (parse-integer
                     (read-until in (complement #'digit-char-p) "" nil))))
                 (t
                  (read-char in))))))

      (let* ( ;; The following token
             (token1 (get-token))
             ;; The following of the following token
             (token2 (get-token)))

        (labels (
                 ;; Return the following token, then upgrade the
                 ;; values of token1 and token2. If there is not
                 ;; following token, then signal a error.
                 (scan ()
                   (if token1
                       (prog1 token1
                         (setf token1 token2)
                         (setf token2 (get-token)))
                       (ill-formed)))

                 ;; Signal a ill-formed error
                 (ill-formed ()
                   (error "bad formed duration."))

                 ;; Add a duration to other. It is consistent if both
                 ;; D1 and D2 are not backward durations.
                 (duration+ (d1 d2)
                   (make-instance 'duration
                                  :days
                                  (+ (%duration-days d1)
                                     (%duration-days d2))
                                  :seconds
                                  (+ (%duration-seconds d1)
                                     (%duration-seconds d2))))

                 ;; Check the current token is the character CH and
                 ;; read a new token. Otherwise, a error is signaled.
                 (check-character (ch)
                   (unless (char= (scan) ch)
                     (ill-formed)))

                 ;; The following functions stand for a state, which
                 ;; parse a subset of the grammar and return a partial
                 ;; duration object.
                 ;;
                 ;; If the input is not matched then an error is
                 ;; signaled.
                 ;; ...

                 (dur-value ()
                   (funcall (case (scan)
                              (#\+
                                 (check-character #\P)
                                 #'identity)
                              (#\-
                                 (check-character #\P)
                                 #'duration-inverse)
                              (#\P
                                 #'identity)
                              (t
                                 (ill-formed)))
                            (cond
                              ((eql token1 #\T)
                               (dur-time))
                              ((eql token2 #\W)
                               (dur-week))
                              ((eql token2 #\D)
                               (dur-date))
                              (t
                               (ill-formed)))))

                 (dur-date ()
                   (duration+ (dur-day)
                              (if token1
                                  (dur-time)
                                  (make-duration))))

                 (dur-time ()
                   (check-character #\T)
                   (case token2
                     (#\H (dur-hour))
                     (#\M (dur-minute))
                     (#\S (dur-second))
                     (t
                        (ill-formed))))

                 (dur-week ()
                   (prog1 (make-duration :days (* (scan) 7))
                     (check-character #\W)))

                 (dur-hour ()
                   (duration+ (prog1 (make-duration :hours (scan))
                                (check-character #\H))
                              (if token1
                                  (dur-minute)
                                  (make-duration))))

                 (dur-minute ()
                   (duration+ (prog1 (make-duration :minutes (scan))
                                (check-character #\M))
                              (if token1
                                  (dur-second)
                                  (make-duration))))

                 (dur-second ()
                   (prog1 (make-duration :seconds (scan))
                     (check-character #\S)))

                 (dur-day ()
                   (prog1 (make-duration :days (scan))
                     (check-character #\D))))

          ;; Initial state
          (prog1 (duration (dur-value))
            (unless (null token1)
              (ill-formed))))))))




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
    (make-duration
     :seconds (- (datetimestamp (period-start x))
                 (datetimestamp (period-end x))))))

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



;;; types.lisp ends here
