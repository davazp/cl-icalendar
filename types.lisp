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


;;; This file contains code which implement the values that iCalendar
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



;;;; Recur data type

(defclass recur ()
  ((freq
    :initarg :freq
    :initform nil
    :reader recur-freq)
   (until
    :initarg :until
    :initform nil
    :reader recur-until)
   (count
    :initarg :count
    :initform nil
    :reader recur-count)
   (interval
    :initarg :interval
    :initform 1
    :reader recur-interval)
   (bysecond
    :initarg :bysecond
    :initform nil
    :reader recur-bysecond)
   (byminute
    :initarg :byminute
    :initform nil
    :reader recur-byminute)
   (byhour
    :initarg :byhour
    :initform nil
    :reader recur-byhour)
   (byday
    :initarg :byday
    :initform nil
    :reader recur-byday)
   (bymonthday
    :initarg :bymonthday
    :initform nil
    :reader recur-bymonthday)
   (byyearday
    :initarg :byyearday
    :initform nil
    :reader recur-byyearday)
   (byweekno
    :initarg :byweekno
    :initform nil
    :reader recur-byweekno)
   (bymonth
    :initarg :bymonth
    :initform nil
    :reader recur-bymonth)
   (bysetpos
    :initarg :bysetpos
    :initform nil
    :reader recur-bysetpos)
   (wkst
    :initarg :wkst
    :initform nil
    :reader recur-wkst)))

(define-predicate-type recur)

(deftype non-zero-integer (a b)
  `(or (integer ,(- b) ,(- a))
       (integer  ,a  ,b)))

(deftype frequency ()
  '(member
    :secondly
    :minutely
    :hourly
    :daily
    :weekly
    :monthly
    :yearly))

(defun check-valid-recur (recur)
  (with-slots (freq until count interval
                    bysecond byminute byhour
                    byday bymonthday byyearday
                    byweekno bymonth bysetpos
                    wkst)
      recur
    (macrolet ((check-type-list (list type)
                 `(dolist (i ,list)
                    (check-type i ,type))))

      (check-type freq frequency)
      (assert (or (not until) (not count)))

      (and until    (check-type until (or date datetime)))
      (and count    (check-type count (integer 0 *)))
      (and interval (check-type interval (integer 0 *)))

      (check-type-list bysecond (integer 0 60))
      (check-type-list byminute (integer 0 59))
      (check-type-list byhour   (integer 0 23))
      (check-type-list bymonthday (non-zero-integer  -31  31))
      (check-type-list byyearday  (non-zero-integer -366 366))
      (check-type-list byweekno   (non-zero-integer   -7   7))
      (check-type-list bymonth    (integer 0 12))
      (check-type-list bysetpos   (non-zero-integer -366 366))
      (if wkst
          (check-type wkst (integer 0 6))))))


;;; Parsing

(defun parse-rule-part (string)
  (let ((eqpos (position #\= string)))
    (when (null eqpos)
      (error "Bad rule part ~a" string))
    (cons (subseq string 0 eqpos)
          (subseq string (1+ eqpos)))))

(defun parse-rules (string)
  (let ((parts (split-string string ";" nil)))
    (when (some #'null parts)
      (error "Empty rule part in the recurrence '~a'." string))
    (mapcar #'parse-rule-part parts)))

(defmethod parse-value (string (type (eql 'recur)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (let ((rules (parse-rules string))
        (recur (make-instance 'recur)))
    (when (duplicatep rules :key #'car :test #'string=)
      (error "Duplicate key in recurrence."))
    (flet ((parse-integer-list (x)
             (mapcar #'parse-integer (split-string x ",")))
           (parse-unsigned-integer-list (x)
             (mapcar #'parse-unsigned-integer (split-string x ","))))
      
      (dolist (rule rules)
        (destructuring-bind (key . value)
            rule
          (cond
            ((string= key "FREQ")
             (setf (slot-value recur 'freq)
                   (cond
                     ((string= value "SECONDLY") :secondly)
                     ((string= value "MINUTELY") :minutely)
                     ((string= value "HOURLY")   :hourly)
                     ((string= value "DAILY")    :daily)
                     ((string= value "WEEKLY")   :weekly)
                     ((string= value "MONTHLY")  :monthly)
                     ((string= value "YEARLY")   :yearly)
                     (t
                      (error "'~a' is not a valid value for the FREQ rule." value)))))
            
            ((string= key "UNTIL")
             ;; TODO: Implement this
             )
            
            ((string= key "COUNT")
             (setf (slot-value recur 'count)
                   (parse-unsigned-integer value)))
            
            ((string= key "INTERVAL")
             (setf (slot-value recur 'interval)
                   (parse-unsigned-integer value)))
            
            ((string= key "BYSECOND")
             (setf (slot-value recur 'bysecond)
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYMINUTE")
             (setf (slot-value recur 'byminute)
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYHOUR")
             (setf (slot-value recur 'byhour)
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYDAY")
             (setf (slot-value recur 'byday)
                   (parse-unsigned-integer-list value)))

            ((string= key "BYMONTH")
             (setf (slot-value recur 'bymonth)
                   (parse-integer-list value)))
            
            ((string= key "BYMONTHDAY")
             (setf (slot-value recur 'bymonthday)
                   (parse-integer-list value)))
            
            ((string= key "BYYEARDAY")
             (setf (slot-value recur 'byyearday)
                   (parse-integer-list value)))
            
            ((string= key "BYWEEKNO")
             (setf (slot-value recur 'byyearday)
                   (parse-integer-list value)))
            
            ((string= key "BYSETPOS")
             (setf (slot-value recur 'bysetpos)
                   (parse-unsigned-integer value )))
            
            ((string= key "WKST")
             (setf (slot-value recur 'wkst)
                   (cond
                     ((string= value "SECONDLY") :secondly)
                     ((string= value "MINUTELY") :minutely)
                     ((string= value "HOURLY")   :hourly)
                     ((string= value "DAILY")    :daily)
                     ((string= value "WEEKLY")   :weekly)
                     ((string= value "MONTHLY")  :monthly)
                     ((string= value "YEARLY")   :yearly))))
            
            (t
             (error "Unknown recurrence component ~a" key)))))

      ;; Return the recur instance
      (check-valid-recur recur)
      recur)))

(defmethod format-value ((recur recur) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (with-output-to-string (s)
    (format s "FREQ=~a"
            (case (recur-freq recur)
              (:secondly "SECONDLY")
              (:minutely "MINUTELY")
              (:hourly "HOURLY")
              (:daily "DAILY")
              (:monthly "MONTHLY")
              (:yearly "YEARLY")))
    ;; Print optional recur slots.
    (format s "~@[;COUNT=~a~]"              (recur-count recur))
    (format s "~@[;UNTIL=~a~]"              (recur-until recur))
    (format s "~@[;BYSECOND=~{~A~^, ~}~]"   (recur-bysecond recur))
    (format s "~@[;BYMINUTE=~{~A~^, ~}~]"   (recur-byminute recur))
    (format s "~@[;BYHOUR=~{~A~^, ~}~]"     (recur-byhour recur))
    (format s "~@[;BYDAY=~{~A~^, ~}~]"      (recur-byday recur))
    (format s "~@[;BYMONTH=~{~A~^, ~}~]"    (recur-bymonth recur))
    (format s "~@[;BYMONTHDAY=~{~A~^, ~}~]" (recur-bymonthday recur))
    (format s "~@[;BYYEARDAY=~{~A~^, ~}~]"  (recur-byyearday recur))
    (format s "~@[;BYWEEKNO=~{~A~^, ~}~]"   (recur-byweekno recur))
    (format s "~@[;BYSETPOS=~{~A~^, ~}~]"   (recur-bysetpos recur))))

;; TODO: Implementation pending
(defun recur-instances nil t)
(defun %unbound-recur-next-instance nil t)
(defun %unbound-recur-initial-instance nil t)

;; Check if DATETIME is a valid ocurrence in RECUR beginning at
;; DTSTART datetime.
(defun %unbound-recur-instance-p (start recur datetime)
  (macrolet ((implyp (condition implication)
               `(aif ,condition ,implication t)))
    (if (recur-count recur)
        ;; TODO: Implementation pending
        (error "Implementation pending"))
    (and
     (aif (recur-bysecond recur)
          (find (time-second datetime) it)
          (implyp (neq :secondly (recur-freq recur))
                  (= (time-second datetime) (time-second start))))
           
     (aif (recur-byminute recur)
          (find (time-minute datetime) it)
          (implyp (neq :minutely (recur-freq recur))
                  (= (time-minute datetime) (time-minute start))))
           
     (aif (recur-byhour recur)
          (find (time-hour datetime) it)
          (implyp (neq :hourly (recur-freq recur))
                  (= (time-hour datetime) (time-hour start))))
           
     (aif (recur-byday recur)
          (find  (date-day-of-week datetime) it)
          (implyp (neq :daily (recur-freq recur))
                  (= (date-day datetime) (date-day start))))
           
     (aif (recur-bymonth recur)
          (or (find (date-month datetime) it)
              (find (- 11 (date-month datetime)) it))
          (implyp (neq :montly (recur-freq recur))
                  (= (date-month datetime) (date-month start))))
           
     (aif (recur-bymonthday recur)
          (let* ((month-days (if (leap-year-p (date-year datetime))
                                 *days-in-month-leap-year*
                                 *days-in-month*))
                 (negative-dom (- (elt month-days (date-day datetime))
                                  (date-day datetime)
                                  1)))
            (or (find (date-day datetime) it)
                (find negative-dom it))))
           
     (implyp (recur-byyearday recur)
             (let ((negative-doy (- (if (leap-year-p (date-year datetime))
                                        366
                                        365)
                                    (date-day-of-year datetime)
                                    1)))
               (or (find (date-day-of-year datetime) it)
                   (find negative-doy it))))
           
     (implyp (recur-byweekno recur)
             ;; TODO: Implement suport for negative weeks
             (find (date-week datetime) it)))))


;;; types.lisp ends here
