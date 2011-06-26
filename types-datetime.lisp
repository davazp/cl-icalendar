;; types-datetime.lisp ---
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

(defclass datetime ()
  ((timestamp
    :initarg :timestamp
    :initform (required-arg)
    :reader datetime-timestamp)))

(defclass float-datetime (datetime)
  nil)

(defclass utc-datetime (datetime)
  nil)

(defclass local-datetime (datetime)
  nil)

(register-ical-value datetime :name "DATE-TIME")
(define-predicate-type datetime)

(defun make-datetime (day month year hour minute second &optional (form :local))
  (declare (type (integer 1 31) day)
           (type (integer 1 12) month)
           (type integer year)
           (type (integer 0 23) hour)
           (type (integer 0 59) minute)
           (type (integer 0 60) second))
  (let ((local-time:*default-timezone*
         (if (eq form :utc)
             local-time:+utc-zone+
             local-time:*default-timezone*)))
    (let ((timestamp (local-time:encode-timestamp 0 second minute hour day month year)))
      (ecase form
        (:utc
         (make-instance 'utc-datetime :timestamp timestamp))
        (:local
         (make-instance 'local-datetime :timestamp timestamp))
        (:float
         (make-instance 'float-datetime :timestamp timestamp))))))


(defmethod date-day ((x datetime))
  (local-time:timestamp-day (datetime-timestamp x)))

(defmethod date-month ((x datetime))
  (local-time:timestamp-month (datetime-timestamp x)))

(defmethod date-year ((x datetime))
  (local-time:timestamp-year (datetime-timestamp x)))

(defmethod time-second ((x datetime))
  (local-time:timestamp-second (datetime-timestamp x)))

(defmethod time-minute ((x datetime))
  (local-time:timestamp-minute (datetime-timestamp x)))

(defmethod time-hour ((x datetime))
  (local-time:timestamp-hour (datetime-timestamp x)))


(defprinter (x datetime)
  (format t "~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
          (date-day x)
          (date-month x)
          (date-year x)
          (time-hour x)
          (time-minute x)
          (time-second x)))

(defvar +timestamp-1900+
  (local-time:encode-timestamp 0 0 0 0 1 1 1900))

(defgeneric day-from-1900 (dt)
  (:method ((dt datetime))
    (1+ (- (local-time:day-of (datetime-timestamp dt))
           (local-time:day-of +timestamp-1900+)))))

(defgeneric seconds-from-1900 (dt)
  (:method ((dt datetime))
    (local-time:timestamp-difference dt +timestamp-1900+)))

;;; Relational functions

(define-transitive-relation datetime= (x y)
  (local-time:timestamp= (datetime-timestamp x)
                         (datetime-timestamp y)))

(define-transitive-relation datetime< (x y)
  (local-time:timestamp< (datetime-timestamp x)
                         (datetime-timestamp y)))

(define-transitive-relation datetime<= (x y)
  (local-time:timestamp<= (datetime-timestamp x)
                          (datetime-timestamp y)))

(define-transitive-relation datetime> (x y)
  (local-time:timestamp> (datetime-timestamp x)
                         (datetime-timestamp y)))

(define-transitive-relation datetime>= (x y)
  (local-time:timestamp>= (datetime-timestamp x)
                          (datetime-timestamp y)))


;; Compositional functions

(defgeneric adjust-datetime (dt &key day month year hour minute second))

(defmethod adjust-datetime ((dt datetime) &key day month year hour minute second)
  (make-datetime (or day (date-day dt))
                 (or month (date-month dt))
                 (or year (date-year dt))
                 (or hour (time-hour dt))
                 (or minute (time-minute dt))
                 (or second (time-second dt))
                 (etypecase dt
                   (local-datetime :local)
                   (utc-datetime :utc)
                   (float-datetime :float))))

(defmethod adjust-date ((dt datetime) &key day month year)
  (make-datetime (or day (date-day dt))
                 (or month (date-month dt))
                 (or year (date-year dt))
                 (time-hour dt)
                 (time-minute dt)
                 (time-second dt)
                 (etypecase dt
                   (local-datetime :local)
                   (utc-datetime :utc)
                   (float-datetime :float))))

(defmethod adjust-time ((dt datetime) &key hour minute second)
  (make-datetime (date-day dt)
                 (date-month dt)
                 (date-year dt)
                 (or hour (time-hour dt))
                 (or minute (time-minute dt))
                 (or second (time-second dt))
                 (etypecase dt
                   (local-datetime :local)
                   (utc-datetime :utc)
                   (float-datetime :float))))



(defun %datetime+ (datetime durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (let ((timestamp (datetime-timestamp datetime)))
      (local-time:timestamp+ (local-time:timestamp+ timestamp day :day) sec :sec))))

(defgeneric datetime+ (datetime durspec)
  (:method ((datetime local-datetime) durspec)
    (make-instance 'local-datetime :timestamp (%datetime+ datetime durspec)))
  (:method ((datetime utc-datetime) durspec)
    (make-instance 'utc-datetime :timestamp (%datetime+ datetime durspec)))
  (:method ((datetime float-datetime) durspec)
    (make-instance 'float-datetime :timestamp (%datetime+ datetime durspec))))

(defgeneric datetime- (datetime durspec)
  (:method ((datetime datetime) durspec)
    (datetime+ datetime (duration-inverse durspec))))

(defmethod date+ ((dt datetime) durspec)
  (let ((duration (duration durspec)))
    (unless (zerop (%duration-seconds duration))
      (error "Duration was expected to be a multiple of days."))
    (datetime+ dt duration)))

(defmethod date- ((dt datetime) durspec)
  (let ((duration (duration durspec)))
    (unless (zerop (%duration-seconds duration))
      (error "Duration was expected to be a multiple of days."))
    (datetime- dt duration)))

(defmethod time+ ((dt datetime) durspec)
  (let ((duration (duration durspec)))
    (unless (zerop (%duration-days duration))
      (error "Duration was expected to be a multiple of seconds."))
    (datetime- dt duration)))

(defmethod time- ((dt datetime) durspec)
  (let ((duration (duration durspec)))
    (unless (zerop (%duration-days duration))
      (error "Duration was expected to be a multiple of seconds."))
    (datetime- dt duration)))


(defun now (&optional (form :local))
  (ecase form
    (:local
     (make-instance 'local-datetime :timestamp (local-time:now)))
    (:utc
     (make-instance 'utc-datetime :timestamp (local-time:now)))
    (:float
     (make-instance 'float-datetime :timestamp (local-time:now)))))

;;; Parser

(defmethod format-value ((dt datetime) &optional params)
  (declare (ignore params))
  (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
          (date-year dt)
          (date-month dt)
          (date-day dt)
          (time-hour dt)
          (time-minute dt)
          (time-second dt)))

(defmethod parse-value (string (type (eql 'datetime)) &optional params)
  (declare (ignore params))
  (flet ((ill-formed () (%parse-error "Bad datetime format.")))
    ;; We want to signal an iCalendar error indeed of delegate to
    ;; subseq, which will emit an arbitrary error.
    (when (< (length string) 9)
      (ill-formed))
    (let ((string-date (subseq string 0  8))
          (string-time (subseq string 9)))
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

;;; types-datetime.lisp ends here
