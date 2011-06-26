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

(defclass time ()
  ((timestamp
    :initarg :timestamp
    :initform (required-arg)
    :reader time-timestamp)))

(defclass float-time (time)
  nil)

(defclass utc-time (time)
  nil)

(defclass local-time (time)
  nil)

(defun !truncate-timestamp (x)
  (setf (local-time::day-of x) 0)
  x)

(defun make-time (hour minute second &optional (form :local))
  (declare (type (integer 0 23) hour)
           (type (integer 0 59) minute)
           (type (integer 0 60) second))
  (let ((local-time:*default-timezone*
         (if (eq form :utc)
             local-time:+utc-zone+
             local-time:*default-timezone*)))
    (let ((timestamp (local-time:encode-timestamp 0 second minute hour 1 3 2000)))
      (!truncate-timestamp timestamp)
      (ecase form
        (:utc
         (make-instance 'utc-time :timestamp timestamp))
        (:local
         (make-instance 'local-time :timestamp timestamp))
        (:float
         (make-instance 'float-time :timestamp timestamp))))))


(register-ical-value time)
(define-predicate-type time)

(defprinter (x time)
  (format t "~2,'0d:~2,'0d:~2,'0d"
          (time-hour   x)
          (time-minute x)
          (time-second x)))

(defprinter (x utc-time)
  (let ((local-time:*default-timezone* local-time:+utc-zone+))
    (format t "~2,'0d:~2,'0d:~2,'0d"
            (time-hour   x)
            (time-minute x)
            (time-second x)))
  (format t "  [~2,'0d:~2,'0d:~2,'0d]"
            (time-hour   x)
            (time-minute x)
            (time-second x)))


(defgeneric time-hour (x)
  (:method ((x time))
    (local-time:timestamp-hour (time-timestamp x))))

(defgeneric time-minute (x)
  (:method ((x time))
    (local-time:timestamp-minute (time-timestamp x))))

(defgeneric time-second (x)
  (:method ((x time))
    (local-time:timestamp-second (time-timestamp x))))

(define-transitive-relation time= (x y)
  (local-time:timestamp= (time-timestamp x) (time-timestamp y)))

(define-transitive-relation time< (x y)
  (local-time:timestamp< (time-timestamp x) (time-timestamp y)))

(define-transitive-relation time<= (x y)
  (local-time:timestamp<= (time-timestamp x) (time-timestamp y)))

(define-transitive-relation time> (x y)
  (local-time:timestamp> (time-timestamp x) (time-timestamp y)))

(define-transitive-relation time>= (x y)
  (local-time:timestamp>= (time-timestamp x) (time-timestamp y)))


;;; Add DURSPEC to TIME and return a new local-time timestamp.
(defun %time+ (time durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (unless (zerop day)
      (%parse-error "The duration cannot specify a number of days"))
    (let ((timestamp (time-timestamp time)))
      (!truncate-timestamp (local-time:timestamp+ timestamp sec :sec)))))

(defgeneric time+ (time durspec)
  (:method ((time local-time) durspec)
    (make-instance 'local-time :timestamp (%time+ time durspec)))
  (:method ((time utc-time) durspec)
    (make-instance 'utc-time :timestamp (%time+ time durspec)))
  (:method ((time float-time) durspec)
    (make-instance 'float-time :timestamp (%time+ time durspec))))

(defgeneric time- (time durspec)
  (:method ((time time) durspec)
    (time+ time (duration-inverse (duration durspec)))))

(defgeneric adjust-time (time &key second minute hour)
  (:method ((time time) &key second minute hour)
    (make-time (or hour (time-hour time))
               (or minute (time-minute time))
               (or second (time-second time)))))

(defmethod format-value ((time time) &optional params)
  (declare (ignore params))
  (format nil "~2,'0d~2,'0d~2,'0d"
          (time-hour   time)
          (time-minute time)
          (time-second time)))

(defmethod parse-value (string (type (eql 'time)) &optional params)
  (cond
    ((= (length string) 6)
     (let ((tzid (parameter "TZID" params)))
       (if tzid
           ;; TODO: Bind the default timezone here when the support
           ;; for timezones is finished.
           (make-time (parse-unsigned-integer string :start 0 :end 2)
                      (parse-unsigned-integer string :start 2 :end 4)
                      (parse-unsigned-integer string :start 4 :end 6)
                      :local)
           (make-time (parse-unsigned-integer string :start 0 :end 2)
                      (parse-unsigned-integer string :start 2 :end 4)
                      (parse-unsigned-integer string :start 4 :end 6)
                      :float))))
    ;; UTC
    ((= (length string) 7)
     (unless (char= (char string 6) #\Z)
       (%parse-error "Last character must be Z to specify UTC time."))
     (make-time (parse-unsigned-integer string :start 0 :end 2)
                (parse-unsigned-integer string :start 2 :end 4)
                (parse-unsigned-integer string :start 4 :end 6)
                :utc))
    (t
     (%parse-error "parse error."))))


;;; types-time.lisp ends here
