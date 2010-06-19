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

(deftype date-time () 'datetime)

(defclass datetime (date time)
  nil)

(define-predicate-type datetime)

;;; TODO: The TZONE argument will be implemented when the module
;;; components is ready.
(defun make-datetime (day month year hour minute second &optional tzone)
  (declare (ignore tzone))
  (make-instance 'datetime
                 :day-from-1900
                 (day-from-1900 (make-date day month year))
                 :seconds-from-midnight
                 (seconds-from-midnight (make-time hour minute second))))

(defmethod print-object ((x datetime) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
            (date-day x)
            (date-month x)
            (date-year x)
            (time-hour x)
            (time-minute x)
            (time-second x))))

;;; Relational functions

(define-transitive-relation datetime= (x y)
  (and (= (day-from-1900 x) (day-from-1900 y))
       (= (seconds-from-midnight x) (seconds-from-midnight y))))

(define-transitive-relation datetime< (x y)
  (or (< (day-from-1900 x) (day-from-1900 y))
      (and (= (day-from-1900 x) (day-from-1900 y))
           (< (seconds-from-midnight x) (seconds-from-midnight y)))))

(define-transitive-relation datetime<= (x y)
  (or (< (day-from-1900 x) (day-from-1900 y))
      (and (= (day-from-1900 x) (day-from-1900 y))
           (<= (seconds-from-midnight x) (seconds-from-midnight y)))))

(define-transitive-relation datetime> (x y)
  (or (> (day-from-1900 x) (day-from-1900 y))
      (and (= (day-from-1900 x) (day-from-1900 y))
           (> (seconds-from-midnight x) (seconds-from-midnight y)))))

(define-transitive-relation datetime>= (x y)
  (or (> (day-from-1900 x) (day-from-1900 y))
      (and (= (day-from-1900 x) (day-from-1900 y))
           (>= (seconds-from-midnight x) (seconds-from-midnight y)))))

(defgeneric seconds-from-1900 (dt)
  (:method ((dt datetime))
    (+ (* (day-from-1900 dt) 60 60 24)
       (seconds-from-midnight dt))))

;; Compositional functions

(defun datetime+ (datetime durspec)
  (let ((duration durspec))
    (let ((delta-days (duration-days duration))
          (delta-secs (+ (* 3600 (duration-hours   duration))
                         (*   60 (duration-minutes duration))
                         (*    1 (duration-seconds duration))))
          (days (day-from-1900 datetime))
          (secs (seconds-from-midnight datetime)))
      (if (duration-backward-p duration)
          (make-instance 'datetime
                         :day-from-1900 (- days delta-days)
                         :seconds-from-midnight (- secs delta-secs))
          (make-instance 'datetime
                         :day-from-1900 (+ days delta-days)
                         :seconds-from-midnight (+ secs delta-secs))))))

(defun datetime- (datetime durspec)
  (datetime+ datetime (duration-inverse durspec)))


(defun now ()
  ;; FIXME: timezone support!
  (multiple-value-bind (second minute hour date month year day daylight timezone)
      (get-decoded-time)
    (declare (ignore daylight day timezone))
    (make-datetime date month year hour minute second)))

(defun adjust-datetime (dt &key day month year hour minute second timezone)
  (make-datetime (or day (date-day dt))
                 (or month (date-month dt))
                 (or year (date-year dt))
                 (or hour (time-hour dt))
                 (or minute (time-minute dt))
                 (or second (time-second dt))
                 (or timezone (time-timezone dt))))

;;; Parser

(defmethod format-value ((dt datetime) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (format nil "~4,'0d~2,'0d~2,'0dT~2,'0d~2,'0d~2,'0d"
          (date-year dt)
          (date-month dt)
          (date-day dt)
          (time-hour dt)
          (time-minute dt)
          (time-second dt)))

(defmethod parse-value (string (type (eql 'datetime)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  ;; TODO: Handling timezones
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
