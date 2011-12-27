;; types-period.lisp ---
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

;;; This clase should not be instantiated directly.
(defclass period ()
  ((start
    :initarg :start
    :type datetime
    :reader period-start)))

(register-ical-value period)
(define-predicate-type period)

;;; Period between two explicit datetimes.
(defclass period-explicit (period)
  ((end
    :initarg :end
    :type datetime
    :reader period-end)))

;;; Period from a dattetime for a duration.
(defclass period-start (period)
  ((duration
    :initarg :duration
    :type duration
    :reader period-duration)))

(defmethod period-duration ((period period-explicit))
  (let* ((start (period-start period))
         (end (period-end period))
         (secs (seconds-between end start)))
    (if (<= start end)
        (make-duration :seconds secs)
        (make-duration :seconds secs :backward-p t))))

(defmethod period-end ((period period-start))
  (let ((dur (period-duration period)))
    (if (duration-backward-p dur)
        (datetime+ (period-start period)
                   (- (duration-days dur))
                   (- (duration-seconds dur)))
        (datetime+ (period-start period)
                   (duration-days dur)
                   (duration-seconds dur)))))

;;; Constructor
(defun make-period (start duration-or-end)
  (etypecase duration-or-end
    (duration (make-instance 'period-start    :start start :duration duration-or-end))
    (datetime (make-instance 'period-explicit :start start :end duration-or-end))))

(defprinter (x period)
  (flet ((write-datetime (dt)
           (format t "~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
                   (date-day  dt) (date-month  dt) (date-year dt)
                   (time-hour dt) (time-minute dt) (time-hour dt))))
    (write-string "PERIOD ")
    (write-datetime (period-start x))
    (write-string " -- ")
    (write-datetime (period-end x))))

(defmethod format-value ((p period-explicit) (type (eql 'period)) &optional params)
  (declare (ignore params))
  (concat (format-value (period-start p) 'datetime)
          "/"
          (format-value (period-end p) 'datetime)))

(defmethod format-value ((p period-start) (type (eql 'period)) &optional params)
  (declare (ignore params))
  (concat (format-value (period-start p) 'datetime)
          "/"
          (format-value (period-duration p) 'duration)))

(defmethod parse-value (string (type (eql 'period)) &optional params)
  (declare (ignore params))
  (destructuring-bind (start end)
      (split-string string "/")
    (list start end)
    (let ((dstart (parse-value start 'datetime)))
      (make-period dstart
                   (if (char= (char end 0) #\P)
                       (parse-value end 'duration)
                       (parse-value end 'datetime))))))

;;; types-period.lisp ends here
