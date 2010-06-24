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

;;; Type checker.
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
         (secs (- (seconds-from-1900 end) (seconds-from-1900 start))))
    (if (datetime<= start end)
        (make-duration :seconds secs)
        (make-duration :seconds secs :backward-p t))))

(defmethod period-end ((period period-start))
  (datetime+ (period-start period) (period-duration period)))

;;; Constructor
(defun make-period (start duration-or-end)
  (etypecase duration-or-end
    (duration (make-instance 'period-start    :start start :duration duration-or-end))
    (datetime (make-instance 'period-explicit :start start :end duration-or-end))))

(defmethod print-object ((x period) stream)
  (print-unreadable-object (x stream)
    (flet ((write-datetime (dt stream)
             (format stream "~2,'0d-~2,'0d-~4,'0d ~2,'0d:~2,'0d:~2,'0d"
                     (date-day  dt) (date-month  dt) (date-year dt)
                     (time-hour dt) (time-minute dt) (time-hour dt))))
      (write-string "PERIOD " stream)
      (write-datetime (period-start x) stream)
      (write-string " -- " stream)
      (write-datetime (period-end x) stream))))

(defmethod format-value ((p period-explicit) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (concat (format-value (period-start p)) "/" (format-value (period-end p))))

(defmethod format-value ((p period-start) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (concat (format-value (period-start p)) "/" (format-value (period-duration p))))

(defmethod parse-value (string (type (eql 'period)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (destructuring-bind (start end)
      (split-string string "/")
    (list start end)
    (let ((dstart (parse-value start 'datetime)))
      (make-period dstart
                   (if (char= (char end 0) #\P)
                       (datetime+ dstart (parse-value end 'duration))
                       (parse-value end 'datetime))))))

;;; types-period.lisp ends here
