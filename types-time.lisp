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
    :initarg :seconds-from-midnight
    :reader seconds-from-midnight)
   (timezone
    :initarg :timezone
    :initform nil
    :reader time-timezone)))

(defun make-time (hour minute second)
  (make-instance 'time :seconds-from-midnight (+ (* hour 3600) (* minute 60) second)))

(define-predicate-type time)

(defmethod print-object ((x time) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d:~2,'0d:~2,'0d"
            (time-hour   x)
            (time-minute x)
            (time-second x))))

(defgeneric time-hour (x)
  (:method ((x time))
    (truncate (seconds-from-midnight x) 3600)))

(defgeneric time-minute (x)
  (:method ((x time))
    (mod (truncate (seconds-from-midnight x) 60) 60)))

(defgeneric time-second (x)
  (:method ((x time))
    (mod (seconds-from-midnight x) 60)))

(define-transitive-relation time= (x y)
  (= (seconds-from-midnight x) (seconds-from-midnight y)))

(define-transitive-relation time< (x y)
  (< (seconds-from-midnight x) (seconds-from-midnight y)))

(define-transitive-relation time<= (x y)
  (<= (seconds-from-midnight x) (seconds-from-midnight y)))

(define-transitive-relation time> (x y)
  (> (seconds-from-midnight x) (seconds-from-midnight y)))

(define-transitive-relation time>= (x y)
  (>= (seconds-from-midnight x) (seconds-from-midnight y)))

(defun time+ (time durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (let ((tstamp (+ (seconds-from-midnight time) sec)))
      (unless (zerop day)
        (%parse-error "The duration cannot specify a number of days"))
      (values (make-instance 'time :seconds-from-midnight (mod tstamp 86400))
              (- (truncate tstamp 86400)
                 (if (< tstamp 0) 1 0))))))

(defun time- (time durspec)
  (let* ((dur (duration durspec))
         (day (%duration-days dur))
         (sec (%duration-seconds dur)))
    (let ((tstamp (- (seconds-from-midnight time) sec)))
      (unless (zerop day)
        (%parse-error "The duration cannot specify a number of days"))
      (values (make-instance 'time :seconds-from-midnight (mod tstamp 86400))
              (- (truncate tstamp 86400)
                 (if (< tstamp 0) 1 0))))))

(defun adjust-time (time &key second minute hour)
  (make-time (or hour (time-hour time))
             (or minute (time-minute time))
             (or second (time-second time))))


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
    (%parse-error "parse error."))
  (make-time (parse-unsigned-integer string :start 0 :end 2)
             (parse-unsigned-integer string :start 2 :end 4)
             (parse-unsigned-integer string :start 4 :end 6)))


;;; types-time.lisp ends here
