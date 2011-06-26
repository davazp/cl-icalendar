;; types-date.lisp ---
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

(deftype day   () '(integer 1 31))
(deftype month () '(integer 1 12))
(deftype year  () 'fixnum)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *weekday*
    #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday)))

(defvar *weekday-names*
  #("MO" "TU" "WE" "TH" "FR" "SA" "SU"))

(deftype weekday ()
  `(member ,@(coerce *weekday* 'list)))

(defclass date ()
  (;; 1 at 1900-01-01
   (day-from-1900
    :type fixnum
    :initarg :day-from-1900
    :reader day-from-1900)))

(register-ical-value date)
(define-predicate-type date)

(defprinter (x date)
  (multiple-value-bind (day month year)
      (%decode-date x)
    (format t "~2,'0d-~2,'0d-~4,'0d" day month year)))

(defun leap-years-before (year)
  (declare (year year))
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
          (truncate years 100))
       (truncate (+ years 300) 400))))

(defun leap-years-between (start end)
  (declare (year start end))
  (- (leap-years-before end)
     (leap-years-before start)))

(defun leap-year-p (year)
  (declare (optimize speed) (year year))
  (and (divisiblep year 4)
       (or (not (divisiblep year 100))
           (divisiblep year 400))))

(defun day-from-new-year (day month year)
  (declare (day day) (month month) (year year))
  (let ((days-before-month      #(0 0 31 59 90 120 151 181 212 243 273 304 334))
        (days-before-month-leap #(0 0 31 60 91 121 152 182 213 244 274 305 335)))
    (if (leap-year-p year)
        (+ day (svref days-before-month-leap month))
        (+ day (svref days-before-month month)))))

(defun make-date (day month year)
  (declare (day day) (month month) (year year))
  (let ((stamp (+ (* 365 (- year 1900))
                  (leap-years-before year)
                  (day-from-new-year day month year))))
    (make-instance 'date :day-from-1900 stamp)))

;;; Accessors

(defun %decode-date (date)
  (let ((stamp (day-from-1900 date)))
    (let (year month day rem)
      ;; Year
      (loop for t1 downfrom (truncate stamp 365)
            for t2 = (+ (* 365 t1) (leap-years-before (+ 1900 t1)))
            for t3 = (- stamp t2)
            while (<= t3 0)
            finally (setf year (+ 1900 t1) rem t3))
      ;; Month
      (loop for m from 12 downto 1
            as t1 = (day-from-new-year 1 m year)
            as t2 = (- rem t1)
            while (< t2 0)
            finally (setf month m rem t2))
      ;; Day
      (setf day (1+ rem))
      ;; Return information
      (values day month year))))

(defgeneric date-day (x)
  (:method ((x date))
    (nth-value 0 (%decode-date x))))

(defgeneric date-month (x)
  (:method ((x date))
    (nth-value 1 (%decode-date x))))

(defgeneric date-year (x)
  (:method ((x date))
    (nth-value 2 (%decode-date x))))

(defun date-day-of-week (x &optional (wkst :monday))
  (declare (weekday wkst))
  ;; Note 1 of January of 1900 was Monday.
  (let* ((nwkst (position wkst *weekday*))
         (nday (mod7 (day-from-1900 x)))
         (day (svref *weekday* (mod7 (1- nday)))))
    (values day (1+ (mod7 (- nday nwkst 1))))))

;;; Begins in 1
(defun date-day-of-year (x)
  (multiple-value-bind (day month year)
        (%decode-date x)
      (day-from-new-year day month year)))

(defun date-week-of-year (dt &optional (wkst :monday))
  (declare (weekday wkst))
  (multiple-value-bind (day month year)
      (%decode-date dt)
    (let* ((yearday (day-from-new-year day month year))
           (weekday (nth-value 1 (date-day-of-week dt wkst)))
           (offset (mod7 (1+ (- weekday yearday))))
           (first (if (<= offset 3)
                      1
                      (- 9 offset))))
      (idiv (+ (- yearday first) 7) 7))))

(define-transitive-relation date= (x y)
  (= (day-from-1900 x) (day-from-1900 y)))

(define-transitive-relation date< (x y)
  (< (day-from-1900 x) (day-from-1900 y)))

(define-transitive-relation date<= (x y)
  (<= (day-from-1900 x) (day-from-1900 y)))

(define-transitive-relation date> (x y)
  (> (day-from-1900 x) (day-from-1900 y)))

(define-transitive-relation date>= (x y)
  (>= (day-from-1900 x) (day-from-1900 y)))

(defgeneric date+ (date durspec)
  (:method ((date date) durspec)
           (let* ((dur (duration durspec))
                  (days (%duration-days dur))
                  (secs (%duration-seconds dur)))
             (unless (zerop secs)
               (%parse-error "The duration ~a is not multiple of days" dur))
             (make-instance 'date :day-from-1900 (+ (day-from-1900 date) days)))))

(defgeneric date- (date durspec)
  (:method ((date date) durspec)
    (let* ((dur (duration durspec))
           (days (%duration-days dur))
           (secs (%duration-seconds dur)))
      (unless (zerop secs)
        (%parse-error "The duration ~a is not multiple of days" dur))
      (make-instance 'date :day-from-1900 (- (day-from-1900 date) days)))))

(defgeneric adjust-date (date &key day month year)
  (:method ((date date) &key day month year)
    (make-date (or day (date-day date))
               (or month (date-month date))
               (or year (date-year date)))))

;;; Parse

(defmethod format-value ((date date) &optional params)
  (declare (ignore params))
  (multiple-value-bind (day month year)
      (%decode-date date)
    (format nil "~4,'0d~2,'0d~2,'0d" year month day)))

(defmethod parse-value (string (type (eql 'date)) &optional params)
  (declare (ignore params))
  (unless (= (length string) 8)
    (%parse-error "parse error."))
  (make-date (parse-unsigned-integer string :start 6 :end 8)
             (parse-unsigned-integer string :start 4 :end 6)
             (parse-unsigned-integer string :start 0 :end 4)))

;;; types-date.lisp ends here
