;; universal-time.lisp --- Basic universal time functions
;;
;; Copyrigth (C) 2012 David Vázquez
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

(defmacro values-accessors (function &body accessors)
  (let ((var (gensym)))
    `(progn
     ,@(loop for i from 0
             for x in accessors
             collect `(defun ,x (,var) (nth-value ,i (,function ,var)))))))

(values-accessors decode-universal-time
  time-second
  time-minute
  time-hour
  date-day
  date-month
  date-year)

;;; Decode a universal time.
(defmacro decoded-universal-time
    ((&key second minute hour date month year day zone)
     form &body body)
  (let ((vars))
    (flet ((** (x) (or x (car (push (gensym) vars)))))
      `(multiple-value-bind
             (,(** second)        ,(** minute)          ,(** hour)
              ,(** date)          ,(** month)           ,(** year)
              ,(** day)           ,(** zone))
           (decode-universal-time ,form)
         (declare (ignore ,@vars))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *weekday*
    #(:monday :tuesday :wednesday :thursday :friday :saturday :sunday)))

(deftype weekday ()
  `(member ,@(coerce *weekday* 'list)))

;;; Determine if YEAR is leap.
(defun leap-year-p (year)
  (and (zerop (mod year 4))
       (or (not (zerop (mod year 100)))
           (zerop (mod year 400)))))

(defun leap-years-before (year)
  (let ((years (- year 1901)))
    (+ (- (truncate years 4)
          (truncate years 100))
       (truncate (+ years 300) 400))))

;;; 1 January = 1
(defun day-from-new-year (day month year)
  (let ((days-before-month      #(0 0 31 59 90 120 151 181 212 243 273 304 334))
        (days-before-month-leap #(0 0 31 60 91 121 152 182 213 244 274 305 335)))
    (if (leap-year-p year)
        (+ day (svref days-before-month-leap month))
        (+ day (svref days-before-month month)))))

(defun 1900+days (stamp)
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
    (encode-universal-time 0 0 0 day month year)))

(defun day-from-1900 (universal-time)
  (decoded-universal-time (:date date :month month :year year) universal-time
    (+ (* 365 (- year 1900))
       (leap-years-before year)
       (day-from-new-year date month year))))

;;; Some useful numbers

(defun second-from-midnight (universal-time)
  (decoded-universal-time (:date d :month m :year y) universal-time
    (- universal-time (encode-universal-time 0 0 0 d m y))))

(defun day-of-week (universal-time &optional (wkst :monday))
  (decoded-universal-time (:day day) universal-time
    (values (elt *weekday* day)
            (mod (- (+ day 7) (position wkst *weekday*)) 7))))

(defun day-of-year (universal-time)
  (decoded-universal-time (:date day :month month :year year) universal-time
    (day-from-new-year day month year)))


(defun nearest-weekday (universal-time wkst)
  (if (< 3 (second-value (day-of-week universal-time wkst)))
      (next-weekday universal-time wkst)
      (previous-weekday (date+ universal-time 1) wkst)))

(defun week-of-year (ut &optional (wkst :monday))
  (declare (weekday wkst))
  (let ((1jan (beginning-of-year ut)))
    (cond
      ((>= ut (nearest-weekday (forward-year 1jan) wkst))
       (setq 1jan (forward-year 1jan)))
      ((< ut (nearest-weekday 1jan wkst))
       (setq 1jan (backward-year 1jan))))
    (values(1+ (weeks-between (nearest-weekday 1jan wkst) ut))
           (date-year 1jan))))


;;; Components upgrading

(defun adjust-datetime (universal-time &key day month year second minute hour)
  (multiple-value-bind (osecond ominute ohour oday omonth oyear)
      (decode-universal-time universal-time)
    (encode-universal-time (or second osecond)
                           (or minute ominute)
                           (or hour ohour)
                           (or day oday)
                           (or month omonth)
                           (or year oyear))))

(defun adjust-time (universal-time &key second minute hour)
  (adjust-datetime universal-time
   :second second
   :minute minute
   :hour hour))

(defun adjust-date (universal-time &key day month year)
  (adjust-datetime universal-time
   :day day
   :month month
   :year year))


;;; Differenting...

;;; Seconds between UT1 and UT2.
(defun seconds-between (ut1 ut2)
  (- ut1 ut2))

;;; Minutes between UT1 and UT2.
(defun minutes-between (ut1 ut2)
  (truncate (seconds-between ut1 ut2) 60))

;;; Hours between UT1 and UT2.
(defun hours-between (ut1 ut2)
  (truncate (seconds-between ut1 ut2) 3600))

;;; Days between UT1 and UT2.
(defun days-between (ut1 ut2)
  (- (day-from-1900 ut2) (day-from-1900 ut1)))

;;; Weeks between UT1 and UT2.
(defun weeks-between (ut1 ut2)
  (truncate (days-between ut1 ut2) 7))

;;; Months between UT1 and UT2.
(defun months-between (ut1 ut2)
  (decoded-universal-time (:month m1) ut1
    (decoded-universal-time (:month m2) ut2
      (+ (* 12 (years-between ut1 ut2)) (- m2 m1)))))

;;; Years between UT1 and UT2.
(defun years-between (ut1 ut2)
  (decoded-universal-time (:year y1) ut1
    (decoded-universal-time (:year y2) ut2
      (- y2 y1))))


;;; Arithmetic

;;; Add SECS to a universal time
(defun time+ (universal-time secs)
  (+ universal-time secs))

;;; Add DAYS to a universal time
(defun date+ (universal-time days)
  (decoded-universal-time (:second sec :minute min :hour h) universal-time
    (let ((d (day-from-1900 universal-time)))
      (adjust-datetime (1900+days (+ d days))
                       :second sec
                       :minute min
                       :hour h))))

;;; Add DAYS and SECS to a universal time
(defun datetime+ (universal-time days secs)
  (time+ (date+ universal-time days) secs))


;;; Return the following WEEKDAY from UT.
(defun next-weekday (ut weekday)
  (date+ ut (- 7 (second-value (day-of-week ut weekday)))))

;;; Return the first WEEKDAY before of UT.
(defun previous-weekday (ut weekday)
  (let ((dow (second-value (day-of-week ut weekday))))
    (if (zerop dow)
        (date+ ut (- 7))
        (date+ ut (- dow)))))

;;; Add N seconds to UT.
(defun forward-second (ut &optional (n 1))
  (time+ ut n))

;;; Add N minutes to UT.
(defun forward-minute (ut &optional (n 1))
  (time+ ut (* 60 n)))

;;; Add N hours to UT.
(defun forward-hour (ut &optional (n 1))
  (time+ ut (* 3600 n)))

;;; Add N days to UT.
(defun forward-day (ut &optional (n 1))
  (date+ ut n))

;;; Subtract N days to UT.
(defun backward-day (ut &optional (n 1))
  (forward-day ut (- n)))

;;; Add N days to UT.
(defun forward-week (ut &optional (n 1))
  (forward-day ut (* 7 n)))

;;; Subtract N days to UT.
(defun backward-week (ut &optional (n 1))
  (forward-week ut (- n)))

;;; Add N months to UT.
(defun forward-month (ut &optional (n 1))
  (decoded-universal-time (:year year :month month) ut
    (multiple-value-bind (delta-year delta-month)
        (truncate n 12)
      (multiple-value-bind (delta-year* month*)
          (truncate (+ delta-month (1- month)) 12)
        (adjust-date ut
                     :month (1+ month*)
                     :year (+ year delta-year delta-year*))))))

;;; Subtract N months to UT.
(defun backward-month (ut &optional (n 1))
  (forward-month ut (- n)))

;;; Add N years to UT.
(defun forward-year (ut &optional (n 1))
  (adjust-date ut :year (+ (date-year ut) n)))

;;; Subtract N years to UT.
(defun backward-year (ut &optional (n 1))
  (forward-year ut (- n)))


;;; Return the the first day of the month of UT.
(defun beginning-of-month (ut)
  (adjust-date ut :day 1))

;;; Return the last day of the month of UT.
(defun end-of-month (ut)
  (decoded-universal-time (:year year :month month) ut
    (let ((monthdays
          (if (leap-year-p year)
              #(0 31 29 31 30 31 30 31 31 30 31 30 31)
              #(0 31 28 31 30 31 30 31 31 30 31 30 31))))
      (adjust-date ut :day (elt monthdays month)))))

;;; Return the first day of the year of UT.
(defun beginning-of-year (ut)
  (adjust-date ut :day 1 :month 1))

;;; Return the last day of the year of UT.
(defun end-of-year (ut)
  (adjust-date ut :day 31 :month 12))


;;; universal-time.lisp ends here
