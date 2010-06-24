;; types-recur.lisp ---
;;
;;     This file implements the recur data type.
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

;;; The RECUR data type implementation involves iteration across RECUR
;;; instances. Iteration is used in order to implement the COUNT and BYSETPOS
;;; rules basically. In the special case these rules are not present, we will
;;; say the recur is _simple_. We will use `%simple-recur-instance-p'.
;;; However, the iteration mechanism will works anyway.
;;;
;;; According to FREQ recur rule, the BY* rules could be classified in
;;; expansion rules and limitation ones. On the one hand, the expansion rules,
;;; are used to select what datetimes of a period of frequency are instances
;;; in a recur. On the other hand, the limitation rules are used in order to
;;; distinguish which periods of frequency are candidate to contain
;;; instances. See BYDAY for some exceptions. This is implemented in the
;;; source code described here by couples of functions which are named
;;; %recur-list-instances-* and %recur-next-*, respectively. These functions
;;; ignore COUNT and BYSETPOS recur rules.
;;;
;;; Finally, `recur-iterator-new' and `recur-iterator-next' will dispatch upon
;;; previous functions, in addition of add support for COUNT and BYSETPOS
;;; rules. A high level macro `do-recur-instances' is also provided.

(deftype non-zero-integer (a b)
  `(and (integer ,a ,b) (not (integer 0 0))))

(deftype recur-frequence ()
  '(member :secondly :minutely :hourly :daily :weekly :monthly :yearly))

;;; Alist of frequency strings and values.
(defvar *frequency-table*
  '(("SECONDLY" . :secondly)
    ("MINUTELY" . :minutely)
    ("HOURLY"   . :hourly)
    ("DAILY"    . :daily)
    ("WEEKLY"   . :weekly)
    ("MONTHLY"  . :monthly)
    ("YEARLY"   . :yearly)))

;;; The recur data type value is documented in the section 3.3.10,
;;; named `Recurrence Rule' of RFC5545.
(defclass recur ()
  ((freq
    :initarg :freq
    :type recur-frequence
    :reader recur-freq)
   (until
    :initarg :until
    :type (or null datetime date)
    :initform nil
    :reader recur-until)
   (count
    :initarg :count
    :type (or null unsigned-byte)
    :initform nil
    :reader recur-count)
   (interval
    :initarg :interval
    :type (integer 1 *)
    :initform 1
    :reader recur-interval)
   (bysecond
    :initarg :bysecond
    :type list
    :initform nil
    :reader recur-bysecond)
   (byminute
    :initarg :byminute
    :type list
    :initform nil
    :reader recur-byminute)
   (byhour
    :initarg :byhour
    :type list
    :initform nil
    :reader recur-byhour)
   ;; Each element of byday list is a pair (wday . n), where wday is a weekday
   ;; value, and n is the optional prefix in the byday rule.
   (byday
    :initarg :byday
    :type list
    :initform nil
    :reader recur-byday)
   (bymonthday
    :initarg :bymonthday
    :type list
    :initform nil
    :reader recur-bymonthday)
   (byyearday
    :initarg :byyearday
    :type list
    :initform nil
    :reader recur-byyearday)
   (byweekno
    :initarg :byweekno
    :type list
    :initform nil
    :reader recur-byweekno)
   (bymonth
    :initarg :bymonth
    :type list
    :initform nil
    :reader recur-bymonth)
   (bysetpos
    :initarg :bysetpos
    :type list
    :initform nil
    :reader recur-bysetpos)
   (wkst
    :initarg :wkst
    :type weekday
    :initform :monday
    :reader recur-wkst)))

;;; Bind the symbols FREQ, UNTIL, COUNT, INTERVAL, BYSECOND, BYMINUTE, BYHOUR,
;;; BYDAY, BYMONTHDAY, BYYEARDAY, BYWEEKNO, BYMONTH, BYSETPOSWKST, WKST and
;;; BYSETPOS to the respective slot values of RECUR. These symbols are
;;; setf-able too.
(defmacro with-recur-slots (recur &body code)
  `(with-slots (freq until count interval bysecond byminute byhour
                     byday bymonthday byyearday byweekno bymonth
                     bysetposwkst wkst bysetpos)
       ,recur
     ,@code))

;;; The predicate function in order to check if an arbitrary object is a
;;; recurrence value.
(define-predicate-type recur)

;;; Useful for debugging.
(defmethod print-object ((obj recur) stream)
  (print-unreadable-object (obj stream :type t)
    (write-string (format-value obj) stream)))

;;; Check the consistency of RECUR. This function makes sure the type of
;;; slot's values in the recur instance are valid. This is not redundant with
;;; :type slots options. The slot types are checked when a recur is
;;; instantiated, while check-recur-consistency is called after
;;; parsing. Indeed, check-recur-consistency is more intensive.
(defun check-recur-consistency (recur)
  (unless (slot-boundp recur 'freq)
    (%parse-error "FREQ rule is required."))
  (with-recur-slots recur
    (macrolet ((check-type-list (list type)
                 ;; Check every element of LIST is of type TYPE.
                 `(dolist (i ,list)
                    (check-type i ,type))))
      (check-type freq recur-frequence)
      (unless (or (not until) (not count))
        (%parse-error "You cannot specify both UNTIL and COUNT recur rules."))
      ;; Check optional slots
      (and until    (check-type until    (or date datetime)))
      (and count    (check-type count    (integer 0 *)))
      (and interval (check-type interval (integer 0 *)))
      (check-type wkst weekday)
      ;; Check list slots
      (unless (or (eq freq :monthly)
                  (eq freq :yearly))
        (dolist (bydayrule byday)
          (when (cdr bydayrule)
            (%parse-error "prefix weekday specified in a no weekly or monthly recur."))))
      (check-type-list bysecond   (integer 0 60))
      (check-type-list byminute   (integer 0 59))
      (check-type-list byhour     (integer 0 23))
      (check-type-list bymonthday (non-zero-integer  -31  31))
      (check-type-list byyearday  (non-zero-integer -366 366))
      (check-type-list byweekno   (non-zero-integer  -53  53))
      (check-type-list bymonth    (integer 0 12))
      (check-type-list bysetpos   (non-zero-integer -366 366)))))


;;;; Date and time related functions

;;; Check if the frequence X is lesser than the Y one.
(defun freq< (x y)
  (declare (recur-frequence x y))
  (let ((freqs #(:secondly :minutely :hourly :daily :weekly :monthly :yearly)))
    (< (position x freqs) (position y freqs))))

;;; Return the difference between two points in the timeline, measured in
;;; different units.

(defun seconds-between (dt1 dt2)
  (- (seconds-from-1900 dt2)  (seconds-from-1900 dt1)))

(defun minutes-between (dt1 dt2)
  (idiv (seconds-between dt1 dt2) 60))

(defun hours-between (dt1 dt2)
  (idiv (seconds-between dt1 dt2) 3600))

(defun days-between (dt1 dt2)
  (- (day-from-1900 dt2) (day-from-1900 dt1)))

(defun weeks-between (dt1 dt2)
  (idiv (days-between dt1 dt2) 7))

(defun months-between (dt1 dt2)
  (let ((years (- (date-year dt2) (date-year dt1))))
    (+ (* 12 years) (- (date-month dt2) (date-month dt1)))))

(defun years-between (dt1 dt2)
  (- (date-year dt2) (date-year dt1)))

;;; Return the following WEEKDAY from DATETIME.
(defun next-weekday (datetime weekday)
  (let ((days
         (mod7 (1+ (- (position weekday *weekday*)
                      (nth-value 1 (date-day-of-week datetime)))))))
    (date+ datetime (make-duration :days (abs days)))))

;;; Return the first WEEKDAY before of DATETIME.
(defun previous-weekday (datetime weekday)
  (let ((days
         (mod7 (1- (- (nth-value 1 (date-day-of-week datetime))
                      (position weekday *weekday*))))))
    (date- datetime (make-duration :days (abs days)))))

;;; Add N seconds to DATETIME.
(defun forward-second (datetime &optional (n 1))
  (datetime+ datetime (make-duration :seconds n)))

;;; Add N minutes to DATETIME.
(defun forward-minute (datetime &optional (n 1))
  (datetime+ datetime (make-duration :minutes n)))

;;; Add N hours to DATETIME.
(defun forward-hour (datetime &optional (n 1))
  (datetime+ datetime (make-duration :hours n)))

;;; Add N days to DATETIME.
(defun forward-day (datetime &optional (n 1))
  (date+ datetime (make-duration :days n)))

;;; Sub N days to DATETIME.
(defun backward-day (datetime &optional (n 1))
  (date- datetime (make-duration :days n)))

;;; Add N days to DATETIME.
(defun forward-week (datetime &optional (n 1))
  (forward-day datetime (* 7 n)))

;;; Sub N days to DATETIME.
(defun backward-week (datetime &optional (n 1))
  (forward-day datetime (* 7 (- n))))

;;; Add N months to DATETIME.
(defun forward-month (datetime &optional (n 1))
  (let ((year (date-year datetime))
        (month (date-month datetime)))
    (multiple-value-bind (delta-year delta-month)
        (truncate n 12)
      (multiple-value-bind (delta-year* month*)
          (truncate (+ delta-month (1- month)) 12)
        (adjust-date datetime
                     :month (1+ month*)
                     :year (+ year delta-year delta-year*))))))

;;; Sub N months to DATETIME.
(defun backward-month (datetime &optional (n 1))
  (forward-month datetime (- n)))

;;; Return the the first day of the month of DT.
(defun beginning-of-month (dt)
  (adjust-date dt :day 1))

;;; Return the last day of the month of DT.
(defun end-of-month (dt)
  (let* ((year (date-year dt))
         (monthdays 
          (if (leap-year-p year)
              #(0 31 29 31 30 31 30 31 31 30 31 30 31)
              #(0 31 28 31 30 31 30 31 31 30 31 30 31))))
    (adjust-date dt :day (elt monthdays (date-month dt)))))

;;; Return the first day of the year of DT.
(defun beginning-of-year (dt)
  (adjust-date dt :day 1 :month 1))

;;; Return the last day of the year of DT.
(defun end-of-year (dt)
  (adjust-date dt :day 31 :month 12))

;;; Return M si 0 < M <= N, and M+N+1 if 0 < M.
(defun mod* (m n)
  (declare (integer m n))
  (cond
    ((and (< m 0) (<= (- m) n))
     (1+ (+ n m)))
    ((and (< 0 m) (<= m n)) m)
    (t (error "~a should be a integer in [-~a,1] or [~:*~a,1]." m n))))

;;; Return the nth DAY of month in year. This handles negative days propertily .
(defun monthday (day month year)
  (let ((monthdays
         (if (leap-year-p year)
             #(0 31 29 31 30 31 30 31 31 30 31 30 31)
             #(0 31 28 31 30 31 30 31 31 30 31 30 31))))
    (mod* day (elt monthdays month))))

;;; Return the nth DAY in year. This handles negative days propertily.
(defun yearday (day year)
  (if (leap-year-p year)
      (mod* day 366)
      (mod* day 365)))

;;; Iterate across the cartesian product of several lists. Each element of
;;; FORMS is a list of the form (variable list). LIST is evaluated and the
;;; body is run with variables bound to the values.
(defmacro do-cartesian (forms &body code)
  (if (null forms)
      `(progn ,@code)
      (destructuring-bind ((var list) &rest others)
          forms
        `(dolist (,var ,list)
           (do-cartesian ,others
             ,@code)))))

;;; Return a recur such that the omitted rules: BYSECOND, BYMINUTE, BYHOUR,
;;; BYMONTHDAY, BYMONTH, and BYDAY, are filled with default values taken from
;;; the DTSTART datetime.
(defun %complete-recur (recur dtstart)
  (with-recur-slots recur
    (make-instance 'recur
                   :freq freq
                   :until until
                   :count count
                   :interval interval
                   :bysetpos bysetpos
                   :wkst wkst
                   :byyearday byyearday
                   :byweekno byweekno
                   :bysecond
                   (or bysecond (list (time-second dtstart)))
                   :byminute
                   (or byminute (and (freq< :minutely freq)
                                     (list (time-minute dtstart))))
                   :byhour
                   (or byhour (and (freq< :hourly freq)
                                   (list (time-hour dtstart))))
                   :byday
                   (or byday
                       (and (or byweekno (eq freq :weekly))
                            (list (list (date-day-of-week dtstart)))))
                   :bymonthday
                   (or bymonthday (and (freq< :weekly freq)
                                       (not byday)
                                       (not byyearday)
                                       (not byweekno)
                                       (list (date-day dtstart))))
                   :bymonth
                   (or bymonth (and (freq< :monthly freq)
                                    (not byday)
                                    (not byyearday)
                                    (not byweekno)
                                    (list (date-month dtstart)))))))

;;; Given a DATETIME and a RECUR, return a couple of values which are the
;;; limits of the period of frequency which the datetime belongs to.
(defun interval-limits (datetime recur)
  (case (recur-freq recur)
    (:secondly
     (values datetime datetime))
    (:minutely
     (values (adjust-time datetime :second 0)
             (adjust-time datetime :second 59)))
    (:hourly
     (values (adjust-time datetime :minute 00 :second 00)
             (adjust-time datetime :minute 59 :second 59)))
    (:daily
     (values (adjust-time datetime :hour 00 :minute 00 :second 00)
             (adjust-time datetime :hour 23 :minute 59 :second 59)))
    (:weekly
     (values (previous-weekday datetime (recur-wkst recur))
             (next-weekday datetime (recur-wkst recur))))
    (:monthly
     (values (beginning-of-month datetime)
             (end-of-month datetime)))
    (:yearly
     (values (beginning-of-year datetime)
             (end-of-year datetime)))))

;;; Check if DATETIME is compatible with a byday VALUE.
(defun byday-compatible-p (value datetime start end)
  (let* ((day-of-week (date-day-of-week datetime))
         (first-day (next-weekday start day-of-week))
         (last-day  (previous-weekday end day-of-week))
         (total-weeks (weeks-between first-day last-day)))
    (and value
         (loop for (weekday . n) in value thereis
              (when (eq day-of-week weekday)
                (or (null n)
                    (let* ((weeks (weeks-between first-day datetime)))
                      (and (< 0 weeks)
                           (= (mod* n total-weeks) (mod* weeks total-weeks))))))))))


;;; Handle some very simple cases of `recur-instance-p'. In general, every
;;; recur without BYSETPOS either COUNT rules are handled here, because it is
;;; not needed to iterate.
(defun %simple-recur-instance-p (start recur datetime)
  (with-recur-slots recur 
    ;; (assert (not bysetpos))
    ;; (assert (not count))
    (and
     ;; DATETIME is an instance of RECUR if and only if all the following
     ;; conditions are satisfied.
     (ecase freq
       (:secondly (divisiblep (seconds-between start datetime) interval))
       (:minutely (divisiblep (minutes-between start datetime) interval))
       (:hourly   (divisiblep (hours-between   start datetime) interval))
       (:daily    (divisiblep (days-between    start datetime) interval))
       (:weekly   (divisiblep (weeks-between   start datetime) interval))
       (:monthly  (divisiblep (months-between  start datetime) interval))
       (:yearly   (divisiblep (years-between   start datetime) interval)))

     (typecase until
       (null t) 
       (datetime
        (check-ical-type datetime datetime)
        (datetime<= start datetime))
       (date
        (check-ical-type datetime date)
        (date<= start datetime)))

     (implyp bymonth
             (find (date-month datetime) bymonth))
     (implyp byweekno
             (find (date-week-of-year datetime wkst) byweekno))
     (implyp byyearday
             (find (date-day-of-year datetime) byyearday
                   :key (lambda (n)
                          (if (leap-year-p (date-year datetime))
                              (mod* n 366)
                              (mod* n 365)))))
     (implyp bymonthday
             (find (date-day datetime) bymonthday
                   :key (lambda (n)
                          (monthday n (date-month datetime) (date-year datetime)))))
     (implyp byday
             (multiple-value-bind (start end)
                 (interval-limits datetime recur)
               (byday-compatible-p byday datetime start end)))
     (implyp byhour
             (find (time-hour datetime) byhour))
     (implyp byminute
             (find (time-minute datetime) byminute))
     (implyp bysecond
             (find (time-second datetime) bysecond)))))


(defmacro ucond ((variable value) &body code)
  (with-gensyms (initial)
    (check-type variable symbol)
    `(let ((,variable ,value))
         (tagbody
            ,initial
            ,@(loop
                 for (condition . body) in code
                 collect
                 `(unless ,condition
                    (setf ,variable (progn ,@body))
                    (go ,initial))))
         ,variable)))

(defun %recur-list-instances-in-second (dt recur)
  (declare (ignorable recur))
  (list dt))

(defun %recur-next-second (dt recur)
  (with-recur-slots recur
    (ucond (datetime (forward-second dt interval))
      ((implyp bymonth
               (find (date-month datetime) bymonth))
       (adjust-datetime (forward-month datetime) :day 1 :hour 00 :minute 00 :second 00))
      ((implyp byyearday
               (find (date-day-of-year datetime) byyearday
                     :key (lambda (yday)
                            (yearday yday (date-year datetime)))))
       (adjust-time (forward-day datetime) :hour 00 :minute 00 :second 00))
      ((implyp bymonthday
               (find (date-day datetime) bymonthday
                     :key (lambda (mday)
                            (let ((month (date-month datetime))
                                  (year  (date-year datetime)))
                              (monthday mday month year)))))
       (adjust-time (forward-day datetime) :hour 00 :minute 00 :second 00))
      ((implyp byday
               (find (date-day-of-week datetime) byday :key #'car))
       (adjust-time (forward-day datetime) :hour 00 :minute 00 :second 00))
      ((implyp byhour
               (find (time-hour datetime) byhour))
       (adjust-time (forward-hour datetime) :minute 00 :second 00))
      ((implyp byminute
               (find (time-minute datetime) byminute))
       (adjust-time (forward-minute datetime) :second 00))
      ((implyp bysecond
               (find (time-second datetime) bysecond))
       (forward-second datetime interval)))))

(defun %recur-list-instances-in-minute (dt recur)
  (with-collect
    (do-cartesian ((second (recur-bysecond recur)))
      (collect (adjust-time dt :second second)))))

(defun %recur-next-minute (dt recur)
  (with-recur-slots recur
    (ucond (datetime (forward-minute dt interval))
      ((implyp bymonth
               (find (date-month datetime) bymonth))
       (adjust-datetime (forward-month datetime) :day 01 :hour 00 :minute 00))
      ((implyp byyearday
               (find (date-day-of-year datetime) byyearday
                     :key (lambda (yday)
                            (yearday yday (date-year datetime)))))
       (adjust-time (forward-day datetime) :hour 00 :minute 00))
      ((implyp bymonthday
               (find (date-day datetime) bymonthday
                     :key (lambda (mday)
                            (let ((month (date-month datetime))
                                  (year  (date-year datetime)))
                              (monthday mday month year)))))
       (adjust-time (forward-day datetime) :hour 00 :minute 00))
      ((implyp byday
               (find (date-day-of-week datetime) byday :key #'car))
       (adjust-time (forward-day datetime) :hour 00 :minute 00))
      ((implyp byhour
               (find (time-hour datetime) byhour))
       (adjust-time (forward-hour datetime) :minute 00))
      ((implyp byminute
               (find (time-minute datetime) byminute))
       (forward-minute datetime interval)))))

(defun %recur-list-instances-in-hour (dt recur)
  (with-collect
    (do-cartesian ((minute (recur-byminute recur))
                   (second (recur-bysecond recur)))
      (collect (adjust-time dt :minute minute :second second)))))

(defun %recur-next-hour (dt recur)
  (with-recur-slots recur
    (ucond (datetime (forward-hour dt interval))
      ((implyp bymonth
               (find (date-month datetime) bymonth))
       (adjust-datetime (forward-month datetime) :day 01 :hour 00))
      ((implyp byyearday
               (find (date-day-of-year datetime) byyearday
                     :key (lambda (yday)
                            (yearday yday (date-year datetime)))))
       (adjust-time (forward-day datetime) :hour 00))
      ((implyp bymonthday
               (find (date-day datetime) bymonthday
                     :key (lambda (mday)
                            (let ((month (date-month datetime))
                                  (year  (date-year datetime)))
                              (monthday mday month year)))))
       (adjust-time (forward-day datetime) :hour 00))
      ((implyp byday
               (find (date-day-of-week datetime) byday :key #'car))
       (adjust-time (forward-day datetime) :hour 00))
      ((implyp byhour
               (find (time-hour datetime) byhour))
       (forward-hour datetime interval)))))

(defun %recur-list-instances-in-day (dt recur)
  (with-collect
    (do-cartesian ((h (recur-byhour recur))
                   (m (recur-byminute recur))
                   (s (recur-bysecond recur)))
      (collect (adjust-time dt :hour h :minute m :second s)))))

(defun %recur-next-day (dt recur)
  (with-recur-slots recur
    (ucond (datetime (forward-day dt interval))
      ((implyp bymonth
               (find (date-month datetime) bymonth))
       (adjust-datetime (forward-month datetime) :day 01))
      ((implyp bymonthday
               (find (date-day datetime) bymonthday
                     :key (lambda (mday)
                            (let ((month (date-month datetime))
                                  (year  (date-year datetime)))
                              (monthday mday month year)))))
       (forward-day datetime interval))
      ((implyp byday
               (find (date-day-of-week datetime) byday :key #'car))
       (forward-day datetime interval)))))

(defun %recur-list-instances-in-week (dt recur)
  (with-collect
    (do-cartesian ((d (recur-byday recur))
                   (h (recur-byhour recur))
                   (m (recur-byminute recur))
                   (s (recur-bysecond recur)))
      (let ((dt* (adjust-time dt :hour h :minute m :second s)))
        (collect (next-weekday dt* (car d)))))))

(defun %recur-next-week (dt recur)
  (with-recur-slots recur
    (ucond (datetime (forward-week dt interval))
      ((implyp bymonth
               (find (date-month datetime) bymonth
                     :key (lambda (m)
                            (let ((month (date-month datetime))
                                  (year (date-year datetime)))
                              (monthday m month year)))))
       (forward-month datetime interval)))))


(defun %recur-list-instances-in-month (dt recur)
  (with-recur-slots recur
    (cond
      ;; If BYMONTHDAY is given, BYDAY rule limits the recur. So, we iterate
      ;; in order to collect the datetimes compatible with the BYMONTHDAY,
      ;; BYHOUR, BYMINUTE and BYSECOND.
      (bymonthday
       (with-collect
         (do-cartesian ((d bymonthday)
                        (h byhour)
                        (m byminute)
                        (s bysecond))
           (let* ((mday (monthday d (date-month dt) (date-year dt)))
                  (dt* (adjust-datetime dt :day mday :hour h :minute m :second s)))
             (if (null byday)
                 (collect dt*)
                 (multiple-value-bind (start end)
                     (interval-limits dt* recur)
                   (when (byday-compatible-p byday dt* start end)
                     (collect dt*))))))))
      ;; If BYMONTHDAY is not present and byday does is, we collect
      ;; compatible datetimes without iterate.
      (byday
       (assert (not bymonthday))
       (with-collect
         (do-cartesian ((d byday)
                        (h byhour)
                        (m byminute)
                        (s bysecond))
           (multiple-value-bind (start end)
               (interval-limits dt recur)
             (let* ((weekday (car d))
                    (nweekday (cdr d))
                    (first-day (next-weekday start weekday))
                    (last-day (previous-weekday end weekday))
                    (total-weeks (1+ (weeks-between first-day last-day)))
                    (adjusted (adjust-time first-day :hour h :minute m :second s)))
               ;; We collect the computed datetime if NWEEKDAY is
               ;; non-nil. Otherwise, we collect datetimes which matches
               ;; with weekday.
               (if nweekday
                   (let ((days (* 7 (1- (mod* nweekday total-weeks)))))
                     (collect (forward-day adjusted days)))
                   (dotimes (i total-weeks)
                     (collect (forward-day adjusted (* i 7)))))))))))))

(defun %recur-next-month (dt recur)
  (with-recur-slots recur
    (do ((delta interval (+ delta interval))
         (datetime (forward-month dt interval)
                   (forward-month datetime interval)))
        ((or (null bymonth)
             (>= delta (lcm 12 interval))
             (find (date-month datetime) bymonth))
         (if (<= delta (lcm 12 interval))
             datetime
             nil)))))

(defun %recur-list-instances-in-year (datetime recur)
  (with-recur-slots recur
    ;; The local function compatible-p checks if a datetime DT is compatible
    ;; with some BY* rules. This does not check all BY* rules, because we can
    ;; make sure some rules will be verified.
    (flet ((compatible-p (dt)
             (and
              (implyp bymonth
                      (find (date-month dt) bymonth))
              (implyp byweekno
                      (find (date-week-of-year dt (recur-wkst recur)) byweekno))
              (implyp byyearday
                      (find (date-day-of-year dt) byyearday
                            :key (lambda (n)
                                   (yearday n (date-year dt)))))
              (implyp bymonthday
                      (find (date-day dt) bymonthday
                            :key (lambda (n)
                                   (monthday n (date-month dt) (date-year dt)))))
              (implyp byday
                      (if bymonth
                          (let ((start (beginning-of-month dt))
                                (end (end-of-month dt)))
                            (byday-compatible-p byday dt start end))
                          (multiple-value-bind (start end)
                              (interval-limits dt recur)
                            (byday-compatible-p byday dt start end)))))))
      (cond
        ;; If one of BYMONTH or BYMONTHDAY is given, then we iterate across
        ;; all dates which match with this description. We must check it is
        ;; compatible with the rest of the BY* rules.
        ((or bymonth bymonthday)
         (with-collect
           (do-cartesian ((month (or bymonth (range 1 12)))
                          (day (or bymonthday (range 1 (monthday -1 month (date-year datetime)))))
                          (hour byhour)
                          (minute byminute)
                          (second bysecond))
             (let ((dt* (make-datetime day month (date-year datetime) hour minute second)))
               (when (compatible-p dt*)
                 (collect dt*))))))
        ;; In this point, not BYMONTH or BYMONTHDAY rule is present. If a
        ;; BYYEARDAY is present, then we can iterate across them.
        (byyearday
         (with-collect
             (let ((first (beginning-of-year datetime)))
               (dolist (yday byyearday)
                 (let ((dt* (forward-day first (1- (yearday yday (date-year first))))))
                   (when (compatible-p dt*)
                     (collect dt*)))))))
        ;; We have considered the BYMONTH rule previosly, so, the offset of
        ;; BYDAY rule here is considered relative to the year.
        (byday
         (assert (not bymonth))
         (with-collect
             (dolist (rule byday)
               (multiple-value-bind (start end)
                   (interval-limits datetime recur)
                 (let* ((weekday (car rule))
                        (n (cdr rule))
                        (first (next-weekday start weekday))
                        (last (previous-weekday end weekday)))
                   (cond
                     ((null n)
                      (dotimes (i (weeks-between first last))
                        (let ((dt* (forward-day first (* 7 (1+ i)))))
                          (when (compatible-p dt*)
                            (collect dt*)))))
                     ((< n 0)
                      (collect (backward-day last (* 7 (1+ (abs n))))))
                     ((> n 0)
                      (collect (forward-day first (* 7 (1- n)))))))))))
        (t
         ;; Note that some of BYDAY, BYYEARDAY or BYMONTHDAY/BYMONTH rules
         ;; should be specified. In other case, %complete-recur should add a
         ;; BYDAY or BYMONTHDAY/BYMONTH rule.
         nil)))))

(defun %recur-next-year (dt recur)
  ;; RECUR objects with FREQ set to YEARLY has not limitation rules, so we add
  ;; INTERVAL years to DT simply.
  (let ((year (date-year dt)))
    (adjust-date dt :year (+ year (recur-interval recur)))))


(defstruct recur-iterator
  dtstart
  recur
  (count 0)
  interval
  instances)

(defun recur-iterator-new (recur datetime)
  (unless (%simple-recur-instance-p datetime recur datetime)
    (error "The recur and DTSTART must be synchronized."))
  (let ((recur (%complete-recur recur datetime)))
    (let* ((first-instances
            (case (recur-freq recur)
              (:secondly
               (%recur-list-instances-in-second datetime recur))
              (:minutely
               (%recur-list-instances-in-minute datetime recur))
              (:hourly
               (%recur-list-instances-in-hour datetime recur))
              (:daily
               (%recur-list-instances-in-day datetime recur))
              (:weekly
               (%recur-list-instances-in-week datetime recur))
              (:monthly
               (%recur-list-instances-in-month datetime recur))
              (:yearly
               (%recur-list-instances-in-year datetime recur))))
           (instances
            (remove-if (lambda (dt) (datetime< dt datetime))
                       first-instances)))
      ;; Return the iterator
      (make-recur-iterator :dtstart datetime :interval datetime
                           :recur recur :instances instances))))


(defun recur-iterator-next (iter)
  (let ((datetime (recur-iterator-interval iter))
        (recur (recur-iterator-recur iter)))
    (cond
      ((and (recur-count recur)
            (>= (recur-iterator-count iter) (recur-count recur)))
       nil)
      ((recur-iterator-instances iter)
       (incf (recur-iterator-count iter))
       (pop (recur-iterator-instances iter)))
      (t
       (case (recur-freq recur)
         (:secondly
          (setf datetime (%recur-next-second datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-second datetime recur)))
         (:minutely
          (setf datetime (%recur-next-minute datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-minute datetime recur)))
         (:hourly
          (setf datetime (%recur-next-hour datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-hour datetime recur)))
         (:daily
          (setf datetime (%recur-next-day datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-day datetime recur)))
         (:weekly
          (setf datetime (%recur-next-week datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-week datetime recur)))
         (:monthly
          (setf datetime (%recur-next-month datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-month datetime recur)))
         (:yearly
          (setf datetime (%recur-next-year datetime recur))
          (setf (recur-iterator-instances iter)
                (%recur-list-instances-in-year datetime recur))))
       (setf (recur-iterator-interval iter) datetime)
       (when (recur-iterator-instances iter)
         (recur-iterator-next iter))))))

(defmacro do-recur-instances ((variable recur dtstart &optional result) &body code)
  (check-type variable symbol)
  (with-gensyms (iterator)
    `(let ((,iterator (recur-iterator-new ,recur ,dtstart)))
       (do ((,variable (recur-iterator-next ,iterator)
                       (recur-iterator-next ,iterator)))
           ((null ,variable)
            ,result)
         ,@code))))

;; Check if DATETIME is a valid ocurrence in RECUR.
(defun recur-instance-p (start recur datetime)
  (let ((complete-recur (%complete-recur recur start)))
    (with-recur-slots complete-recur
        (cond
          ((and (null count) (null bysetpos))
           (%simple-recur-instance-p start complete-recur datetime))
          (t
           (do-recur-instances (dt recur start nil)
              (when (datetime= dt datetime)
                (return t))))))))


;;; Parsing and formatting

(defun parse-byday-value (string)
  (multiple-value-bind (n end)
      (parse-integer string :junk-allowed t)
    (if (and (null n) (< 0 end))
        (%parse-error "~a is not a weekday." string)
        (let* ((str (subseq string end)))
          (aif (position str #("MO" "TU" "WE" "TH" "FR" "SA" "SU") :test #'string=)
               (cons (elt *weekday* it) n)
               (%parse-error "~a is not a weekday." str))))))

(defun parse-rule-part (string)
  (declare (string string))
  (let ((eqpos (position #\= string)))
    (when (null eqpos)
      (%parse-error "Bad rule part ~a" string))
    (cons (subseq string 0 eqpos)
          (subseq string (1+ eqpos)))))

(defun parse-rules (string)
  (declare (string string))
  (let ((parts (split-string string ";" nil)))
    (when (some #'null parts)
      (%parse-error "Empty rule part in the recurrence '~a'." string))
    (mapcar #'parse-rule-part parts)))

(defmethod parse-value (string (type (eql 'recur)) &rest params &key &allow-other-keys)
  (declare (string string))
  (declare (ignore params))
  (let ((rules (parse-rules string))
        (recur (make-instance 'recur)))
    (when (duplicatep rules :key #'car :test #'string=)
      (%parse-error "Duplicate key in recurrence."))
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
                   (or (cdr (assoc value *frequency-table* :test #'string=))
                       (%parse-error "'~a' is not a valid value for the FREQ rule." value))))

            ((string= key "UNTIL")
             (setf (slot-value recur 'until)
                   (handler-case
                       (parse-value value 'datetime)
                     (icalendar-parse-error ()
                       (parse-value value 'date)))))
            
            ((string= key "COUNT")
             (setf (slot-value recur 'count)
                   (parse-unsigned-integer value)))
            
            ((string= key "INTERVAL")
             (setf (slot-value recur 'interval)
                   (parse-unsigned-integer value)))
            
            ((string= key "BYSECOND")
             (setf (slot-value recur 'bysecond)
                   (sort (parse-unsigned-integer-list value) #'<)))
            
            ((string= key "BYMINUTE")
             (setf (slot-value recur 'byminute)
                   (sort (parse-unsigned-integer-list value) #'<)))
            
            ((string= key "BYHOUR")
             (setf (slot-value recur 'byhour)
                   (sort (parse-unsigned-integer-list value) #'<)))
            
            ((string= key "BYDAY")
             (setf (slot-value recur 'byday)
                   (mapcar #'parse-byday-value (split-string value ","))))

            ((string= key "BYMONTH")
             (setf (slot-value recur 'bymonth)
                   (sort (parse-integer-list value) #'<)))
            
            ((string= key "BYMONTHDAY")
             (setf (slot-value recur 'bymonthday)
                   (parse-integer-list value)))
            
            ((string= key "BYYEARDAY")
             (setf (slot-value recur 'byyearday)
                   (parse-integer-list value)))
            
            ((string= key "BYWEEKNO")
             (setf (slot-value recur 'byweekno)
                   (parse-integer-list value)))
            
            ((string= key "BYSETPOS")
             (setf (slot-value recur 'bysetpos)
                   (sort (parse-unsigned-integer value) #'<)))
            
            ((string= key "WKST")
             (setf (slot-value recur 'wkst)
                   (let ((nday (position value *weekday-names* :test #'string-ci=)))
                     (when (null nday)
                       (%parse-error "~a is not a weekday." value))
                     (elt *weekday* nday))))
            (t
             (%parse-error "Unknown recurrence component ~a" key)))))

      ;; Return the recur instance
      (check-recur-consistency recur)
      recur)))


(defmethod format-value ((recur recur) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (with-output-to-string (s)
    (format s "FREQ=~a" (car (rassoc (recur-freq recur) *frequency-table*)))
    ;; Print optional recur slots.
    (format s "~[~;~;~:;;INTERVAL=~:*~d~]"  (recur-interval recur))
    (format s "~@[;COUNT=~a~]"              (recur-count recur))
    (format s "~@[;UNTIL=~a~]"              (recur-until recur))
    (format s "~@[;BYSECOND=~{~A~^,~}~]"    (recur-bysecond recur))
    (format s "~@[;BYMINUTE=~{~A~^,~}~]"    (recur-byminute recur))
    (format s "~@[;BYHOUR=~{~A~^,~}~]"     (recur-byhour recur))
    (format s "~@[;BYDAY=~{~@[~d~]~a~^,~}~]"
            (with-collect
              (dolist (day (recur-byday recur))
                (destructuring-bind (wday . n) day
                  (collect n)
                  (collect (elt *weekday-names* (position wday *weekday*)))))))
    (format s "~@[;BYMONTH=~{~A~^,~}~]"   (recur-bymonth recur))
    (format s "~@[;BYMONTHDAY=~{~A~^,~}~]" (recur-bymonthday recur))
    (format s "~@[;BYYEARDAY=~{~A~^,~}~]"  (recur-byyearday recur))
    (format s "~@[;BYWEEKNO=~{~A~^,~}~]"   (recur-byweekno recur))
    (format s "~@[;BYSETPOS=~{~A~^,~}~]"   (recur-bysetpos recur))
    (unless (eq (recur-wkst recur) :monday)
      (let ((nwkst (position (recur-wkst recur) *weekday*)))
        (format s ";WKST=~a" (elt *weekday-names* nwkst))))))


;;; Local variables:
;;; fill-column: 78
;;; indent-tabs-mode: nil
;;; End:

;;; types-recur.lisp ends here
