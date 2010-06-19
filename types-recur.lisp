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
   ;; Earch element of byday list is a pair (wday . n), where wday is
   ;; a `weekday' value, and n is the optional prefix in the byday
   ;; rule.
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

;;; The predicate function in order to check if an arbitrary object is
;;; a recurrence value.
(define-predicate-type recur)

;; ;;; Useful for debugging
(defmethod print-object ((obj recur) stream)
  (print-unreadable-object (obj stream :type t)
    (write-string (format-value obj) stream)))

;;; Check the consistency of RECUR. This funcions makes sure the type
;;; of slots in the recur instance are valid. This is not redundand
;;; with :type slots options. The slot types are checked when a recur
;;; is instantiated, while check-recur-consistency is called after
;;; parsing. Indeed, check-recur-consistency is more intensive.
(defun check-recur-consistency (recur)
  (unless (slot-boundp recur 'freq)
    (%parse-error "FREQ rule is required."))
  (with-slots (freq until count interval
                    bysecond byminute byhour
                    byday bymonthday byyearday
                    byweekno bymonth bysetpos
                    wkst)
      recur
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


;;; Check if the frequence X is lesser than the Y one.
(defun freq< (x y)
  (declare (recur-frequence x y))
  (let ((freqs #(:secondly :minutely :hourly :daily :weekly :monthly :yearly)))
    (< (position x freqs)
       (position y freqs))))

(defun seconds-between (dt1 dt2)
  (- (seconds-from-1900 dt2)
     (seconds-from-1900 dt1)))

(defun minutes-between (dt1 dt2)
  (idiv (seconds-between dt1 dt2) 60))

(defun hours-between (dt1 dt2)
  (idiv (seconds-between dt1 dt2) 3600))

(defun days-between (dt1 dt2)
  (- (day-from-1900 dt2)
     (day-from-1900 dt1)))

(defun weeks-between (dt1 dt2)
  (idiv (days-between dt1 dt2) 7))

(defun months-between (dt1 dt2)
  (let ((years (- (date-year dt2) (date-year dt1))))
    (+ (* 12 years)
       (- (date-month dt2) (date-month dt1)))))

(defun years-between (dt1 dt2)
  (- (date-year dt2) (date-year dt1)))


;;; Return a recur such that omitted BYSECOND, BYMINUTE, BYHOUR,
;;; BYMONTHDAY, BYMONTH, BYDAY, are filled which default values taken
;;; from a dtstart.
(defun %complete-recur (recur dtstart)
  (with-slots (freq until count interval bysecond byminute byhour
                    byday bymonthday byyearday byweekno bymonth
                    bysetposwkst wkst bysetpos)
      recur
      (make-instance 'recur
                     :freq freq
                     :until until
                     :count count
                     :interval interval
                     :bysetpos bysetpos
                     :wkst wkst
                     :byday byday
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

;;; 0-monday
(defun date-day-of-week* (x)
  (nth-value 1 (date-day-of-week x)))

(defun mod* (m n)
  (declare (integer m n))
  (cond
    ((< m 0)
     (1+ (+ n m)))
    ((>= n m 0)
     m)
    (t
     (error "~a should be a integer in [-~a,1] or [~:*~a,1]." m n))))

;;; Handle some very simple cases of `recur-instance-p'. In general,
;;; every recur without BYSETPOS either COUNT rules are handled here,
;;; because it is not needed to iterate.
(defun %simple-recur-instance-p (start recur datetime)
  (with-slots (freq until count interval bysecond byminute byhour
                    byday bymonthday byyearday byweekno bymonth
                    bysetposwkst wkst bysetpos)
      recur 
    (macrolet ((implyp (condition implication)
                 `(aif ,condition ,implication t)))
      (assert (not bysetpos))
      (assert (not count))
      (and
       ;; DATETIME is an instance of RECUR if and only if all the
       ;; following conditions are satisfied.
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
               (find (date-month datetime) it))
       (implyp byweekno
               (find (date-week-of-year datetime wkst) it))
       (implyp byyearday
               (find (date-day-of-year datetime) it
                     :key (lambda (n)
                            (if (leap-year-p (date-year datetime))
                                (mod* n 366)
                                (mod* n 365)))))
       (implyp bymonthday
               (find (date-day datetime) bymonthday
                     :key (lambda (n)
                            (let ((month-days
                                   (if (leap-year-p (date-year datetime))
                                       #(0 31 29 31 30 31 30 31 31 30 31 30 31)
                                       #(0 31 28 31 30 31 30 31 31 30 31 30 31))))
                              (mod* n (elt month-days (date-month datetime)))))))
       (cond
         ((null byday)
          (implyp (eq freq :weekly)
                  (= (nth-value 1 (date-day-of-week datetime))
                     (nth-value 1 (date-day-of-week start)))))
         (t
          (let ((byday-1
                 ;; Elements of the byday list which match with
                 ;; the day of the week of the recur.
                 (remove (date-day-of-week datetime)
                         byday
                         :test (complement #'eql)
                         :key #'car)))
            (cond
              ((null byday-1) nil)
              ((some* #'null byday-1 :key #'cdr))
              (t
               (assert (member freq '(:monthly :yearly)))
               (let* ( ;; First day of month or the year, according to
                      ;; the freq rule.
                      (first-day-of-month-or-year
                       (if (or (eq freq :monthly) (null byweekno))
                           (adjust-date datetime :day 1)
                           (adjust-date datetime :day 1 :month 1)))
                      ;; Last day of month or the year, according to
                      ;; the freq rule.
                      (last-day-of-month-or-year
                       (if (or (eq freq :monthly) (null byweekno))
                           (adjust-date first-day-of-month-or-year
                                        :day
                                        (elt (if (leap-year-p (date-year datetime))
                                                 #(0 31 29 31 30 31 30 31 31 30 31 30 31)
                                                 #(0 31 28 31 30 31 30 31 31 30 31 30 31))
                                             (date-month first-day-of-month-or-year)))
                           (adjust-date first-day-of-month-or-year :day 31 :month 12)))
                      ;; First day greater than first-day-of-month-or-year which matches with
                      ;; the weekday.
                      (first-day
                       (date+ first-day-of-month-or-year
                              (let ((delta (- (date-day-of-week* datetime)
                                              (date-day-of-week* first-day-of-month-or-year))))
                                (make-duration :days (mod7 delta)))))
                      ;; Last day smaller than The last day of the month or the year,
                      ;; according to first-day. It is used in order
                      ;; to compute the negative offsets.
                      (last-day
                       (date- last-day-of-month-or-year
                              (let ((delta (- (date-day-of-week* last-day-of-month-or-year)
                                              (date-day-of-week* datetime))))
                                (make-duration :days (mod7 delta)))))
                      ;; Weeks in month/year
                      (total-weeks (weeks-between first-day last-day))
                      ;; Weeks between FIRST-DAY and DATETIME.
                      (weeks (weeks-between first-day datetime)))

                 ;; Found the weeks between FIRST-DAY and DATETIME in
                 ;; the CDRs of BYDAY-1. Indeed, it implements
                 ;; negative values, which are computed respect to
                 ;; TOTAL-WEEKS.
                 (find weeks byday-1
                       :key (lambda (week)
                              (mod* (cdr week) total-weeks)))))))))

       (implyp byhour
               (find (time-hour datetime) it))
       (implyp byminute
               (find (time-minute datetime) it))
       (implyp bysecond
               (find (time-second datetime) it))

       ;; If above all conditions satisfied, then return t
       t))))


(defstruct recur-iterator
  dtstart
  recur
  count
  instances)

(defun recur-iterator-new (recur datetime)
  (make-recur-iterator :dtstart datetime :recur recur))

(defun %recur-list-instances-in-second (dt recur)
  (declare (ignorable recur))
  (list dt))

(defun %recur-list-instances-in-minute (dt recur)
  (with-collect
    (dolist (second (recur-bysecond recur))
      (collect (adjust-datetime dt :second second)))))

(defun %recur-list-instances-in-hour (dt recur)
  (with-collect
    (dolist (minute (recur-byminute recur))
      (dolist (second (recur-bysecond recur))
        (collect (adjust-datetime dt :minute minute :second second))))))

(defun %recur-list-instances-in-daiy (dt recur)
  (with-collect
    (dolist (h (recur-byhour recur))
      (dolist (m (recur-byminute recur))
        (dolist (s (recur-bysecond recur))
          (collect (adjust-datetime dt :hour h :minute m :second s)))))))

(defun %recur-list-instances-in-week (dt recur)
  )

(defun %recur-list-instances-in-month (dt recur)
  )
(defun %recur-list-instances-in-year (dt recur)
  )

(defun recur-iterator-next (iter)
  ;; Devolver, una por una, las instancias que estan en la lista de
  ;; instancias del iterador. En este paso de filtran para devolver
  ;; unicamente las que coinciden con la regla BYSETPOS.
  (pop (recur-iterator-instances iter))
  ;; Incrementar el contador de instancias
  (incf (recur-iterator-count iter))  
  ;; Si la lista esta vacia, entonces obtenemos la lista de instancias
  ;; del siguiente período acorde a la frecuencia del recur. El
  ;; siguiente recur debemos obtenerlo tambien.
  (ecase (recur-freq iter)
    (:secondly)
    (:minutely)
    (:hourly)
    (:daily)
    (:weekly)
    (:monthly)
    (:yearly)))


;; Check if DATETIME is a valid ocurrence in the RECUR unbound
;; recurrence rule beginning at START datetime.
(defun recur-instance-p (start recur datetime)
  (let ((complete-recur (%complete-recur recur start)))
    (with-slots (freq until count interval bysecond byminute byhour
                      byday bymonthday byyearday byweekno bymonth
                      bysetposwkst wkst bysetpos)
        complete-recur
        (cond
          ((and (null count) (null bysetpos))
           (%simple-recur-instance-p start complete-recur datetime))
          (t
           ;; (do-recur-instances (dt recur start nil)
           ;;    (when (datetime= dt datetime)
           ;;      (return t)))
           )))))

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
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYMINUTE")
             (setf (slot-value recur 'byminute)
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYHOUR")
             (setf (slot-value recur 'byhour)
                   (parse-unsigned-integer-list value)))
            
            ((string= key "BYDAY")
             (setf (slot-value recur 'byday)
                   (mapcar #'parse-byday-value (split-string value ","))))

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
             (setf (slot-value recur 'byweekno)
                   (parse-integer-list value)))
            
            ((string= key "BYSETPOS")
             (setf (slot-value recur 'bysetpos)
                   (parse-unsigned-integer value )))
            
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
    (format s "~@[;BYSECOND=~{~A~^, ~}~]"   (recur-bysecond recur))
    (format s "~@[;BYMINUTE=~{~A~^, ~}~]"   (recur-byminute recur))
    (format s "~@[;BYHOUR=~{~A~^, ~}~]"     (recur-byhour recur))
    (format s "~@[;BYDAY=~{~@[~d~]~a~^,~}~]"
            (with-collect
              (dolist (day (recur-byday recur))
                (destructuring-bind (wday . n) day
                  (collect n)
                  (collect (elt *weekday-names* (position wday *weekday*)))))))
    (format s "~@[;BYMONTH=~{~A~^, ~}~]"    (recur-bymonth recur))
    (format s "~@[;BYMONTHDAY=~{~A~^, ~}~]" (recur-bymonthday recur))
    (format s "~@[;BYYEARDAY=~{~A~^, ~}~]"  (recur-byyearday recur))
    (format s "~@[;BYWEEKNO=~{~A~^, ~}~]"   (recur-byweekno recur))
    (format s "~@[;BYSETPOS=~{~A~^, ~}~]"   (recur-bysetpos recur))
    (unless (eq (recur-wkst recur) :monday)
      (let ((nwkst (position (recur-wkst recur) *weekday*)))
        (format s ";WKST=~a" (elt *weekday-names* nwkst))))))

;;; types-recur.lisp ends here
