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

;;; The recur data type value is documented in the section 3.3.10,
;;; named `Recurrence Rule' of RFC5545. Note a recur object has an
;;; associated unbound recur object, i.e, a recur without count and
;;; until slots. If count or until rule is specified, then the
;;; outcoming bound recur data type is a strict subset of the unbound
;;; recur.

(defclass recur ()
  ((freq
    :initarg :freq
    :initform nil
    :reader recur-freq)
   (until
    :initarg :until
    :initform nil
    :reader recur-until)
   (count
    :initarg :count
    :initform nil
    :reader recur-count)
   (interval
    :initarg :interval
    :initform 1
    :reader recur-interval)
   (bysecond
    :initarg :bysecond
    :initform nil
    :reader recur-bysecond)
   (byminute
    :initarg :byminute
    :initform nil
    :reader recur-byminute)
   (byhour
    :initarg :byhour
    :initform nil
    :reader recur-byhour)
   (byday
    :initarg :byday
    :initform nil
    :reader recur-byday)
   (bymonthday
    :initarg :bymonthday
    :initform nil
    :reader recur-bymonthday)
   (byyearday
    :initarg :byyearday
    :initform nil
    :reader recur-byyearday)
   (byweekno
    :initarg :byweekno
    :initform nil
    :reader recur-byweekno)
   (bymonth
    :initarg :bymonth
    :initform nil
    :reader recur-bymonth)
   (bysetpos
    :initarg :bysetpos
    :initform nil
    :reader recur-bysetpos)
   (wkst
    :initarg :wkst
    :initform 0
    :reader recur-wkst)))

;;; The predicate function in order to check if an arbitrary object is
;;; a recurrence value.
(define-predicate-type recur)

(deftype non-zero-integer (a b)
  `(and (integer ,a ,b) (not (integer 0 0))))

(deftype recur-frequence ()
  '(member :secondly :minutely :hourly :daily :weekly :monthly :yearly))

(defun check-recur-consistency (recur)
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
      (assert (or (not until) (not count)))
      ;; Check optional slots
      (and until    (check-type until    (or date datetime)))
      (and count    (check-type count    (integer 0 *)))
      (and interval (check-type interval (integer 0 *)))
      (and wkst     (check-type wkst     (integer 0 6)))
      ;; Check list slots
      (check-type-list bysecond   (integer 0 60))
      (check-type-list byminute   (integer 0 59))
      (check-type-list byhour     (integer 0 23))
      (check-type-list bymonthday (non-zero-integer  -31  31))
      (check-type-list byyearday  (non-zero-integer -366 366))
      (check-type-list byweekno   (non-zero-integer   -7   7))
      (check-type-list bymonth    (integer 0 12))
      (check-type-list bysetpos   (non-zero-integer -366 366)))))


;; TODO: Implementation pending
(defun recur-instances nil t)
(defun %unbound-recur-next-instance nil t)
(defun %unbound-recur-initial-instance nil t)


(defun freq< (x y)
  (declare (recur-frequence x y))
  (let ((freqs #(:secondly :minutely :hourly :daily :weekly :monthly :yearly)))
    (< (position x freqs)
       (position y freqs))))

;; Check if DATETIME is a valid ocurrence in the RECUR unbound
;; recurrence rule beginning at START datetime.
(defun %unbound-recur-instance-p (start recur datetime)
  (macrolet ((implyp (condition implication)
               `(aif ,condition ,implication t)))
    (and
     ;; DATETIME is a instance of RECUR if and only if all the
     ;; following conditions are satisfied.
     (case (recur-freq recur)
       (:secondly
          (/debug
           (divisiblep (- (seconds-from-1900 datetime) (seconds-from-1900 start))
                       (recur-interval recur))))
       (:minutely
          (/debug
           (divisiblep (idiv (- (seconds-from-1900 datetime) (seconds-from-1900 start)) 60)
                       (recur-interval recur))))
       (:hourly
          (/debug
           (divisiblep (idiv (- (seconds-from-1900 datetime) (seconds-from-1900 start)) 3600)
                       (recur-interval recur))))
       (:daily
          (/debug
           (divisiblep (- (days-from-1900 datetime) (days-from-1900 start))
                       (recur-interval recur))))
       (:weekly
          (/debug
           (and
            ;; FIXME: This first condition should not be checked
            ;; here. When BYDAY rule is implemented, it will be check
            ;; the condition if no byday is specified and the freq
            ;; slot is weekly.
            (divisiblep (- (days-from-1900 datetime) (days-from-1900 start)) 7)
            (divisiblep (idiv (- (days-from-1900 datetime) (days-from-1900 start)) 7)
                        (recur-interval recur)))))
       (:monthly
          )
       (:yearly
          (/debug
           (divisiblep (- (date-year datetime) (date-year start))
                       (recur-interval recur)))))

     (/debug
      (aif (recur-bymonth recur)
           (find (date-month datetime) it)
           (implyp (freq< :monthly (recur-freq recur))
                   (= (date-month datetime) (date-month start)))))
     (/debug
      (implyp (recur-byweekno recur)
              ;; TODO: Implement suport for negative weeks
              (find (date-week-of-year datetime (recur-wkst recur)) it)))
     (/debug
      (implyp (recur-byyearday recur)
              (let ((negative-doy (- (if (leap-year-p (date-year datetime))
                                         366
                                         365)
                                     (date-day-of-year datetime)
                                     1)))
                (or (find (date-day-of-year datetime) it)
                    (find negative-doy it)))))
     (/debug
      (implyp (recur-bymonthday recur)
              (let* ((month-days (if (leap-year-p (date-year datetime))
                                     *days-in-month-leap-year*
                                     *days-in-month*))
                     (negative-dom (- (elt month-days (date-day datetime))
                                      (date-day datetime)
                                      1)))
                (or (find (date-day datetime) it)
                    (find negative-dom it)))))
     (/debug
      ;; Implement (recur-byday)
      t)
     (/debug
      (aif (recur-byhour recur)
           (find (time-hour datetime) it)
           (implyp (freq< :hourly (recur-freq recur))
                   (= (time-hour datetime) (time-hour start)))))
     
     (/debug
      (aif (recur-byminute recur)
           (find (time-minute datetime) it)
           (implyp (freq< :minutely (recur-freq recur))
                   (= (time-minute datetime) (time-minute start)))))
     (/debug
      (aif (recur-bysecond recur)
           (find (time-second datetime) it)
           (implyp (freq< :secondly (recur-freq recur))
                   (= (time-second datetime) (time-second start)))))

     (/debug
      ;; TODO: Do it!
      (aif (recur-bysetpos recur)
           t
           t)))))


(defun recur-instance-p (start recur datetime)
  ;; TODO: COUNT and UNTIL support here.
  (and (datetime<= start datetime)
       (%unbound-recur-instance-p start recur datetime)))




;;; Parsing and formating

(defun parse-rule-part (string)
  (let ((eqpos (position #\= string)))
    (when (null eqpos)
      (%parse-error "Bad rule part ~a" string))
    (cons (subseq string 0 eqpos)
          (subseq string (1+ eqpos)))))

(defun parse-rules (string)
  (let ((parts (split-string string ";" nil)))
    (when (some #'null parts)
      (%parse-error "Empty rule part in the recurrence '~a'." string))
    (mapcar #'parse-rule-part parts)))

(defmethod parse-value (string (type (eql 'recur)) &rest params &key &allow-other-keys)
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
                   (cond
                     ((string= value "SECONDLY") :secondly)
                     ((string= value "MINUTELY") :minutely)
                     ((string= value "HOURLY")   :hourly)
                     ((string= value "DAILY")    :daily)
                     ((string= value "WEEKLY")   :weekly)
                     ((string= value "MONTHLY")  :monthly)
                     ((string= value "YEARLY")   :yearly)
                     (t
                      (%parse-error "'~a' is not a valid value for the FREQ rule." value)))))
            
            ((string= key "UNTIL")
             ;; TODO: Implement this
             )
            
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
                   (parse-unsigned-integer-list value)))

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
             (setf (slot-value recur 'byyearday)
                   (parse-integer-list value)))
            
            ((string= key "BYSETPOS")
             (setf (slot-value recur 'bysetpos)
                   (parse-unsigned-integer value )))
            
            ((string= key "WKST")
             (setf (slot-value recur 'wkst)
                   (or (position value #("MO" "TU" "WE" "TH" "FR" "SA" "SU") :test #'string=)
                       ;; check-recur-consistency will signal error
                       value)))
            (t
             (%parse-error "Unknown recurrence component ~a" key)))))

      ;; Return the recur instance
      (check-recur-consistency recur)
      recur)))

(defmethod format-value ((recur recur) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (with-output-to-string (s)
    (format s "FREQ=~a"
            (case (recur-freq recur)
              (:secondly "SECONDLY")
              (:minutely "MINUTELY")
              (:hourly "HOURLY")
              (:daily "DAILY")
              (:monthly "MONTHLY")
              (:yearly "YEARLY")))
    ;; Print optional recur slots.
    (format s "~@[;COUNT=~a~]"              (recur-count recur))
    (format s "~@[;UNTIL=~a~]"              (recur-until recur))
    (format s "~@[;BYSECOND=~{~A~^, ~}~]"   (recur-bysecond recur))
    (format s "~@[;BYMINUTE=~{~A~^, ~}~]"   (recur-byminute recur))
    (format s "~@[;BYHOUR=~{~A~^, ~}~]"     (recur-byhour recur))
    (format s "~@[;BYDAY=~{~A~^, ~}~]"      (recur-byday recur))
    (format s "~@[;BYMONTH=~{~A~^, ~}~]"    (recur-bymonth recur))
    (format s "~@[;BYMONTHDAY=~{~A~^, ~}~]" (recur-bymonthday recur))
    (format s "~@[;BYYEARDAY=~{~A~^, ~}~]"  (recur-byyearday recur))
    (format s "~@[;BYWEEKNO=~{~A~^, ~}~]"   (recur-byweekno recur))
    (format s "~@[;BYSETPOS=~{~A~^, ~}~]"   (recur-bysetpos recur))))


;;; types-recur.lisp ends here
