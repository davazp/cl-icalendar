;; types.lisp
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

;;;; Boolean

(defun format-boolean (bool)
  (ecase bool
    ((t) "TRUE")
    ((nil) "FALSE")))

(defun parse-boolean (string)
  (cond
    ((string= string "TRUE")  t)
    ((string= string "FALSE") nil)
    (t
     (error "~a is not a boolean data type." string))))


;;;; Float

(defun format-float (x)
  (format nil "~f" x))

(defun parse-float (string)
  (let ((sign 1)                        ; the sign
        (x 0)                           ; integer part
        (y 0))                          ; fractional part
    (with-input-from-string (in string)
      ;; Read sign
      (case (peek-char nil in)
        (#\+
         (read-char in))
        (#\-
         (setf sign -1)
         (read-char in)))
        
      ;; Read integer part
      (let ((istring (read-until in (complement #'digit-char-p) nil nil)))
        (setf x (parse-integer istring)))
        
      ;; Read fractinal part (if any)
      (let ((dot (read-char in nil)))
        (unless (null dot)
          (unless (char= dot #\.)
            (error "Bad formed float."))

          (let ((fstring (read-until in (complement #'digit-char-p) nil nil)))
            (setf y (/ (float (parse-integer fstring))
                       (expt 10 (length fstring))))))))

    (* sign (+ x y))))


;;;; Text

(defclass text* ()
  ((language
    :initarg :language
    :initform nil
    :reader text-language)
   (string
    :initarg :text
    :reader text)))

(defmethod print-object ((x text*) stream)
  (print-unreadable-object (x stream)
    (format stream "TEXT :LANG ~a ~w"
            (text-language x)
            (text x))))

(deftype text ()
  '(or string text*))

(defun textp (x)
  (typep x 'text))

(defmethod text ((x string))
  x)

(defmethod text-language ((x string))
  nil)

(defun make-text (string &optional language)
  (if language
      (make-instance 'text* :text string :language language)
      string))

(defun format-text (text)
  (let ((string (text text)))
    (with-input-from-string (in string)
      (with-output-to-string (out)
        (loop for ch = (read-char in nil)
              while ch
              do
              (cond
                ((char= ch #\newline)
                 (write-char #\\ out)
                 (write-char #\n out))
                ((char= ch #\\)
                 (write-char #\\ out)
                 (write-char #\\ out))
                ((char= ch #\,)
                 (write-char #\\ out)
                 (write-char #\, out))
                ((char= ch #\;)
                 (write-char #\\ out)
                 (write-char #\; out))
                (t
                 (write-char ch out))))))))


(defun parse-text (text)
  (let ((string (text text)))
    (with-input-from-string (in string)
      (with-output-to-string (out)
        (loop for ch = (read-char in nil)
              while ch
              do
              (write-char (if (char/= ch #\\)
                              ch
                              (ecase (read-char in nil)
                                (#\\ #\\)
                                (#\; #\;)
                                (#\, #\,)
                                (#\N #\newline)
                                (#\n #\newline)))
                          out))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Time data types ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Datetime

(defclass datetime ()
  ((timestamp
    :initarg :timestamp
    :reader datetimestamp)))

(defclass date ()
  ((timestamp
    :initarg :timestamp
    :reader datestamp)))

(defclass time ()
  ((timestamp
    :initarg :timestamp
    :reader timestamp)))

(defgeneric universal-time (x))
(defmethod universal-time ((x datetime))
  (datetimestamp x))
(defmethod universal-time ((x date))
  (datestamp x))
(defmethod universal-time ((x time))
  (timestamp x))

;;; Accessors
(defun date-day (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    date))

(defun date-month (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    month))

(defun date-year (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    year))

(defun time-hour (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    hour))

(defun time-minute (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    minute))

(defun time-second (x)
  (multiple-value-bind (second minute hour date month year day daylight-p zone)
      (decode-universal-time (universal-time x))
    (declare (ignorable second minute hour date month year day daylight-p zone))
    second))


;;; TODO: The TZONE argument will be implemented when the module
;;; components is ready.
(defun make-datetime (day month year hour minute second &optional tzone)
  (declare (ignore tzone))
  (make-instance 'datetime
                 :timestamp
                 (encode-universal-time second minute hour
                                        day month year)))

(defun datetimep (x)
  (typep x 'datetime))

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
  (= (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime< (x y)
  (< (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime<= (x y)
  (<= (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime> (x y)
  (> (datetimestamp x) (datetimestamp y)))

(define-transitive-relation datetime>= (x y)
  (>= (datetimestamp x) (datetimestamp y)))


;;; Compositional functions

(defun datetime+ (datetime durspec)
  (make-instance 'datetime
                 :timestamp (+ (datetimestamp datetime)
                               (duration-in-seconds durspec))))

(defun datetime- (datetime durspec)
  (make-instance 'datetime
                 :timestamp (- (datetimestamp datetime)
                               (duration-in-seconds durspec))))

;;; Decompound

(defun datetime-date (dt)
  (check-type dt datetime)
  (make-date (date-day dt)
             (date-month dt)
             (date-year dt)))

(defun datetime-time (dt)
  (check-type dt datetime)
  (make-time (time-hour dt)
             (time-minute dt)
             (time-second dt)))

;;; Parser

(defun format-datetime (dt)
  (check-type dt datetime)
  (concatenate 'string
               (format-date (datetime-date dt))
               "T"
               (format-time (datetime-time dt))))

(defun parse-datetime (string)
  ;; TODO: Handling timezones
  (let ((string-date (subseq string 0  8))
        (string-time (subseq string 9 15)))
    (flet ((ill-formed ()
             (error "Bad datetime format.")))
      (unless (char= (elt string 8) #\T)
        (ill-formed))
      
      (let ((date   (parse-date string-date))
            (time   (parse-time string-time)))
        (make-datetime (date-day    date)
                       (date-month  date)
                       (date-year   date)
                       (time-hour   time)
                       (time-minute time)
                       (time-second time))))))


;;; Date

(defun make-date (day month year)
  (make-instance 'date :timestamp (encode-universal-time 0 0 0 day month year)))

(defun datep (x)
  (typep x 'date))

(defmethod print-object ((x date) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d-~2,'0d-~4,'0d"
            (date-day x)
            (date-month x)
            (date-year x))))

(define-transitive-relation date= (x y)
  (= (datestamp x) (datestamp y)))

(define-transitive-relation date< (x y)
  (< (datestamp x) (datestamp y)))

(define-transitive-relation date<= (x y)
  (<= (datestamp x) (datestamp y)))

(define-transitive-relation date> (x y)
  (> (datestamp x) (datestamp y)))

(define-transitive-relation date>= (x y)
  (>= (datestamp x) (datestamp y)))


(defun date+ (date durspec)
  (let* ((dur (duration durspec))
         (sec (duration-in-seconds durspec)))
    (unless (divisiblep sec 86400)
      (error "The duration ~a is not multiple of days" dur))
    (make-instance 'date :timestamp (+ (datestamp date) sec))))


(defun date- (date durspec)
  (let* ((dur (duration durspec))
         (sec (duration-in-seconds durspec)))
    (unless (divisiblep sec 86400)
      (error "The duration ~a is not multiple of days" dur))
    (make-instance 'date :timestamp (- (datestamp date) sec))))


(defun format-date (date)
  (check-type date date)
  (format nil "~4,'0d~2,'0d~2,'0d"
          (date-year date)
          (date-month date)
          (date-day date)))


(defun parse-date (string)
  (unless (= (length string) 8)
    (error "parse error."))
  (make-date (parse-unsigned-integer string :start 6 :end 8)
             (parse-unsigned-integer string :start 4 :end 6)
             (parse-unsigned-integer string :start 0 :end 4)))


;;; Time

(defun make-time (hour minute second)
  (make-instance 'time :timestamp (+ (* hour 3600) (* minute 60) second)))

(defun timep (x)
  (typep x 'time))

(defmethod print-object ((x time) stream)
  (print-unreadable-object (x stream :type t)
    (format stream "~2,'0d:~2,'0d:~2,'0d"
            (time-hour   x)
            (time-minute x)
            (time-second x))))

(define-transitive-relation time= (x y)
  (= (timestamp x) (timestamp y)))

(define-transitive-relation time< (x y)
  (< (timestamp x) (timestamp y)))

(define-transitive-relation time<= (x y)
  (<= (timestamp x) (timestamp y)))

(define-transitive-relation time> (x y)
  (> (timestamp x) (timestamp y)))

(define-transitive-relation time>= (x y)
  (>= (timestamp x) (timestamp y)))


(defun time+ (date durspec)
  (let ((x (+ (timestamp date) (duration-in-seconds durspec))))
    (if (<= 0 x 86399)
        (make-instance 'time :timestamp x)
        (error "Time overflow"))))

(defun time- (date durspec)
  (let ((x (+ (timestamp date) (duration-in-seconds durspec))))
    (if (<= 0 x 86399)
        (make-instance 'time :timestamp x)
        (error "Time overflow"))))

(defun format-time (time)
  (check-type time time)
  (format nil "~2,'0d~2,'0d~2,'0d"
          (time-hour   time)
          (time-minute time)
          (time-second time)))

(defun parse-time (string)
  (unless (or (= (length string) 6)
              (= (length string) 7))
    (error "parse error."))
  (make-time (parse-unsigned-integer string :start 0 :end 2)
             (parse-unsigned-integer string :start 2 :end 4)
             (parse-unsigned-integer string :start 4 :end 6)))



;;;; Duration data type

(defvar *print-duration-abbrev* nil)

(defclass duration ()
  ((seconds
    :initarg :seconds
    :reader duration-in-seconds)))

(defgeneric duration-days (duration))
(defgeneric duration-hours (duration))
(defgeneric duration-minutes (duration))
(defgeneric duration-seconds (duration))
(defgeneric duration-backward-p (duration))

(defun durationp (x)
  (typep x 'duration))

(defun make-duration (&key (days 0) (hours 0) (minutes 0) (seconds 0) backward-p)
  (check-type days    (integer 0 *))
  (check-type hours   (integer 0 *))
  (check-type minutes (integer 0 *))
  (check-type seconds (integer 0 *))
  (make-instance 'duration
                 :seconds
                 (* (if backward-p -1 1)
                    (+ (* days  84706)
                       (* hours 3600)
                       (* minutes 60)
                       seconds))))

(defun duration (durspec)
  (etypecase durspec
    (duration durspec)
    (integer (make-duration :seconds (abs durspec) :backward-p (> 0 durspec)))
    (string (parse-duration durspec))))


;;; Accessor for duration designators

(defmethod duration-days ((x duration))
  (duration-days (duration-in-seconds x)))

(defmethod duration-hours ((x duration))
  (duration-hours (duration-in-seconds x)))

(defmethod duration-minutes ((x duration))
  (duration-minutes (duration-in-seconds x)))

(defmethod duration-seconds ((x duration))
  (duration-seconds (duration-in-seconds x)))

(defmethod duration-backward-p ((x duration))
  (duration-backward-p (duration-in-seconds x)))


(defmethod duration-days ((x integer))
  (abs (idiv x 86400)))

(defmethod duration-hours ((x integer))
  (mod (abs (idiv x 3600)) 24))

(defmethod duration-minutes ((x integer))
  (mod (abs (idiv x 60)) 60))

(defmethod duration-seconds ((x integer))
  (mod (abs x) 60))

(defmethod duration-backward-p ((x integer))
  (< x 0))

(defmethod duration-in-seconds ((x integer))
  x)


(defmethod duration-days ((x string))
  (duration-days (duration x)))

(defmethod duration-hours ((x string))
  (duration-hours (duration x)))

(defmethod duration-minutes ((x string))
  (duration-minutes (duration x)))

(defmethod duration-seconds ((x string))
  (duration-seconds (duration x)))

(defmethod duration-backward-p ((x string))
  (duration-backward-p (duration x)))

(defmethod duration-in-seconds ((x string))
  (duration-in-seconds (duration x)))


;;; Relational functions

(define-transitive-relation duration= (x y)
  (= (duration-in-seconds x)
     (duration-in-seconds y)))

(define-transitive-relation duration< (x y)
  (< (duration-in-seconds x)
     (duration-in-seconds y)))

(define-transitive-relation duration<= (x y)
  (<= (duration-in-seconds x)
      (duration-in-seconds y)))

(define-transitive-relation duration> (x y)
  (> (duration-in-seconds x)
     (duration-in-seconds y)))

(define-transitive-relation duration>= (x y)
  (>= (duration-in-seconds x)
      (duration-in-seconds y)))


;;; Compositional functions

(defun duration+ (&rest args)
  (duration (reduce #'+ (mapcar #'duration-in-seconds args))))

(defun duration- (x &rest args)
  (if (null args)
      (duration (- (duration-in-seconds x)))
      (duration (reduce #'- (mapcar #'duration-in-seconds (cons x args))))))


;;; Printer
(defmethod print-object ((x duration) stream)
  (print-unreadable-object (x stream :type t)
    (let* ((component-names
            (if *print-duration-abbrev*
                '("d"   "h"    "m"      "s")
                '("day" "hour" "minute" "second")))
           (output
            (loop with i = 0
                  for c in component-names
                  for n in (list (duration-days    x)
                                 (duration-hours   x)
                                 (duration-minutes x)
                                 (duration-seconds x))
                  unless (zerop n)  
                  collect n and collect c and do (incf i))))
      (cond
        ((null output)
         (format stream "empty duration"))
        (*print-duration-abbrev*
         (format stream "~{~d~a~^ ~}" output))
        (t
         (format stream "~{~d ~a~2:*~p~*~#[~;~; and ~:;, ~]~}" output)))

      (when (duration-backward-p x)
        (format stream " to BACKWARD")))))


;;; Return a string which stand for DURSPECS in the format described
;;; in the RFC5545 section 3.3.6.
(defun format-duration (durspec)
  (let ((dur (duration durspec)))
    (let ((days (duration-days dur))
          (hours (duration-hours dur))
          (minutes (duration-minutes dur))
          (seconds (duration-seconds dur))
          (backward-p (duration-backward-p dur)))
      (with-output-to-string (out)
        (format out "~:[~;-~]" backward-p)
        (format out "P")
        (cond
          ((zerop (duration-in-seconds dur))
           (format out "T0S"))
          ((divisiblep (duration-in-seconds dur) 604800)
           (format out "~aW" (/ days 7)))
          (t
           (unless (zerop days)
             (format out "~aD" days))
           (cond
             ((= 0 hours minutes seconds))
             ((and (zerop minutes)
                   (and (not (zerop hours))
                        (not (zerop seconds))))
              (format out "T~aH~aM~aS" hours minutes seconds))
             (t
              (format out "T~[~:;~:*~aH~]~[~:;~:*~aM~]~[~:;~:*~aS~]"
                      hours
                      minutes
                      seconds)))))))))


;;; Parse a duration according to the format which is described in the
;;; RFC5545 section 3.3.6.
(defun parse-duration (string)
  (with-input-from-string (in string)
    (flet (;; Get a token from IN
           (get-token ()
             (let ((ch (peek-char nil in nil)))
               (cond
                 ((null ch)
                  nil)
                 ((digit-char-p ch)
                  (values
                   (parse-integer
                    (read-until in (complement #'digit-char-p) "" nil))))
                 (t
                  (read-char in))))))

      (let* (;; The following token
             (token1 (get-token))
             ;; The following of the following token
             (token2 (get-token)))

        (labels (;; Return the following token, then upgrade the
                 ;; values of token1 and token2. If there is not
                 ;; following token, then signal a error.
                 (scan ()
                   (if token1
                       (prog1 token1
                         (setf token1 token2)
                         (setf token2 (get-token)))
                       (ill-formed)))

                 ;; Signal a ill-formed error
                 (ill-formed ()
                   (error "bad formed duration."))
                 
                 ;; Check the current token is the character CH and
                 ;; read a new token. Otherwise, a error is signaled.
                 (check-character (ch)
                   (unless (char= (scan) ch)
                     (ill-formed)))

                 ;; The following functions stand for a state, which
                 ;; parse a subset of the grammar and return the
                 ;; number of seconds which is represented by the
                 ;; matched part in input.
                 ;;
                 ;; If the input is not matched then an error is
                 ;; signaled.
                 ;; ...

                 (dur-value ()
                   (funcall (case (scan)
                              (#\+
                               (check-character #\P)
                               #'identity)
                              (#\-
                               (check-character #\P)
                               #'-)
                              (#\P #'identity)
                              (t
                               (ill-formed)))
                            (cond
                              ((eql token1 #\T)
                               (dur-time))
                              ((eql token2 #\W)
                               (dur-week))
                              ((eql token2 #\D)
                               (dur-date))
                              (t
                               (ill-formed)))))

                 (dur-date ()
                   (+ (dur-day)
                      (if token1
                          (dur-time)
                          0)))
                 
                 (dur-time ()
                   (check-character #\T)
                   (case token2
                     (#\H (dur-hour))
                     (#\M (dur-minute))
                     (#\S (dur-second))
                     (t
                      (ill-formed))))

                 (dur-week ()
                   (prog1 (* (scan) 604800)
                     (check-character #\W)))

                 (dur-hour ()
                   (+ (prog1 (* (scan) 3600)
                        (check-character #\H))
                      (if token1
                          (dur-minute)
                          0)))

                 (dur-minute ()
                   (+ (prog1 (* (scan) 60)
                        (check-character #\M))
                      (if token1
                          (dur-second)
                          0)))

                 (dur-second ()
                   (prog1 (scan)
                     (check-character #\S)))

                 (dur-day ()
                   (prog1 (* (scan) 86400)
                     (check-character #\D))))

          ;; Estado inicial
          (prog1 (duration (dur-value))
            (unless (null token1)
              (ill-formed))))))))



;;;; Period

(defclass period ()
  ((start
    :initarg :start
    :reader period-start)
   (end
    :initarg :end
    :reader period-end)))

(defun make-period (start end)
  (make-instance 'period :start start :end end))

(defun period-duration (period)
  (let ((start (period-start period))
        (end   (period-end period)))
    (make-duration  :seconds
                    (- (universal-time start)
                       (universal-time end)))))

(defun format-period (p)
  ;; TODO: We should write down in the class `period' if the user
  ;; specifies a duration or a end datetime, in order to format it so.
  (format nil "~a/~a" (period-start p) (period-end p)))

(defun parse-period (string)
  (destructuring-bind (start end)
      (split-string string "/")
    (let ((dstart (parse-datetime start)))
      (make-period dstart
                   (if (char= (char end 0) #\P)
                       (datetime+ dstart (parse-duration end))
                       (parse-datetime end))))))



;;;; Recur data type

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
    :initform nil
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
    :initform nil
    :reader recur-wkst)))


(deftype non-zero-integer (a b)
  `(or (integer ,(- b) ,(- a))
       (integer  ,a  ,b)))

(deftype frequency ()
  '(member
    :secondly
    :minutely
    :hourly
    :daily
    :weekly
    :monthly
    :yearly))

(deftype weekday ()
  '(member
    :sunday
    :monday
    :tuesday
    :wednesday
    :thursday
    :friday
    :saturday))

(defun check-valid-recur (recur)
  (with-slots (freq until count interval
                    bysecond byminute byhour
                    byday bymonthday byyearday
                    byweekno bymonth bysetpos
                    wkst)
      recur

    (macrolet ((check-type-list (list type)
                 `(dolist (i ,list)
                    (check-type i ,type))))

      (check-type freq frequency)
      (assert (or (not until) (not count)))

      (and until    (check-type until (or date datetime)))
      (and count    (check-type count (integer 0 *)))
      (and interval (check-type interval (integer 0 *)))

      (check-type-list bysecond (integer 0 60))
      (check-type-list byminute (integer 0 59))
      (check-type-list byhour   (integer 0 23))
      ;; TODO: Write a checker for byday
      ;;(check-type-list byday)

      (check-type-list bymonthday (non-zero-integer  -31  31))
      (check-type-list byyearday  (non-zero-integer -366 366))
      (check-type-list byweekno   (non-zero-integer   -7   7))
      (check-type-list bymonth    (integer 0 12))
      (check-type-list bysetpos   (non-zero-integer -366 366))

      (and wkst (check-type wkst weekday )))))


;;; Parsing

(defun parse-rule-part (string)
  (let ((eqpos (position #\= string)))
    (when (null eqpos)
      (error "Bad rule part ~a" string))
    (cons (subseq string 0 eqpos)
          (subseq string (1+ eqpos)))))

(defun parse-rules (string)
  (let ((parts (split-string string ";" nil)))
    (when (some #'null parts)
      (error "Empty rule part in the recurrence '~a'." string))
    (mapcar #'parse-rule-part parts)))

(defun parse-recur (string)
  (let ((rules (parse-rules string)))
    ;; Check errores
    (when (duplicatep rules :key #'car :test #'string=)
      (error "Duplicate key in recurrence."))
    (let ((recur (make-instance 'recur)))

      (macrolet (;; Iterate on the substrings in a multiple-value.
                 (do-list-values ((var value) &body body)
                   (with-gensyms (list)
                     `(let ((,list (split-string ,value "," nil)))
                        (assert (not (null ,list)))
                        (mapcar (lambda (,var) ,@body)
                                ,list)))))
        (flet ((%freq (value)
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
                          (error "'~a' is not a valid value for the FREQ rule." value)))))

               (%until (value)
                 (setf (slot-value recur 'until)
                       (case (length value)
                         (8 (parse-date value))
                         (t (parse-datetime value)))))

               (%count (value)
                 (setf (slot-value recur 'count)
                       (parse-unsigned-integer value)))
               
               (%interval (value)
                 (setf (slot-value recur 'interval)
                       (parse-unsigned-integer value)))

               (%bysecond (value)
                 (setf (slot-value recur 'bysecond)
                       (do-list-values (sn value)
                         (parse-unsigned-integer sn))))

               (%byminute (value)
                 (setf (slot-value recur 'byminute)
                       (do-list-values (sn value)
                         (parse-unsigned-integer sn))))

               (%byhour (value)
                 (setf (slot-value recur 'byhour)
                       (do-list-values (sn value)
                         (parse-unsigned-integer sn))))

               (%byday (value)
                 (setf (slot-value recur 'byday)
                       (do-list-values (str value)
                         (multiple-value-bind (n endn)
                             (parse-integer str :junk-allowed t)
                           (list n (subseq str endn))))))

               (%bymonthday (value)
                 (setf (slot-value recur 'bymonthday)
                       (do-list-values (sn value)
                         (parse-unsigned-integer sn))))

               (%byyearday (value)
                 (setf (slot-value recur 'byyearday)
                       (do-list-values (sn value)
                         (parse-integer sn))))

               (%byweekno (value)
                 (setf (slot-value recur 'byyearday)
                       (do-list-values (sn value)
                         (parse-integer sn))))
               
               (%bymonth (value)
                 (setf (slot-value recur 'byyearday)
                       (do-list-values (sn value)
                         (parse-unsigned-integer sn))))

               (%bysetpos (value)
                 (setf (slot-value recur 'byyearday)
                       (do-list-values (sn value)
                         (parse-integer sn))))

               (%wkst (value)
                 (setf (slot-value recur 'wkst) 
                       (cond
                         ((string= value "SU") :sunday)
                         ((string= value "MO") :monday)
                         ((string= value "TU") :tuesday)
                         ((string= value "WE") :wednesday)
                         ((string= value "TH") :thursday)
                         ((string= value "FR") :friday)
                         ((string= value "SA") :saturday)
                         (t
                          (error "No a weekday."))))))

          ;; Scan rules
          (dolist (rule rules)
            (destructuring-bind (key . value)
                rule
              (cond
                ((string= key "FREQ")
                 (%freq value))
                ((string= key "UNTIL")
                 (%until value))
                ((string= key "COUNT")
                 (%count value))
                ((string= key "INTERVAL")
                 (%interval value))
                ((string= key "BYSECOND")
                 (%bysecond value))
                ((string= key "BYMINUTE")
                 (%byminute value))
                ((string= key "BYHOUR")
                 (%byhour value))
                ((string= key "BYDAY")
                 (%byday value))
                ((string= key "BYMONTHDAY")
                 (%bymonthday value))
                ((string= key "BYYEARDAY")
                 (%byyearday value))
                ((string= key "BYWEEKNO")
                 (%byweekno value))
                ((string= key "BYMONTH")
                 (%bymonth value))
                ((string= key "BYSETPOS")
                 (%bysetpos value))
                ((string= key "WKST")
                 (%wkst value))))))

        ;; Return the recur instance
        (check-valid-recur recur)
        recur))))


;;; types.lisp ends here
