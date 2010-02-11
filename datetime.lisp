;; datetime.lisp
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


(defclass date ()
  ((day
    :initarg :day
    :reader date-day)
   (month
    :initarg :month
    :reader date-month)
   (year
    :initarg :year
    :reader date-year)))

(defclass time ()
  ((hour
    :initarg :hour
    :reader time-hour)
   (minute
    :initarg :minute
    :reader time-minute)
   (second
    :initarg :second
    :reader time-second)))

(defclass datetime (date time)
  ())


(defun datep (x)
  (typep x 'date))

(defun timep (x)
  (typep x 'time))

(defun datetimep (x)
  (typep x 'datetime))




;;;; Duration data type

(defvar *print-duration-abbrev* nil)

(defclass duration ()
  ((backward-p
    :initarg :backward-p
    :initform nil
    :reader duration-backward-p)
   (days
    :initarg :days
    :initform 0
    :reader duration-days)
   (hours
    :initarg :hours
    :initform 0
    :reader duration-hours)
   (minutes
    :initarg :minutes
    :initform 0
    :reader duration-minutes)
   (seconds
    :initarg :seconds
    :initform 0
    :reader duration-seconds)))


(defun durationp (x)
  (typep x 'duration))


(defmethod initialize-instance :after ((dur duration) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (labels ((deaccumulate (n acc)
             (let ((quotient (abs (idiv n (first acc)))))
               (if (endp (cdr acc))
                   (list quotient)
                   (cons (mod quotient (second acc))
                         (deaccumulate quotient (cdr acc)))))))
    (destructuring-bind (seconds minutes hours days)
        (deaccumulate (duration-in-seconds dur) '(1 60 60 24))
      (setf (slot-value dur 'days)    days)
      (setf (slot-value dur 'hours)   hours)
      (setf (slot-value dur 'minutes) minutes)
      (setf (slot-value dur 'seconds) seconds)
      (when (= 0 days hours minutes seconds)
        (setf (slot-value dur 'backward-p) nil)))))


(defun make-duration (&key (days 0) (hours 0) (minutes 0) (seconds 0) backward-p)
  (check-type days    (integer 0 *))
  (check-type hours   (integer 0 *))
  (check-type minutes (integer 0 *))
  (check-type seconds (integer 0 *))
  (make-instance 'duration
                 :days days
                 :hours hours
                 :minutes minutes
                 :seconds seconds
                 :backward-p backward-p))


(defun duration (durspec)
  (etypecase durspec
    (duration durspec)
    (integer (make-duration :seconds (abs durspec) :backward-p (> 0 durspec)))
    (string (parse-duration durspec))))


;;; Accessor for duration designators

(defmethod duration-days ((x integer))
  (duration-days (duration x)))

(defmethod duration-hours ((x integer))
  (duration-days (duration x)))

(defmethod duration-minutes ((x integer))
  (duration-days (duration x)))

(defmethod duration-seconds ((x integer))
  (duration-days (duration x)))

(defmethod duration-days ((x string))
  (duration-days (duration x)))

(defmethod duration-hours ((x string))
  (duration-days (duration x)))

(defmethod duration-minutes ((x string))
  (duration-days (duration x)))

(defmethod duration-seconds ((x string))
  (duration-days (duration x)))

(defun duration-in-seconds (durspec)
  (let ((dur (duration durspec)))
    (* (if (duration-backward-p dur) -1 1)
       (+ (* (duration-days    dur) 86400)
          (* (duration-hours   dur) 3600)
          (* (duration-minutes dur) 60)
          (* (duration-seconds dur) 1)))))


;;; Relational functions

;;; FIXME: The following relational functions compute duplicately the
;;; duration-in-seconds of N-2 durspecs.

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
    (with-slots (backward-p days hours minutes seconds)
        dur
      (with-output-to-string (out)
        (format out "~:[~;-~]" backward-p)
        (format out "P")
        (cond
          ((zerop (duration-in-seconds dur))
           (format out "T0S"))
          ((zerop (mod (duration-in-seconds dur) 604800))
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


;;; datetime.lisp ends here
