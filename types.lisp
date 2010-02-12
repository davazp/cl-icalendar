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

;;;; Date, time, and date-time

(defclass datetime ()
  ((timestamp
    :initarg :timestamp
    :reader timestamp)))

(defgeneric date-day (x))
(defgeneric date-month (x))
(defgeneric date-year (x))
(defgeneric time-hour (x))
(defgeneric time-minute (x))
(defgeneric time-second (x))

(defmethod date-day ((x datetime))
  (local-time:timestamp-day (timestamp x)))

(defmethod date-month ((x datetime))
  (local-time:timestamp-month (timestamp x)))

(defmethod date-year ((x datetime))
  (local-time:timestamp-year (timestamp x)))

(defmethod time-hour ((x datetime))
  (local-time:timestamp-hour (timestamp x)))

(defmethod time-minute ((x datetime))
  (local-time:timestamp-hour (timestamp x)))

(defmethod time-second ((x datetime))
  (local-time:timestamp-second (timestamp x)))

;;; Relational functions

(define-transitive-relation datetime= (x y)
  (local-time:timestamp= (timestamp x) (timestamp y)))

(define-transitive-relation datetime< (x y)
  (local-time:timestamp< (timestamp x) (timestamp y)))

(define-transitive-relation datetime<= (x y)
  (local-time:timestamp<= (timestamp x) (timestamp y)))

(define-transitive-relation datetime> (x y)
  (local-time:timestamp> (timestamp x) (timestamp y)))

(define-transitive-relation datetime>= (x y)
  (local-time:timestamp>= (timestamp x) (timestamp y)))

;;; Printer

(defmethod print-object ((x datetime) stream)
  (print-unreadable-object (x stream :type t)
    (print-object (timestamp x) stream)))




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
(defgeneric duration-in-seconds (duration))
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
                       (+ seconds )))))

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

(defun parse-float (string)
  (with-input-from-string (in string)
    (let* ((sign (case (read-char in)
		   (#\+ 1)
		   (#\- -1)
		   (t (error "First char should be +/-"))))
	   (int-part (read-until in "." nil nil))
	   (rest (read-until in nil nil nil))
	   (decimals (unless (zerop (length rest))
		       (subseq rest 1))))
      (float (+ (parse-integer int-part)
		(/ (if decimals
		       (parse-integer decimals) 0)
		   (expt 10 (length decimals))
		   sign))))))

;;; types.lisp ends here
