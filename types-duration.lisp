;; types-duration.lisp --- Duration data type implementation
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

(defclass duration ()
  ((days
    :initarg :days
    :initform 0
    :reader %duration-days)
   (seconds
    :initarg :seconds
    :initform 0
    :reader %duration-seconds)))

(register-ical-value duration)
(define-predicate-type duration)

(defun make-duration (&key (days 0) (hours 0) (minutes 0) (seconds 0) backward-p)
  (declare (type (integer 0 *) days hours minutes seconds))
  (make-instance 'duration
                 :days (if backward-p (- days) days)
                 :seconds (* (if backward-p -1 1)
                             (+ (* hours 3600)
                                (* minutes 60)
                                seconds))))
(defun duration (durspec)
  (etypecase durspec
    (duration durspec)
    (string (parse-value durspec 'duration))))


;;; Accessor for duration designators

(defgeneric duration-days (duration)
  (:method ((x duration))
    (abs (%duration-days x))))

(defgeneric duration-hours (duration)
  (:method ((x duration))
    (truncate (abs (%duration-seconds x)) 3600)))

(defgeneric duration-minutes (duration)
  (:method ((x duration))
    (mod (abs (truncate (%duration-seconds x) 60)) 60)))

(defgeneric duration-seconds (duration)
  (:method ((x duration))
    (mod (abs (%duration-seconds x)) 60)))

(defgeneric duration-backward-p (duration)
  (:method ((x duration))
    (or (< (%duration-seconds x) 0)
        (< (%duration-days    x) 0))))

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

(defgeneric duration-inverse (duration)
  (:method ((d1 duration))
    (make-instance 'duration
                   :days    (- (%duration-days    d1))
                   :seconds (- (%duration-seconds d1)))))

;;; Printer
(defvar *print-duration-abbrev* nil)

(defprinter (x duration)
  (let* ((component-names
          (if *print-duration-abbrev*
              '("d"   "h"    "m"      "s")
              '("day" "hour" "minute" "second")))
         (output
          (loop for c in component-names
                for n in (list (duration-days    x)
                               (duration-hours   x)
                               (duration-minutes x)
                               (duration-seconds x))
                unless (zerop n)
                collect n and collect c)))
    (cond
      ((null output)
       (format t "empty duration"))
      (*print-duration-abbrev*
       (format t "~{~d~a~^ ~}" output))
      (t
       (format t "~{~d ~a~2:*~p~*~#[~;~; and ~:;, ~]~}" output)))
    (when (duration-backward-p x)
      (format t " to BACKWARD"))))


;;; Return a string which stand for DURSPECS in the format described
;;; in the RFC5545 section 3.3.6.
(defmethod format-value ((dur duration) (type (eql 'duration)) &optional params)
  (declare (ignore params))
  (let ((days       (duration-days dur))
        (hours      (duration-hours dur))
        (minutes    (duration-minutes dur))
        (seconds    (duration-seconds dur))
        (backward-p (duration-backward-p dur)))
    (with-output-to-string (out)
      (format out "~:[~;-~]" backward-p)
      (format out "P")
      (cond
        ((and (zerop (%duration-seconds dur))
              (zerop (%duration-days dur)))
         (format out "T0S"))
        ((and (zerop (%duration-seconds dur))
              (zerop (mod (%duration-days dur) 7)))
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
                    seconds))))))))


;;; Parse a duration according to the format which is described in the
;;; RFC5545 section 3.3.6.
(defmethod parse-value (string (type (eql 'duration)) &optional params)
  (declare (ignore params))
  (with-input-from-string (in string)
    ;; The token1 and token2 variables keep the following token in the
    ;; automata and the following of the following one.
    (let* (token1
           token2)
      (labels (
               ;; Read and return a token from the input stream. This
               ;; is intended to be used by `scan' internally.
               (%scan-token ()
                 ;; Read and return a new token.
                 (let ((ch (peek-char nil in nil)))
                   (cond
                     ((null ch)
                      nil)
                     ((digit-char-p ch)
                      (values
                        (parse-integer
                         (read-until in (complement #'digit-char-p) "" nil))))
                     (t
                      (read-char in)))))
               ;; Return the following token, then upgrade the
               ;; values of token1 and token2. If there is not
               ;; following token, then signal a error.
               (scan ()
                 (if token1
                     (prog1 token1
                       (setf token1 token2)
                       (setf token2 (%scan-token)))
                     (ill-formed)))

               ;; Signal a ill-formed error
               (ill-formed ()
                 (%parse-error "bad formed duration."))

               ;; Add a duration to other. It is consistent if both
               ;; D1 and D2 are not backward durations.
               (duration+ (d1 d2)
                 (make-instance 'duration
                                :days
                                (+ (%duration-days d1)
                                   (%duration-days d2))
                                :seconds
                                (+ (%duration-seconds d1)
                                   (%duration-seconds d2))))

               ;; Check the current token is the character CH and
               ;; read a new token. Otherwise, a error is signaled.
               (check-character (ch)
                 (unless (char= (scan) ch)
                   (ill-formed)))

               ;; The following functions stand for a state, which
               ;; parse a subset of the grammar and return a partial
               ;; duration object.
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
                               #'duration-inverse)
                            (#\P
                               #'identity)
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
                 (duration+ (dur-day)
                            (if token1
                                (dur-time)
                                (make-duration))))

               (dur-time ()
                 (check-character #\T)
                 (case token2
                   (#\H (dur-hour))
                   (#\M (dur-minute))
                   (#\S (dur-second))
                   (t (ill-formed))))

               (dur-week ()
                 (prog1 (make-duration :days (* (scan) 7))
                   (check-character #\W)))

               (dur-hour ()
                 (duration+ (prog1 (make-duration :hours (scan))
                              (check-character #\H))
                            (if token1
                                (dur-minute)
                                (make-duration))))

               (dur-minute ()
                 (duration+ (prog1 (make-duration :minutes (scan))
                              (check-character #\M))
                            (if token1
                                (dur-second)
                                (make-duration))))

               (dur-second ()
                 (prog1 (make-duration :seconds (scan))
                   (check-character #\S)))

               (dur-day ()
                 (prog1 (make-duration :days (scan))
                   (check-character #\D))))

        ;; Initialize scanner token1 and token2 variables.
        (setf token1 (%scan-token))
        (setf token2 (%scan-token))

        ;; Initial state
        (prog1 (duration (dur-value))
          ;; We make sure we have parse all string.
          (unless (null token1)
            (ill-formed)))))))


;;; types-duration.lisp
