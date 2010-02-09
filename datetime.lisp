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

(defun get-time (&optional time)
  (multiple-value-list (decode-universal-time (or time (get-universal-time)))))

(defun time-second (&optional time)
  (nth 0 (get-time time)))

(defun time-minute (&optional time)
  (nth 1 (get-time time)))

(defun time-hour (&optional time)
  (nth 2 (get-time time)))

(defun time-day (&optional time)
  (nth 3 (get-time time)))

(defun time-month (&optional time)
  (nth 4 (get-time time)))

(defun time-year (&optional time)
  (nth 5 (get-time time)))


(declaim (inline time= time< time<= time> time>=))

(define-transitive-relation time= (a b)
  (= a b))

(define-transitive-relation time< (a b)
  (< a b))

(define-transitive-relation time<= (a b)
  (<= a b))

(define-transitive-relation time> (a b)
  (> a b))

(define-transitive-relation time>= (a b)
  (>= a b))

(defun time+ (x durspec)
  (+ x (duration durspec)))

(defun time- (x durspec)
  (- x (duration durspec)))


;;;; Duration

(deftype duration () 'integer)

(defun durationp (x)
  (typep x 'duration))

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
          (prog1 (dur-value)
            (unless (null token1)
              (ill-formed))))))))

(defun duration (durspec)
  (etypecase durspec
    (duration durspec)
    (string (parse-duration durspec))))



;;; datetime.lisp ends here
