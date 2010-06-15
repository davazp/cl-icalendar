;; types-period.lisp ---
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

(defclass period ()
  ((start
    :initarg :start
    :reader period-start)
   (end
    :initarg :end
    :reader period-end)))

(define-predicate-type period)

(defun make-period (start end)
  (make-instance 'period :start start :end end))

(defgeneric duration-of (x)
  (:method ((x period))
    ;; FIXME: Use days component of duration if possible.
    (make-duration
     :seconds (- (seconds-from-1900 (period-start x))
                 (seconds-from-1900 (period-end x))))))

(defmethod format-value ((p period) &rest params &key &allow-other-keys)
  (declare (ignore params))
  ;; TODO: We should write down in the class `period' if the user
  ;; specifies a duration or a end datetime, in order to format it so.
  (concatenate 'string
               (format-value (period-start p))
               "/"
               (format-value (period-end p))))

(defmethod parse-value (string (type (eql 'period)) &rest params &key &allow-other-keys)
  (declare (ignore params))
  (destructuring-bind (start end)
      (split-string string "/")
    (list start end)
    (let ((dstart (parse-value start 'datetime)))
      (list dstart end)
      (make-period dstart
                   (if (char= (char end 0) #\P)
                       (datetime+ dstart (parse-value end 'duration))
                       (parse-value end 'datetime))))))

;;; types-period.lisp
