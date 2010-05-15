;; test-types-recur.lisp
;;
;; Copyrigth (C) 2010 David Vázquez
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

(in-package :cl-icalendar-tests)

(in-suite icalendar-types)

;;; Recur data type tests

(test recur-instance-p-interval-001
  "Check INTERVAL rule."
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (make-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (make-datetime 03 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (make-datetime 05 01 2000 00 00 00)))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (make-datetime 02 01 2000 00 00 00))))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (make-datetime 04 01 2000 00 00 00))))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (make-datetime 06 01 2000 00 00 00)))))

(test recur-instance-p-interval-002
  "Check INTERVAL rule."
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (make-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (make-datetime 15 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (make-datetime 29 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (make-datetime 12 02 2000 00 00 00)))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (make-datetime 08 01 2000 00 00 00))))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (make-datetime 02 01 2000 00 00 00))))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (make-datetime 22 01 2000 00 00 00))))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (make-datetime 05 02 2000 00 00 00)))))

(test recur-instance-p-bymonth
  "Check BYMONTH rule."
  (is (recur-instance-p (make-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                        (make-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (make-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                        (make-datetime 31 01 2000 00 00 00)))
  (is (not (recur-instance-p (make-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                             (make-datetime 01 02 2000 00 00 00))))
  (is (recur-instance-p (make-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=YEARLY;BYMONTH=1,2" 'recur)
                        (make-datetime 01 02 2000 00 00 00))))


;;; test-types-recur.lisp ends here
