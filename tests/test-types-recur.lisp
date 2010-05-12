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

(test recur-instance-p-bymonth
  "Check BYMONTH rule."
  (is (recur-instance-p (make-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                        (make-datetime 01 01 2000 00 00 00))))

;;; test-types-recur.lisp ends here
