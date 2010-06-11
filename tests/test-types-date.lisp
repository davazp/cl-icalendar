;; test-types-date.lisp
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

(test datep-001
  "Check `make-date' returns a date type."
  (is (datep (make-date 26 07 1989)))
  (is (datep (make-date 01 01 1970)))
  (is (datep (make-date 01 01 1900))))

(test datep-002
  "Check others objects are not date type."
  (is (not (datep 0)))
  (is (not (datep 'today)))
  (is (not (datep "yesterday")))
  (is (not (datep '(1 . 2))))
  (is (not (datep #(0 1 2 3 4)))))

(test make-date-001
  "Check arbitrary dates."
  (finishes (make-date 26 07 1989))
  (finishes (make-date 01 01 1970))
  (finishes (make-date 01 01 1900)))

(test make-date-002
  "Check wrong dates."
  (signals error (make-date  01  01  1001))
  (signals error (make-date -01  01  2000))
  (signals error (make-date  01 -01  2000))
  (signals error (make-date  01 -01 -2000)))

(test date-week-of-year-001
  (is (= (date-week-of-year (make-date 01 1 2010)) 0))
  (is (= (date-week-of-year (make-date 02 1 2010)) 0))
  (is (= (date-week-of-year (make-date 03 1 2010)) 0))
  (is (= (date-week-of-year (make-date 04 1 2010)) 1))
  (is (= (date-week-of-year (make-date 10 1 2010)) 1))
  (is (= (date-week-of-year (make-date 11 1 2010)) 2)))

;;; test-types.lisp ends here
