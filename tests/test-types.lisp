;; test-types.lisp
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

;;; Boolean data type

(test booleanp-001
  "Check membership to boolean data type."
  (is (booleanp t))
  (is (booleanp nil))
  (is (not (booleanp 3)))
  (is (not (booleanp "true"))))

(test parse-value-boolean-001
  "Parse some boolean values."
  (is (parse-value "true" 'boolean))
  (is (parse-value "TRUE" 'boolean))
  (is (parse-value "tRUE" 'boolean))
  (is (not (parse-value "false" 'boolean)))
  (is (not (parse-value "FALSE" 'boolean)))
  (is (not (parse-value "fAlse" 'boolean))))

(test parse-value-boolean-002
  "Parse some non-boolean values."
  (signals error (parse-value "23" 'boolean))
  (signals error (parse-value "t" 'boolean))
  (signals error (parse-value "true2" 'boolean))
  (signals error (parse-value "falses" 'boolean)))

(test format-value-boolean-001
  "Format some boolean values."
  (is (string= (format-value t)   "TRUE"))
  (is (string= (format-value nil) "FALSE")))



;;; Date data type

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


;;; Period type

(test parse-value-period-001
  (is (periodp (parse-value "19970101T180000Z/PT5H30M" 'period))))

(test parse-value-period-002
  (is (periodp (parse-value "19970101T180000Z/19970102T070000Z" 'period))))


;;; test-types.lisp ends here
