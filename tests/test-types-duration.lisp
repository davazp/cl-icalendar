;; test-types-duration.lisp
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

;;; Parsing

(test parse-value-duration-001
  "Parse some simple duration values."
  (is (= (duration-days (parse-value "P1D"  'duration))  1))
  (is (= (duration-days (parse-value "P7D"  'duration))  7))
  (is (= (duration-days (parse-value "+P1W"  'duration)) 7))
  (is (= (duration-days (parse-value "-P1W"  'duration)) 7))
  (is (= (duration-days (parse-value "PT0H" 'duration))  0)))

(test parse-value-duration-002
  "Parse some wrong duration strings."
  (signals error (parse-value "P1H"    'duration))
  (signals error (parse-value "PT1H3S" 'duration))
  (signals error (parse-value "PT1H3S" 'duration)))

;;; test-types-duration.lisp ends here
