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

(test date-week-of-year-001
  (is (= (week-of-year (encode-date 01 1 2010)) 0))
  (is (= (week-of-year (encode-date 02 1 2010)) 0))
  (is (= (week-of-year (encode-date 03 1 2010)) 0))
  (is (= (week-of-year (encode-date 04 1 2010)) 1))
  (is (= (week-of-year (encode-date 10 1 2010)) 1))
  (is (= (week-of-year (encode-date 11 1 2010)) 2))
  (is (= (week-of-year (encode-date 01 1 2013) :tuesday) 1))
  (is (= (week-of-year (encode-date 07 1 2013) :tuesday) 1))
  (is (= (week-of-year (encode-date 09 1 2013) :tuesday) 2)))

;;; test-types.lisp ends here
