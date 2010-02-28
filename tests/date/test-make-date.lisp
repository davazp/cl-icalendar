;; test-make-date.lisp
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

;;; test-make-date.lisp ends here