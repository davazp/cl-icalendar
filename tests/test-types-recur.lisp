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
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 03 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 05 01 2000 00 00 00)))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 02 01 2000 00 00 00))))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 04 01 2000 00 00 00))))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 06 01 2000 00 00 00)))))

(test recur-instance-p-interval-002
  "Check INTERVAL rule."
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 15 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 29 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                        (ical::encode-datetime 12 02 2000 00 00 00)))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 08 01 2000 00 00 00))))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 02 01 2000 00 00 00))))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 22 01 2000 00 00 00))))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=WEEKLY;INTERVAL=2" 'recur)
                             (ical::encode-datetime 05 02 2000 00 00 00)))))

(test recur-parser-interval-p-003
  "Check intervals"
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=YEARLY;INTERVAL=4;UNTIL=20160101T010101" 'recur)
                             (ical::encode-datetime 01 01 2009 00 00 00))))
  (is (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                        (parse-value "FREQ=MONTHLY;INTERVAL=6;COUNT=18" 'recur)
                        (ical::encode-datetime 01 07 2008 00 00 00))))

(test recur-instance-p-bymonth
  "Check BYMONTH rule."
  (is (recur-instance-p (ical::encode-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                        (ical::encode-datetime 01 01 2000 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                        (ical::encode-datetime 31 01 2000 00 00 00)))
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;BYMONTH=1" 'recur)
                             (ical::encode-datetime 01 02 2000 00 00 00))))
  (is (recur-instance-p (ical::encode-datetime 1 1 2000 00 00 00)
                        (parse-value "FREQ=YEARLY;BYMONTH=1,2" 'recur)
                        (ical::encode-datetime 01 02 2000 00 00 00))))

(test recur-parser-complex-p-001
  "Check parser for some complex recurrency values and other errors"
  (is (not (recur-instance-p (ical::encode-datetime 01 01 2000 00 00 00)
                             (parse-value "FREQ=DAILY;UNTIL=20121212T230000;BYDAY=MO,TH,SA;BYMONTH=1,3,5,7,9,11" 'recur)
                             (ical::encode-datetime 12 12 2012 00 00 00))))
  (signals error (parse-value "FREQ=MONTHLY;COUNT=20 ;UNTIL=20240812T230000;BYDAY=FR" 'recur))
  (signals error (parse-value "FREQ=DAILY;INTERVAL=4;?UNTIL=20121212T000000" 'recur))
  (signals error (parse-value "INTERVAL=4;COUNT=8" 'recur)))

(test recur-parser-complex-p-002
  "Check parser for some complex syntax errors."
  (signals error (parse-value "FREQ=MONTLY" 'recur))
  (signals error (parse-value "FREQ=DAILY; INTERVAL=8" 'recur))
  (signals error (parse-value "FREQ=_DAILY" 'recur))
  (finishes (parse-value "FREQ=WEEKLY;INTERVAL=2;UNTIL=20121212" 'recur)))

(test recur-complex-sign-p-001
  "Check dates with signs"
  (is (recur-instance-p (ical::encode-datetime 28 06 2010 00 00 00)
                        (parse-value "FREQ=MONTHLY;INTERVAL=2;BYDAY=-1MO" 'recur)
                        (ical::encode-datetime 30 8 2010 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 11 06 2010 00 00 00)
                        (parse-value "FREQ=MONTHLY;BYDAY=-1MO,-2TH,-3FR" 'recur)
                        (ical::encode-datetime 17 06 2010 00 00 00)))
  (is (recur-instance-p (ical::encode-datetime 31 12 2010 00 00 00)
                        (parse-value "FREQ=YEARLY;BYDAY=-1MO,-1FR" 'recur)
                        (ical::encode-datetime 29 12 2014 00 00 00))))

;;; test-types-recur.lisp ends here
