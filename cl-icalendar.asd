;;                                                               -*- Lisp -*-
;; cl-icalendar.asd --
;;
;; Copyright (C) 2010, 2012 David Vazquez
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
;;

(defsystem :cl-icalendar
  :name "iCalendar library"
  :license "GPLv3+"
  :version "0.0"
  :depends-on (:trivial-gray-streams :cl-base64 :uuid :babel)
  :serial t
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   ;; Source code
   (:file "package")
   (:file "utils")
   (:file "conditions")
   (:file "translate")
   (:file "parameters")
   (:file "types")
   (:file "types-binary")
   (:file "types-utc-offset")
   (:file "types-text")
   (:file "universal-time")
   (:file "types-date")
   (:file "types-time")
   (:file "types-datetime")
   (:file "types-duration")
   (:file "types-period")
   (:file "types-recur")
   (:file "folding")
   (:file "content-line")
   (:file "property")
   (:file "components")
   (:file "cl-icalendar")
   ;; Documentation
   (:module "doc"
            :components
            ((:static-file "Makefile")
             (:doc-file "cl-icalendar.texi")
             (:doc-file "fdl.texi")
             (:doc-file "version.texi"))))

  :in-order-to ((test-op (load-op cl-icalendar-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN-TESTS" (find-package :cl-icalendar-tests)))))


(defsystem :cl-icalendar-tests
  :name "iCalendar library tests"
  :license "GPLv3+"
  :depends-on (:cl-icalendar :fiveam)
  :serial t
  :components
  ((:module "tests"
            :serial t
            :components
            ((:static-file "test-types.001")
             (:static-file "test-icalendar.001")
             (:static-file "test-icalendar.002")
             (:static-file "test-icalendar.003")
             (:static-file "test-icalendar.004")
             (:static-file "test-icalendar.005")
             (:static-file "test-icalendar.006")
             (:file "package")
             (:file "tsuite")
             (:file "test-types")
             (:file "test-types-date")
             (:file "test-types-recur")))))


;; cl-icalendar.asd ends here
