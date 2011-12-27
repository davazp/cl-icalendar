;;                                                               -*- Lisp -*-
;; cl-icalendar.asd --
;;
;; Copyright (C) 2010 David Vazquez
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

(defpackage :cl-icalendar-system
  (:use :cl :asdf))

(in-package :cl-icalendar-system)

(declaim (optimize (speed 0) (safety 3) (space 0) (debug 3)))

(defsystem :cl-icalendar
  :name "iCalendar library"
  :license "GPLv3+"
  :version "0.0"
  :depends-on (:trivial-gray-streams :cl-base64 :uuid :closer-mop :flexi-streams)
  :serial t
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   ;; Source code
   (:file "package")
   (:file "utils")
   (:file "error")
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
   (:file "components")
   (:file "components-standard")
   (:file "cl-icalendar")
   ;; Documentation
   (:module "doc"
            :components
            ((:static-file "Makefile")
             (:doc-file "cl-icalendar.texi")
             (:doc-file "fdl.texi")
             (:doc-file "version.texi")))))

(defmethod perform ((op test-op) (c (eql (find-system :cl-icalendar))))
  (operate 'load-op ':cl-icalendar-tests)
  (operate 'test-op ':cl-icalendar-tests))


(defsystem :cl-icalendar-iterate
  :name "iCalendar library iterate integration"
  :license "GPLv3+"
  :depends-on (:cl-icalendar :iterate)
  :serial t
  :components
  ((:module "iterate"
            :serial t
            :components ((:file "ical-iterate")))))


;; cl-icalendar.asd ends here
