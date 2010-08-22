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
  (:use :cl :asdf)
  (:export #:release))

(in-package :cl-icalendar-system)

(defsystem :cl-icalendar
  :name "iCalendar library"
  :license "GPLv3+"
  :version "0.0"
  :depends-on (:trivial-gray-streams
               :cl-base64
               :uuid
               :closer-mop
               :flexi-streams)
  :serial t
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   ;; Source code
   (:file "package")
   (:file "utils")
   (:file "error")
   (:file "vendor")
   (:file "types")
   (:file "types-binary")
   (:file "types-utc-offset")
   (:file "types-text")
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


(defsystem :cl-icalendar-tests
  :name "iCalendar library tests"
  :license "GPLv3+"
  :depends-on (:cl-icalendar :fiveam)
  :serial t
  :components
  ((:module "tests"
            :serial t
            :components
            ((:file "package")
             (:file "tsuite")
             (:static-file "test-types.001")
             (:file "test-types")
             (:file "test-types-date")
             (:file "test-types-recur")))))

(defmethod perform ((op test-op) (c (eql (find-system :cl-icalendar))))
  (operate 'load-op ':cl-icalendar-tests)
  (operate 'test-op ':cl-icalendar-tests))

(defmethod perform ((op test-op) (c (eql (find-system :cl-icalendar-tests))))
  (funcall (intern "RUN-TESTS" (find-package :cl-icalendar-tests))))


;;;; Release tool

;;; Create a tarball of cl-icalendar. This is inspired by
;;; asdf-packaging-tools but allow packing of several systems.

(defclass copy-op (operation)
  ((target-directory
    :initarg :directory
    :reader target-directory))
  (:default-initargs :force t))

(defun run-safe-shell-command (format &rest args)
  (unless (zerop (apply #'run-shell-command format args))
    (error 'shell-command-error 
	   :format-string format
	   :format-args args)))

(defun copy-file (from to)
  (run-safe-shell-command "install -D -m 644 '~A' '~A'" from to))

(defun delete-directory (directory)
  (run-safe-shell-command "rm -rf '~A'" directory))

(defun create-tarball (directory output)
  (format t "creating tarball ~a..." output)
  (finish-output)
  (run-safe-shell-command "tar zcvf '~a' '~a'" output directory)
  (format t "done~%"))

(defmethod perform ((op copy-op) (c source-file))
  (let ((source (enough-namestring (component-pathname c)))
        (target (enough-namestring (component-pathname c))))
    (copy-file source (format nil "~a~a" (target-directory op) target))))

(defmethod perform ((op copy-op) (c system))
  (let ((source (enough-namestring (component-pathname c)))
        (target (enough-namestring (component-pathname c))))
    (copy-file source (format nil "~a~a" (target-directory op) target))))

(defun package-string (system)
  (format nil "~a-~a"
          (component-name system)
          (component-version system)))

(defun release ()
  (let* ((system1 (find-system :cl-icalendar))
         (system2 (find-system :cl-icalendar-tests))
         (pkgstring (package-string system1))
         (output (make-pathname :directory (list :relative pkgstring))))
    (ensure-directories-exist output :verbose t)
    (oos 'copy-op system1 :directory output)
    (oos 'copy-op system2 :directory output)
    (create-tarball output (format nil "~a.tar.gz" pkgstring))
    (delete-directory output)
    nil))

;; cl-icalendar.asd ends here
