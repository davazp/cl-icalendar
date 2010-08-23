#!/usr/local/bin/sbcl --script
;;                                                              -*- Lisp -*-
;; 
;; release.lisp --- Create a tarball of the cl-icalendar library
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

;;; If you want to edit this script, put (pushnew :interactive
;;; *features*) in your .sbclrc script, in order to load this file in
;;; your Lisp image.

(eval-when (:compile-toplevel :load-toplevel :execute)
 (require :asdf)
 (require :cl-icalendar)
 (require :cl-icalendar-tests))

(defpackage :release
  (:use :cl :asdf :cl-icalendar-system))

(in-package :release)

;;; Create a tarball of cl-icalendar. This is inspired by
;;; asdf-packaging-tools but allow packing of several systems.
(defclass copy-op (operation)
  ((target-directory
    :initarg :directory
    :reader target-directory))
  (:default-initargs :force t))

(defun run-safe-shell-command (format &rest args)
  (unless (zerop (apply #'run-shell-command format args))
    (error 'simple-error
	   :format-string format
	   :format-args args)))

(defun copy-file (from to &optional (perms 644))
  (run-safe-shell-command "install -D -m ~A '~A' '~A'" perms from to))

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

(defmethod perform ((op copy-op) (c script-file))
  (let ((source (enough-namestring (component-pathname c)))
        (target (enough-namestring (component-pathname c))))
    (let ((to (format nil "~a~a" (target-directory op) target))
          (perm 744))
      (copy-file source to perm))))

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

#-interactive
(release)

;;; release.lisp ends here
