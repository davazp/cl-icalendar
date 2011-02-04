;; translate.lisp --- Dictionary between iCalendar names and Lisp objects
;;
;; Copyrigth (C) 2010,2011 David Vázquez
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

(in-package :cl-icalendar)

;;; Translate-table is a couple of syncronized hashtables essentially,
;;; providing a bidirectional indexing.
(defclass translate-table ()
  ((ical-to-lisp-table
    :initform (make-hash-table :test #'equal)
    :reader %ical-to-lisp-table)
   (lisp-to-ical-table
    :initform (make-hash-table)
    :reader %lisp-to-ical-table)))

(defun make-translate-table ()
  (make-instance 'translate-table))

(define-predicate-type translate-table)

(defun register-translation (object icalname translate-table)
  (let ((icalname (string-upcase icalname)))
    (setf (gethash icalname (%ical-to-lisp-table translate-table)) object)
    (setf (gethash object (%lisp-to-ical-table translate-table)) icalname)
    (values)))

(defun translate-to-lisp (icalname translate-table)
  (values (gethash icalname (%ical-to-lisp-table translate-table))))

(defun translate-to-ical (object translate-table)
  (values (gethash object (%lisp-to-ical-table translate-table))))


;;; translate.lisp ends here
