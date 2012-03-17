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

;;; Bidirectional mapping.
(defclass translate-table ()
  ((ical>lisp :initform (make-hash-table :test #'equal))
   (lisp>ical :initform (make-hash-table))))

;;; Table of translation tables indexed by entity.
(defvar *translation-tables*
  (make-hash-table :test #'eq))

(defun intern-translation-table (entity)
  (or (gethash entity *translation-tables*)
      (setf (gethash entity *translation-tables*)
            (make-instance 'translate-table))))

;;; Public functions

;;; ENTITY stands for any Lisp object which is used to specify a
;;; namespaces. Two ENTITY are equivalent if they are EQ.

;;; Register a translation between the Lisp value OBJECT and iCalendar
;;; name ICALNAME in the ENTITY namespace.
(defun register-translation (object icalname entity)
  (with-slots (ical>lisp lisp>ical)
      (intern-translation-table entity)
    (let ((icalname (string-upcase icalname)))
      ;; Remove old associations for OBJECT and ICALNAME.
      (let ((oldlisp (gethash icalname ical>lisp))
            (oldical (gethash object lisp>ical)))
        (remhash oldlisp lisp>ical)
        (remhash oldical ical>lisp))
      ;; Set new associations
      (setf (gethash icalname ical>lisp) object)
      (setf (gethash object lisp>ical) icalname)
      (values))))

;;; Translate the iCalendar name ICALNAME to a corresponding Lisp
;;; object in the ENTITY namespace.
(defun translate-to-lisp (icalname entity)
  (with-slots (ical>lisp) (intern-translation-table entity)
    (values (gethash (string-upcase icalname) ical>lisp))))

;;; Translate the Lisp value OBJECT to the corresponding iCalendar
;;; name in the ENTITY namespace.
(defun translate-to-ical (object entity)
  (with-slots (lisp>ical) (intern-translation-table entity)
    (values (gethash object lisp>ical))))


;;; translate.lisp ends here
