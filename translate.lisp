;; translate.lisp --- Dictionary between iCalendar names and Lisp symbols
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

(defvar *translate-table* *standard-vendor*)

(defun translate (entity kind)
  (let ((kindtable (gethash kind *translate-table*)))
    (if kindtable
        (gethash (string-upcase entity) kindtable)
        (values nil nil))))

(defun set-translate (entity kind value)
  (let ((table *translate-table*))
    (let ((kindtable
           (or (gethash kind table)
               (setf (gethash kind table) (make-hash-table :test #'equal)))))
      (setf (gethash (string-upcase entity) kindtable) value))))

(defsetf translate (entity kind) (value)
  `(set-translate ,entity ,kind ,value))

;;; translate.lisp ends here
