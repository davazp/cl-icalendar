;; error.lisp --- Error handling machinery
;;
;; Copyrigth (C) 2010, 2012 David Vázquez
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

(define-condition icalendar-condition ()
  nil)

(define-condition icalendar-error (icalendar-condition error)
  nil)

(define-condition icalendar-type-error (icalendar-error)
  ((expected)
   (found)
   (place)))

(define-condition icalendar-parse-error (icalendar-error)
  ((stream :initarg :stream :reader icalendar-parse-error-stream)
   (line :initarg :line :reader icalendar-parse-error-line)
   (column :initarg :column :reader icalendar-parse-error-column)))

(define-condition icalendar-warning (icalendar-condition warning)
  nil)

(define-condition validation-error (icalendar-error)
  ((component :initarg :component :reader validation-error-component)))

(define-condition missing-property (validation-error)
  ((property-name)))

(define-condition invalid-parameter (validation-error)
  ((property)
   (parameter)))

(define-condition invalid-property (validation-error)
  ((component)
   (property)))

(define-condition invalid-component (validation-error)
  ((component)))


;;; Signal an icalendar-parse-error condition.
(defun %parse-error (format &rest args)
  (error 'icalendar-parse-error
         :format-control format
         :format-arguments args))


(defun ical-error (format &rest args)
  (error 'icalendar-error
         :format-control format
         :format-arguments args))


;;; error.lisp ends here
