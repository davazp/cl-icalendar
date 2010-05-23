;; error.lisp --- Error handling machinery
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

(in-package :cl-icalendar)

;;; It is signaled when if an error ocurred while parsing iCalendar
;;; values, properties or components.
(define-condition icalendar-parse-error (simple-error)
  nil)

;;; Signal an icalendar-parse-error condition.
(defun %parse-error (format &rest args)
  (error 'icalendar-parse-error
         :format-control format
         :format-arguments args))

;;; error.lisp ends here
