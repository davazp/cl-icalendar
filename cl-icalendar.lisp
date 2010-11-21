;; cl-icalendar.lisp
;;
;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
;; Copyrigth (C) 2009, 2010 David Vázquez
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

(defun read-vcalendar (stream &optional (vendor *vendor*))
  (read-component-class 'vcalendar stream vendor))

(defun write-vcalendar (component stream)
  (write-component component stream))

(defun open-vcalendar (pathname)
  (with-open-file (infile pathname :element-type '(unsigned-byte 8))
    (with-folding-stream (in infile)
      (read-vcalendar in))))

(defun save-vcalendar (vcalendar pathname &key if-exists)
  (declare (type vcalendar vcalendar))
  (with-open-file (outfile
                   pathname
                   :element-type '(unsigned-byte 8)
                   :direction :output :if-exists if-exists)
    (with-folding-stream (out outfile)
      (write-vcalendar vcalendar out))))

;; cl-icalendar.lisp ends here
