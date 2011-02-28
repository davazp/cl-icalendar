;; dependant.lisp --- Code implementation dependant
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

;;; Return the number of octets CHAR take up for a stream of the given
;;; EXTERNAL-FORMAT.
(defun character-length (char external-format)
  #+sbcl
  (length (sb-ext:string-to-octets
           (string char)
           :external-format external-format))
  #+clisp (length (ext:convert-string-to-bytes (string char) external-format))
  #-(or sbcl clisp)
  (error "character-length function must be implement for this implementation."))

;; dependant.lisp ends here
