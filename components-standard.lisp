;; components-standard.lisp --- Standard iCalendar's component definitions
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

(defclass standard-component-object ()
  nil
  (:metaclass component-class))

(defclass x-component-object ()
  nil
  (:metaclass component-class))

(defcomponent vcalendar (standard-component-object)
  ((prodid :type text)
   (version :type text)
   (calscale :initform "GREGORIAN")
   (method :type text))
  (:subcomponents vtodo vjournal))

(defcomponent vtodo (standard-component-object)
  nil)

(defcomponent vjournal (standard-component-object)
  nil)


;;; components-standard.lisp ends here
