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

(defsystem :cl-icalendar
  :name "iCalendar library"
  :license "GPLv3+"
  :depends-on (:trivial-gray-streams :cl-base64 :uuid)
  :serial t
  :components
  ((:static-file "COPYING")
   (:static-file "README")
   (:file "package")
   (:file "utils")
   (:file "error")
   (:file "types")
   (:file "types-duration")
   (:file "types-recur")
   (:file "folding")
   (:file "content-line")
   (:file "properties")
   (:file "components")
   (:file "cl-icalendar")))

;; cl-icalendar.asd ends here
