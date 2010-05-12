;; content-line.lisp
;;
;; Copyrigth (C) 2009, 2010 David Vázquez
;; Copyrigth (C) 2009, 2010 Mario Castelán Castro <marioxcc>
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

;;; TODO: Support for write content lines.

(defstruct content-line
  name
  params
  value)

(defun read-params-value (stream)
  (if (char= (peek-char nil stream) #\")
      (prog2 (read-char stream)
          (read-until stream "#\"" +return-character+)
        (read-char stream))
      (read-until stream ",;:" #\Newline)))

(defun read-params-values (stream)
  (cons (read-params-value stream)
        (with-collecting
          (while (char= (peek-char nil stream) #\,)
            (read-char stream)
            (collect (read-params-value stream))))))

(defun read-params (stream)
  (with-collecting
    (while (char= (read-char stream) #\;)
      (let ((name (read-until stream "=" #(#\Newline #\: #\;))))
        (read-char stream)
        (collect (cons name (read-params-values stream)))))))

(defun read-content-line (stream)
  (make-content-line
   :name (read-until stream ";:" #\Newline)
   :params (read-params stream)
   :value (read-line stream)))

(defun read-content-line-from-string (string)
  (with-input-from-string (in string)
    (read-content-line in)))

;; content-line.lisp ends here
