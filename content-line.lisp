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

(in-package :cl-icalendar)

(defstruct content-line
  name
  params
  value)

(defmethod print-object ((object content-line) stream)
  (print-unreadable-object (object stream :type t)
    (write-string (write-content-line-to-string object) stream)))

(defun read-params-value (stream)
  (if (char= (peek-char nil stream) #\")
      (prog2 (read-char stream)
          (read-until stream "#\"" +return-character+)
        (read-char stream))
      (read-until stream ",;:" #\Newline)))

(defun read-params-values (stream)
  (cons (read-params-value stream)
        (with-collect
          (while (char= (peek-char nil stream) #\,)
            (read-char stream)
            (collect (read-params-value stream))))))

(defun read-params (stream)
  (with-collect
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

(defun write-content-line (content-line stream)
  (declare (content-line content-line) (stream stream))
  (format stream "~a~{;~a=~{~a~}~}:~a~%" 
          (content-line-name content-line)
          (with-collect
            (dolist (entry (content-line-params content-line))
              (collect (car entry))
              (collect (cdr entry))))
          (content-line-value content-line)))

(defun write-content-line-to-string (content-line)
  (string-right-trim (list #\newline)
                     (with-output-to-string (out)
                       (write-content-line content-line out))))

;; content-line.lisp ends here
