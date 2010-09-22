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

(defun read-params-value (stream)
  (if (char= (peek-char nil stream) #\")
      (prog2 (read-char stream)
          (read-until stream "#\"" #\newline)
        (read-char stream))
      (read-until stream ",;:" #\Newline)))

(defun read-params-values (stream)
  (cons (read-params-value stream)
        (with-collect
          (while (char= (peek-char nil stream) #\,)
            (read-char stream)
            (collect (read-params-value stream))))))

(defun read-params (stream)
  (let ((plist nil)
        (count 0))
    (while (char= (read-char stream) #\;)
      (let ((name (read-until stream "=" (coerce #(#\Newline #\: #\;) 'string))))
        (read-char stream)
        (push (read-params-values stream) plist)
        (push name plist)
        (incf count)))
    (if (null plist)
        nil
        (parameter-table plist))))

(defun read-content-line (stream)
  ;; Skip whitespaces (newlines and spaces) characters.
  (loop for ch = (peek-char nil stream)
        while (or (char= ch #\newline)
                  (char= ch #\space)
                  (char= ch #\tab))
        do (read-char stream))
  (values (read-until stream ";:" #\Newline)
          (read-params stream)
          (read-line stream)))

(defun read-content-line-from-string (string)
  (with-input-from-string (in string)
    (read-content-line in)))

(defun quote-param-value (value)
  (with-output-to-string (stream)
    (let ((quoted
           (or (find #\: value)
               (find #\; value)
               (find #\, value))))
      (when quoted
        (write-char #\" stream))
      (write-string value stream)
      (when quoted
        (write-char #\" stream)))))

(defun write-content-line (name params value stream)
  (declare (stream stream))
  (format stream "~a~{;~a=~{~a~^,~}~}:~a~%" 
          name
          (loop for (param values)
                on (list-parameters (parameter-table params)) by #'cddr
                collect param
                collect (mapcar #'quote-param-value values))
          value))

(defun write-content-line-to-string (name params value)
  (string-right-trim (list #\newline)
                     (with-output-to-string (out)
                       (write-content-line name params value out))))


;; content-line.lisp ends here
