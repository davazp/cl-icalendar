;; types-binary.lisp ---
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

(deftype binary ()
  '(array (unsigned-byte 8)))

(register-ical-value binary)

(defun read-binary-from-stream (stream)
  (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
        (content (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t)))
    ;; Read from STREAM sequences into buffer and fill content. When
    ;; content is exhausted, we will adjust it.
    (loop for index = 0 then (+ index nbytes)
          for nbytes = (read-sequence buffer stream)
          until (zerop nbytes)
          when (< (- (array-dimension content 0) index) nbytes)
          do (adjust-array content (* 2 (array-dimension content 0)))
          do (replace content buffer :start1 index :end1 (+ index nbytes))
          finally (adjust-array content index))
    ;; Finally return a binary instance.
    content))

(defun read-binary-from-file (pathname)
  (with-open-file (in pathname :element-type '(unsigned-byte 8))
    (read-binary-from-stream in)))

(defun write-binary-to-stream (binary stream)
  (write-sequence binary stream)
  (values))

(defun write-binary-to-file (binary pathname &key if-exists)
  (with-open-file (out pathname
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists if-exists)
    (write-binary-to-stream binary out)))

(defun binary-length (binary)
  (length binary))

(progn
  ;; The binary data value must be written as a base64-encoded
  ;; sequence. Therefore, the ENCODING=BASE64 parameter should be
  ;; present. We don't check this here; instead, we trust in the caller
  ;; (property code basically) will do the right thing.
  (defmethod format-value (value (type (eql 'binary)) &optional params)
    (declare (ignore params) (type binary value))
    (base64:usb8-array-to-base64-string value))

  (defmethod parse-value (string (type (eql 'binary)) &optional params)
    (declare (ignore params))
    (base64:base64-string-to-usb8-array string)))


;;; types-binary.lisp
