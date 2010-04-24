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

;;; Wrapped character streams

;;; Wrapped streams wrappes a stream (sure?), so read and write to a
;;; stream directly. It is not very useful, but I can inherent new
;;; streams from it easily. Indeed, wrapped-character-stream keeps
;;; some useful information.

(defconstant +tab-character+    (code-char #x09))
(defconstant +return-character+ (code-char #x0D))

(defconstant +content-line-max-length+ 75)

(defclass wrapped-character-stream (fundamental-character-input-stream
                                    fundamental-character-output-stream)
  (
   ;; FIXME: COLUMN should keep the current number of column, but it
   ;; counts the number of characters from the last #\newline, i.e, it
   ;; assumes a character is an octect. The `Flexistreams' library
   ;; supports a lot of encodings, indeed of several newlines.
   (column
    :initform 0
    :accessor wrapped-stream-column)
   (stream
    :initarg :stream
    :reader wrapped-stream)))

(defmethod stream-read-char ((stream wrapped-character-stream))
  (let ((character (read-char (wrapped-stream stream) nil :eof)))
    (cond
      ((eq character :eof)
       :eof)
      (t
       (incf (wrapped-stream-column stream))
       (when (char= character #\Newline)
         (zerof (wrapped-stream-column stream)))
       character))))

(defmethod stream-unread-char ((stream wrapped-character-stream) character)
  (prog1 (unread-char character (wrapped-stream stream))
    (decf (wrapped-stream-column stream))
    (when (char= character #\Newline)
      (zerof (wrapped-stream-column stream)))))

(defmethod stream-write-char ((stream wrapped-character-stream) character)
  (prog1 (write-char character (wrapped-stream stream))
    (incf (wrapped-stream-column stream))
    (when (char= character #\Newline)
      (zerof (wrapped-stream-column stream)))))

(defmethod stream-line-column ((stream wrapped-character-stream))
  (wrapped-stream-column stream))

(defmethod stream-finish-output ((stream wrapped-character-stream))
  (finish-output (wrapped-stream stream)))

(defmethod stream-force-output ((stream wrapped-character-stream))
  (force-output (wrapped-stream stream)))

(defmethod stream-clear-output ((stream wrapped-character-stream))
  (clear-output (wrapped-stream stream)))

(defmethod close ((stream wrapped-character-stream) &key abort)
  (close (wrapped-stream stream) :abort abort))


;;; CRLF/LF Conversion stream. Convert UNIX type line endings to DOS.

(defclass crlf-stream (wrapped-character-stream)
  nil)

(defmethod stream-read-char ((stream crlf-stream))
  (let ((character (call-next-method)))
    (cond
      ((eq character :eof)
       :eof)
      ((char= character +return-character+)
       (if (char= (peek-char nil (wrapped-stream stream) nil #\Space) #\Newline)
           (call-next-method)
           +return-character+))
      (t
       character))))

(defmethod stream-write-char ((stream crlf-stream) character)
  (when (char= character #\Newline)
    (call-next-method stream +return-character+))
  (call-next-method stream character))


;;;; Folding/Unfolding stream

;;; This stream implements the folding/unfolding algorithm described
;;; in the RFC5545 and autochains with a CRLF stream.

(defclass folding-stream (wrapped-character-stream)
  nil)

(defmethod initialize-instance :around ((inst folding-stream)
                                        &rest initargs &key stream &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method inst :stream (make-instance 'crlf-stream :stream stream)))

(defun linear-whitespace-p (character)
  (or (char= character #\Space)
      (char= character +tab-character+)))

(defmethod stream-read-char ((stream folding-stream))
  (let ((character (call-next-method)))
    (cond
      ((eq character :eof)
       :eof)
      ((and (char= character #\Newline)
            (linear-whitespace-p (peek-char nil (wrapped-stream stream) nil #\A)))
       (call-next-method)
       (call-next-method))
      (t
       character))))

(defmethod stream-write-char ((stream folding-stream) character)
  (when (= (stream-line-column stream) +content-line-max-length+)
    (call-next-method stream #\Newline)
    (call-next-method stream #\Space))
  (call-next-method stream character))


(defun make-folding-stream (stream)
  (make-instance 'folding-stream :stream stream))

(defmacro with-folding-stream ((var stream) &body code)
  (check-type var symbol)
  `(let ((,var (make-folding-stream ,stream)))
     (unwind-protect
          (progn ,@code)
       (close ,var))))


;;; folding.lisp ends here
