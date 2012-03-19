;; folding.lisp
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

(defconstant +tab-character+ (code-char #x09))

(defconstant +content-line-max-length+ 75)

(defclass folding-stream (fundamental-character-input-stream
                          fundamental-character-output-stream)
  ((column-octets
    :initform 0
    :type fixnum
    :reader folding-column-octets)
   (backend-stream
    :initform (required-arg)
    :type stream
    :initarg :stream
    :reader folding-backend-stream)))

(defun make-folding-stream (stream)
  (make-instance 'folding-stream :stream stream))

(defmacro with-folding-stream ((var stream) &body code)
  `(with-open-stream (,var (make-folding-stream ,stream))
     ,@code))

(defun linear-whitespace-p (character)
  (or (char= character #\space)
      (char= character +tab-character+)))

(defmethod stream-read-char ((stream folding-stream))
  (with-slots (backend-stream) stream
    (let ((character (read-char backend-stream)))
      (cond
        ((eq character :eof) :eof)
        ;; #\return #\newline => #\newline
        ((char= character #\return)
         (when (eql (peek-char nil backend-stream nil) #\newline)
           (stream-read-char stream)))
        ((and (char= character #\newline)
              (linear-whitespace-p
               (peek-char nil backend-stream nil #\A)))
         ;; Skip the newline from folding algorithm and go on.
         (read-char backend-stream)
         (read-char backend-stream))
        (t
         character)))))

(defmethod stream-unread-char ((stream folding-stream) character)
  (unread-char character (folding-backend-stream stream)))

;;; This method is redundant as we have defined stream-read-char, but
;;; we provide it due to performance reasons.
(defmethod stream-read-line ((stream folding-stream))
  (let ((backend (folding-backend-stream stream))
        (output (make-string-output-stream))
        line missing-newline-p
        (finishp nil))
    (while (not finishp)
      (multiple-value-setq (line missing-newline-p) (read-line backend nil))
      (write-string (string-right-trim (list #\return) line) output)
      (if (linear-whitespace-p (peek-char nil backend nil #\A))
          (read-char backend)
          (setq finishp t)))
    (values (get-output-stream-string output) missing-newline-p)))

(defmethod stream-write-char ((stream folding-stream) character)
  (with-slots (column-octets backend-stream) stream
    (let* ((encoded (babel:string-to-octets
                     (if (char= character #\newline)
                         #.(coerce '(#\return #\linefeed) 'string)
                         (string character))))
           (size (length encoded)))
         (when (> (+ column-octets size) +content-line-max-length+)
           (stream-write-char stream #\newline)
           (stream-write-char stream #\space)
           (zerof column-octets))
         (if (char= character #\linefeed)
             (zerof column-octets)
             (incf column-octets size))
         (write-sequence encoded backend-stream))))

(defmethod stream-line-column ((stream folding-stream))
  (stream-line-column (folding-backend-stream stream)))

(defmethod stream-finish-output ((stream folding-stream))
  (finish-output (folding-backend-stream stream)))

(defmethod stream-force-output ((stream folding-stream))
  (force-output (folding-backend-stream stream)))

(defmethod stream-clear-output ((stream folding-stream))
  (clear-output (folding-backend-stream stream)))

(defmethod close ((stream folding-stream) &key abort)
  (close (folding-backend-stream stream) :abort abort))


;;; folding.lisp ends here
