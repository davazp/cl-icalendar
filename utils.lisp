;; utils.lisp
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

(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it ,then ,else)))

(defmacro zerof (place)
  `(setf ,place 0))

(defmacro nilf (place)
  `(setf ,place nil))

(defmacro while (condition &body code)
  `(do ()
       ((not ,condition))
     ,@code))

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars collect `(,i (gensym)))
     ,@code))

(defmacro with-collecting (&body code)
  (with-gensyms (collected tail)
    `(let* ((,collected (list '#:collect))
            (,tail ,collected))
       (flet ((collect (x)
                (setf (cdr ,tail) (list x))
                (setf ,tail (cdr ,tail))))
         ,@code)
       (cdr ,collected))))

(defmacro define-transitive-relation (name (arg1 arg2) &body body)
  (with-gensyms (argsvar)
    `(defun ,name (&rest ,argsvar)
       ;; SBCL fails here if we use (block nil ...) indeed of progn.
       (loop for (,arg1 ,arg2) on ,argsvar
             while ,arg2
             always (progn ,@body)))))


(defun strip-if (func seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position-if func seq rest)))

(defun strip (x seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position x seq rest)))

(defun read-until (stream char-bag &optional (not-expect "") (eof-error-p t))
  (flet (;; Check if CH is a terminal char
         (terminal-char-p (ch)
           (etypecase char-bag
             (character (char= ch char-bag))
             (sequence  (find ch char-bag :test #'char=))
             (function  (funcall char-bag ch))))
         ;; Check if CH is not a expected char
         (not-expect-char-p (ch)
           (etypecase not-expect
             (character (char= ch not-expect))
             (sequence (find ch not-expect :test #'char=))
             (function (funcall not-expect ch)))))
    ;; Read characters
    (with-output-to-string (out)
      (loop for ch = (peek-char nil stream eof-error-p)
            until (and (not eof-error-p) (null ch))
            until (terminal-char-p ch)
            when (not-expect-char-p ch)
            do (error "Character ~w is not expected." ch)
            do (write-char (read-char stream) out)))))


(defmacro definline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))


(defun split-string (string &optional (separators " ") (omit-nulls t))
  (declare (type string string))
  (flet ((separator-p (char)
           (etypecase separators
             (character (char= char separators))
             (sequence  (find char separators))
             (function  (funcall separators char)))))
    (loop for start = 0 then (1+ end)
          for end = (position-if #'separator-p string :start start)
          as seq = (subseq string start end)
          unless (and omit-nulls (string= seq ""))
          collect seq
          while end)))


;;; Like `parse-integer' but it is not allowed to have a sign (+\-).
(defun parse-unsigned-integer (string &rest keyargs &key &allow-other-keys)
  (unless (or (zerop (length string))
              (digit-char-p (elt string 0)))
    (error "~w is not an unsigned integer." string))
  (apply #'parse-integer string keyargs))


;;; Integer division
(definline idiv (a b)
  (declare (integer a b))
  (values (truncate a b)))

(definline divisiblep (m n)
  (declare (integer m n))
  (zerop (mod m n)))

(defun check-length (seq length &optional (msg "Bad length for sequence"))
  (if (= (length seq) length)
      t
      (error msg)))

;;; utils.lisp ends here
