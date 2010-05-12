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

;;; Anaphoric IF.
(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it ,then ,else)))

;;; Set PLACE to zero.
(defmacro zerof (place)
  `(setf ,place 0))

;;; Set PLACE to nil.
(defmacro nilf (place)
  `(setf ,place nil))

;;; Execute CONDITION and CODE in order while CONDITION.
(defmacro while (condition &body code)
  `(do ()
       ((not ,condition))
     ,@code))

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars
	       collect (etypecase i
			 (symbol `(,i (gensym ,(symbol-name i))))
			 (list `(,(first i) (gensym ,(second i))))))
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

;; TODO: Enhanche this with a optional finally section, mantaning
;; backward compatibility is not need
(defun %do-sequence (function sequence &key (start 0) end)
  (etypecase sequence
    (list
       (if (not end)
           (loop for x in (nthcdr start sequence) do (funcall function x))
           (loop for x in (nthcdr start sequence)
                 for i from start below end
                 do (funcall function x))))
    (sequence
       (loop for i from start below (or end (length sequence))
             for x = (elt sequence i)
             do (funcall function x)))))

;;; Iterate for the elements of a sequence for side efects, from the
;;; START position element until the END position element. If END is
;;; omitted, then it iterates for all elements of sequence.
(defmacro do-sequence ((var sequence &key (start 0) end) &body body)
  (declare (symbol var))
  `(%do-sequence (lambda (,var) ,@body)
                 ,sequence
                 :start ,start
                 :end ,end))

;;; Define a variable-arity transitive predicate from a body which
;;; define a transtivie relation of arity 2. The body is contained in
;;; an implicit block.
(defmacro define-transitive-relation (name (arg1 arg2) &body body)
  (with-gensyms (argsvar)
    `(defun ,name (&rest ,argsvar)
       (loop for (,arg1 ,arg2) on ,argsvar
             while ,arg2
             always
             (block nil
               ((lambda () ,@body)))))))

;;; Return a fresh copy subsequence of SEQ bound from 0 until the
;;; first element what verifies the FUNC predicate.
(defun strip-if (func seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position-if func seq rest)))

;;; Return a fresh copy subsequence of SEQ bound from 0 until the
;;; position of X in sequence.
(defun strip (x seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position x seq rest)))

;;; Make sure that ITEM is an element of LIST, otherwise this function
;;; signals an simple-error condtion.
(defmacro check-member (item list &key (test #'eql))
  `(if (not (position ,item ',list :test ,test))
       (error "Not a member of the specified list")))

;;; Like `some', but it works on bound sequences
(defun some* (predicate sequence &key (start 0) end)
  (do-sequence (item sequence :start start :end end)
    (when (funcall predicate item)
      (return-from some* t)))
  nil)

;;; Read characters from STREAM until it finds a char of CHAR-BAG. If
;;; it finds a NON-EXPECT character, it signals an error. If an end of
;;; file condition is signaled and EOF-ERROR-P is nil, return nil.
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

;;; Like `defun' but declare the function as inline.
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

;;; Concatenate the list of STRINGS.
(defun join-strings (strings &optional (separator #\space))
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2)
                (concatenate 'string s1 (string separator) s2))
              strings)))

;;; Like `parse-integer' but it is not allowed to have a sign (+\-).
(defun parse-unsigned-integer (string &rest keyargs &key &allow-other-keys)
  (unless (or (zerop (length string))
              (digit-char-p (elt string 0)))
    (error "~w is not an unsigned integer." string))
  (apply #'parse-integer string keyargs))


;;; Integer division
(definline idiv (a b)
  (declare (integer a b)
           (optimize speed))
  (values (truncate a b)))

;;; Check if M divides to N.
(definline divisiblep (m n)
  (declare (integer m n))
  (zerop (mod m n)))

;;; Check if there is duplicated elements in LIST. KEY functions are
;;; applied to elements previosly. The elements are comparaed by TEST
;;; function.
(defun duplicatep (list &key (test #'eql) (key #'identity))
  (and (loop for x on list
             for a = (funcall key (car x))
             for b = (cdr x)
             thereis (find a b :key key :test test))
       t))

;;; Like `char=' but is it case-insensitive.
(defun char-ci= (char1 char2)
  (declare (character char1 char2))
  (char= (char-upcase char1)
         (char-upcase char2)))

;;; Like `string=' but it is case-insensitive.
(defun string-ci= (str1 str2)
  (declare (string str1 str2))
  (and (= (length str1) (length str2))
       (every #'char-ci= str1 str2)))

;;; Define a predicate named NAME in order to check if an object is a
;;; object TYPE. If NAME is omitted, NAMEP is used.
(defmacro define-predicate-type (type &optional name)
  (declare (type (or symbol null) name))
  (let ((fname (or name (intern (format nil "~aP" type)))))
    `(defun ,fname (x)
       (typep x ',type))))

;;; (modf place N) set place to (mod place N)
(define-modify-macro modf (n) mod)

;;; This function is thought to use this function as default-value in
;;; optional or keyword arguments.
(defun required-arg ()
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

;;; Check if X and Y are not eq.
(definline neq (x y)
  (not (eq x y)))

;;; Mark a function as deprecated. When FUNCTION is called, it signals
;;; a simple warning. If REPLACEMENT is given, it will recommend to
;;; use REPLACEMENT indeed.
;;; 
;;; FUNCTION and REPLACEMENT are symbols.
(defmacro deprecate-function (function &body ignore &key replacement)
  (declare (ignore ignore))
  (declare (symbol function replacement))
  `(define-compiler-macro ,function (&whole form &rest args)
     (declare (ignore args))
     (warn "Function ~a is deprecated. ~@[Use ~a indeed.~]"
           ',function ',replacement)
     form))

;;; Read from a stream and write the content to other one.
(defun copy-stream (from to &key (element-type t))
  (let ((buffer (make-array 1024 :element-type element-type)))
    (loop for nbytes = (read-sequence buffer from)
          until (zerop nbytes)
          do (write-sequence buffer to :end nbytes))))

;;; utils.lisp ends here
