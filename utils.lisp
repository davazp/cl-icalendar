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

;;;; Misc macros

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Given two string designators, concatenate the strings and intern
  ;; the symbol in a package. By default, current package is used.
  (defun symbolize (string1 string2 &optional (package *package*))
    (intern (concatenate 'string (string string1) (string string2)) package)))

(defmacro until (condition &body code)
  `(do () (,condition) ,@code))

(defmacro while (condition &body code)
  `(until (not ,condition) ,@code))

(defmacro with-gensyms ((&rest vars) &body code)
  `(let ,(loop for i in vars
	       collect (etypecase i
			 (symbol `(,i (gensym ,(symbol-name i))))
			 (list `(,(first i) (gensym ,(second i))))))
     ,@code))

;;; The famous and ugly once-only macro.
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))


;;; Iterate ELEMENT across LIST and collect the the results of
;;; evaluate BODY in a progn.
;;;
;;; for example:                                expands to:
;;;
;;;    (let ((list '(x y z)))                   (progn
;;;       (progn-expand (var list)                 (1+ x)
;;;         `(1+ ,var)))                           (1+ y)
;;;                                                (1+ z))
;;;
(defmacro progn-expand ((element list) &body body)
  ``(progn ,@(loop for ,element in ,list collect (progn ,@body))))


(defmacro with-named-collector% ((accumulator initial collector) &body body)
  (check-type accumulator symbol)
  (check-type collector symbol)
  (let ((head (gensym))
        (tail (gensym)))
    `(let* ((,head (cons :accumulator ,initial))
            (,tail ,head))
       (flet ((,collector (item)
                (let ((c (list item)))
                  (setf (cdr ,tail) c)
                  (setf ,tail c))))
         (symbol-macrolet ((,accumulator (cdr ,head)))
           ,@body)))))

(defmacro with-collect (&body body)
  (let ((accumulator (gensym)))
    `(with-named-collector% (,accumulator nil collect)
       ,@body
       ,accumulator)))

(defmacro with-collectors (names &body body)
  (if (null names)
      `(progn ,@body)
      (destructuring-bind (name
                           &optional
                           initial
                           (collector (intern (format nil "COLLECT-~a" (string name)))))
          (mklist (car names))
        `(with-named-collector% (,name ,initial ,collector)
           (with-collectors ,(cdr names)
             ,@body)))))


;;;; Declarations and definitions facilities

;;; Like `defun' but declare the function as inline.
(defmacro definline (name args &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,args ,@body)))

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

;;; Define a predicate named NAME in order to check if the type of an
;;; object is TYPE. If NAME is omitted, NAMEP is used.
(defmacro define-predicate-type (type &optional name)
  (declare (type (or symbol null) name))
  (let ((fname (or name (intern (format nil "~aP" type)))))
    `(defun ,fname (x)
       (typep x ',type))))

;;; Mark a function as deprecated. When FUNCTION is called, it signals
;;; a simple warning. If REPLACEMENT is given, it will recommend to
;;; use REPLACEMENT instead.
;;;
;;; FUNCTION and REPLACEMENT are symbols.
(defmacro deprecate-function (function &body ignore &key replacement)
  (declare (ignore ignore))
  (declare (symbol function replacement))
  `(define-compiler-macro ,function (&whole form &rest args)
     (declare (ignore args))
     (warn "Function ~a is deprecated. ~@[Use ~a instead.~]"
           ',function ',replacement)
     form))

;;; Define a printer for a object of class CLASS, bound to VAR. The
;;; body of the macro is supposed to write to the standard output
;;; stream.
(defmacro defprinter ((var class) &body code)
  (with-gensyms (stream)
    `(defmethod print-object ((,var ,class) ,stream)
       (print-unreadable-object (,var ,stream :type t)
         (let ((*standard-output* ,stream))
           ,@code)))))

;;;; Sequences

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
  `(unless (find ,item ',list :test ,test)
     (error "Not a member of the specified list")))

;;; Check that the type of every element of LIST is TYPE.
(defmacro check-type-list (list type)
  `(dolist (x ,list)
     (check-type x ,type)))

;;; Like `some', but it works on bound sequences
(defun some* (predicate sequence &key (start 0) end (key #'identity))
  (do-sequence (item sequence :start start :end end)
    (when (funcall predicate (funcall key item))
      (return-from some* t)))
  nil)

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

;;; Concatenate strings.
(defun concat (&rest strings)
  (reduce (lambda (s1 s2) (concatenate 'string s1 s2))
          strings
          :initial-value (make-string 0)))

;;; Concatenate the list of STRINGS.
(defun join-strings (strings &optional (separator #\space))
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2) (concat s1 (string separator) s2)) strings)))

;;; Check if the PREFIX string matches with some substrings in the
;;; beginning of STRING, using TEST to compare the characters.
(defun string-prefix-p (prefix string &key (test #'char=))
  (declare (string string prefix))
  (let ((offset (mismatch prefix string :test test)))
    (if offset
        (= offset (length prefix))
        t)))

;;; Check if there are duplicated elements in LIST. KEY functions are
;;; applied to elements previosly. The elements are compared by TEST
;;; function.
(defun duplicatep (list &key (test #'eql) (key #'identity))
  (let ((list (mapcar key list)))
    (and (loop for (a . b) on list thereis (find a b :test test))
         t)))

;;; Return a list with the nth element of list removed.
(defun remove-nth (n list)
  (let* ((result (cons nil nil))
         (tail result))
    (do ((i 0 (1+ i))
         (l list (cdr l)))
        ((or (= i n) (null l))
         (setf (cdr tail) (cdr l))
         (cdr result))
      (setf (cdr tail) (cons (car l) nil))
      (setf tail (cdr tail)))))

;;; delete-nth is like remove-nth but it could modify the list.
;;;
;;; NOTE: if you want delete the nth element of the value of a
;;; variable V, you should use '(setf v (delete-nth n v))', instead of
;;; '(delete-nth n v)', just as the standard delete function.
(defun delete-nth (n list)
  (declare (type (integer 0 *) n) (list list))
  (if (zerop n)
      (cdr list)
      (let ((tail (nthcdr (1- n) list)))
        (setf (cdr tail) (cddr tail))
        list)))

;;; Return X if it is a list, (list X) otherwise.
(defun mklist (x)
  (if (listp x)
      x
      (list x)))

;;; Check if X is a list of an element.
(defun singlep (x)
  (and (consp x) (null (cdr x))))

;;; If X is a single list, it returns the element. Otherwise, return
;;; the list itself.
(defun unlist (x)
  (if (singlep x)
      (car x)
      x))

;;; Build a circular list
(defun circular (&rest list)
  (let ((result (copy-list list)))
    (nconc result result)))

;;; Map the function arg with the first argument fixed to ARG across
;;; the elements in LIST and LISTS like mapcar.
(defun map1 (function arg list &rest lists)
  (apply #'mapcar function (circular arg) list lists))

;;; Check if X appears before than Y in LIST.
(defun member< (x y list &rest args &key &allow-other-keys )
  (let ((sublist (apply #'member x list args)))
    (and (apply #'member y (cdr sublist) args) t)))

;;;; Streams

;;; Read characters from STREAM until it finds a char of CHAR-BAG. If
;;; it finds a NON-EXPECT character, it signals an error. If an end of
;;; file condition is signaled and EOF-ERROR-P is nil, return nil.
(defun parse (stream char-bag &optional (not-expect "") (eof-error-p t))
  (flet ((in (ch char-bag)
           ;; Check if CH is in CHAR-BAG.
           (etypecase char-bag
             (character (char= ch char-bag))
             (sequence (find ch char-bag))
             (function (funcall char-bag ch)))))
    ;; Read characters
    (with-output-to-string (out)
      (loop for ch = (peek-char nil stream eof-error-p)
            until (and (not eof-error-p) (null ch))
            until (in ch char-bag)
            when (in ch not-expect)
            do (error "Character ~w is not expected." ch)
            do (write-char (read-char stream) out)))))

;;; An optimized version of `parse' specific for calls where
;;; `char-bag' and `not-expected' are simple strings.
(declaim (inline parse-simple))
(defun parse-simple (stream char-bag &optional (not-expect "") (eof-error-p t))
  (declare (optimize speed)
           (simple-string char-bag not-expect)
           (boolean eof-error-p))
  ;; Read characters
  (with-output-to-string (out)
    (loop for ch = (peek-char nil stream eof-error-p)
          until (and (not eof-error-p) (null ch))
          until (find (the character ch) char-bag)
          when (find (the character ch) not-expect)
          do (error "Character ~w is not expected." ch)
          do (write-char (read-char stream) out))))

(define-compiler-macro parse (&whole form stream char-bag &optional (not-expect "") (eof-error-p t))
  (if (and (stringp char-bag) (stringp not-expect))
      `(parse-simple ,stream ,char-bag ,not-expect ,eof-error-p)
      form))


;;;; Comparators

;;; Like `char=' but it is case-insensitive.
(defun char-ci= (char1 char2)
  (declare (character char1 char2))
  (char= (char-upcase char1)
         (char-upcase char2)))

;;; Like `string=' but it is case-insensitive.
(defun string-ci= (str1 str2)
  (and (= (length str1) (length str2))
       (every #'char-ci= str1 str2)))

;;; Check if X and Y are not eq.
(definline neq (x y)
  (not (eq x y)))

;;; Anaphoric IF.
(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it ,then ,else)))

(defmacro second-value (form)
  `(nth-value 1 ,form))


;;;; setf-based

;;; The multiple-setf macro was written by Mario Castelán. It is a
;;; beautiful form to support multiple places in zerof and nilf.
(defmacro multiple-setf (value &rest places)
  (once-only (value)
    `(setf ,@(loop for place in places
                   append `(,place ,value)))))

;;; Set PLACES to 0
(defmacro zerof (&rest places)
  `(multiple-setf 0 ,@places))

;;; Set PLACE to nil.
(defmacro nilf (&rest places)
  `(multiple-setf nil ,@places))

;;; (modf place N) set place to (mod place N)
(define-modify-macro modf (n) mod)

;;; (appendf place list) set place to (append place list)
(define-modify-macro appendf (list) append)

;;;; Others

;;; Like `parse-integer' but it is not allowed to have a sign (+\-).
(defun parse-unsigned-integer (string &rest keyargs &key (start 0) end &allow-other-keys)
  (declare (string string))
  (unless (or (eql start end) (digit-char-p (char string start)))
    (error "~w is not an unsigned integer." string))
  (apply #'parse-integer string keyargs))

;;; Set PLACE to zero.
;;; This function is thought to use this function as default-value in
;;; optional or keyword arguments.
(defun required-arg (&optional fmt &rest args)
  (if fmt
      (error 'simple-error  :format-control fmt :format-arguments args)
      (error "A required &KEY or &OPTIONAL argument was not supplied.")))

(defmacro /debug1 (form)
  (with-gensyms (value)
    `(progn
       (let ((,value ,form))
         (format *error-output* "~%; DEBUG~@[ (~a)~]"
                 (load-time-value (aif *load-pathname* (enough-namestring it))))
         (pprint ',form *error-output*)
         (write-string " ===> " *error-output*)
         (princ ,value *error-output*)
         (terpri *error-output*)
         ,value))))

;;; Run CODE and print information about the evaluation of
;;; CODE. Useful for debugging.
(defmacro /debug (&body code)
  `(progn
     ,@(loop for form in code collect `(/debug1 ,form))))

(defmacro implyp (p q)
  `(if ,p
       (and ,q t)
       t))

(defun range (m n &optional (step 1))
  (loop for i from m to n by step collect i))

(defun curry (fn &rest preargs)
  (lambda (&rest postargs)
    (apply fn (append preargs postargs))))

(defun rcurry (fn &rest postargs)
  (lambda (&rest preargs)
    (apply fn (append preargs postargs))))

;;; Iterate across entries in a hash table.
(defmacro do-hash-table ((key value) hash-table &body code)
  (with-gensyms (iter morep)
    `(with-hash-table-iterator (,iter ,hash-table)
       (loop
        (multiple-value-bind (,morep ,key ,value)
            (,iter)
          (declare (ignorable ,key ,value))
          (unless ,morep (return))
          ((lambda () ,@code)))))))

;;; Return M si 0 < M <= N, and M+N+1 if M<0.
(defun mod* (m n)
  (declare (integer m n))
  (cond
    ((and (< m 0) (<= (- m) n))
     (1+ (+ n m)))
    ((and (< 0 m) (<= m n)) m)
    (t (error "~a should be a integer in [-~a,1] or [~:*~a,1]." m n))))


;;; iCalendar utilities

;;; Check if string starts with "X-" or "x-".
(defun x-name-p (string)
  (string-prefix-p "X-" string :test #'char-ci=))

;;; utils.lisp ends here
