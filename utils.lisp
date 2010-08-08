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
  (defun symbolize (symbol1 symbol2)
    (intern (concatenate 'string (string symbol1) (string symbol2)))))

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

;;; The famous macro once-only macro.
;;; FIXME: How ugly is! Write a more beautiful implementation. This would be both
;;; more verbose, and clearer.
(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))

;;; TODO: Document me!
(defmacro with-collectors ((&rest names) &body code)
  (let ((names (mapcar #'mklist names))
        ;; A list of lists of the form (NAME INITFORM BEGIN END),
        ;; where BEGIN and END are the gensymed symbols of the first
        ;; and the last cons of the collector. Note we use a special
        ;; header cons.
        (table nil))
    ;; Fill the table 
    (dolist (collector names)
      (destructuring-bind (name &optional initform fname) collector
        (push (list name
                    initform
                    (gensym)
                    (gensym)
                    (or fname (symbolize 'collect- name)))
              table)))
    (macrolet (;; Map through collectors binding NAME INITFORM BEGIN
               ;; and END variables, collecting the results in a list.
               (map* (form)
                 `(loop for (name initform begin end fname)
                        in table
                        collect ,form)))
      ;; Macroexpansion
      `(let ,(map* `(,begin (cons :collector ,initform)))
         (let ,(map* `(,end (last ,begin)))
           (symbol-macrolet ,(map* `(,name (cdr ,begin)))
             (flet ,(map* `(,fname (value)
                                   (setf (cdr ,end) (list value))
                                   (setf ,end (cdr ,end))))
               ,@code)))))))

;;; TODO: Document me!
(defmacro with-collect (&body code)
  (with-gensyms (name)
    `(with-collectors ((,name nil collect))
       ,@code
       ,name)))


;;; Iteration form is based in Scheme named-let. It bounds NAME to a
;;; local function which ARG arguments and body CODE. ARGS is like an
;;; ordinary lambda form, but the required arguments are specified as
;;; a two-list, where the first element is the variable name and the
;;; second is the initial value.
(defmacro iteration (name args &body code)
  (let* ((required-args-count
          ;; Number of required arguments
          (loop for keyword in lambda-list-keywords
                for pos = (position keyword args)
                when pos
                minimizing pos into aux
                finally (return (if (null pos) (length args) aux))))
         ;; List of required arguments
         (required-args (subseq args 0 required-args-count))
         ;; List of optional arguments
         (non-required  (nthcdr required-args-count args)))
    ;; Check extended lambda form is well formed.
    (dolist (arg required-args)
      (unless (= (length arg) 2)
        (error "Iterate form ill-formed.")))
    (let* ((required-args-names (mapcar #'first required-args))
           (initial-arg-values (mapcar #'second required-args))
           (function-lambda (append required-args-names non-required)))
      `(labels ((,name ,function-lambda
                  (block nil ,@code)))
         (,name ,@initial-arg-values)))))

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
  `(if (not (position ,item ',list :test ,test))
       (error "Not a member of the specified list")))

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
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2)
                (concatenate 'string s1 s2))
              strings)))

;;; Concatenate the list of STRINGS.
(defun join-strings (strings &optional (separator #\space))
  (if (null strings)
      (make-string 0)
      (reduce (lambda (s1 s2)
                (concatenate 'string s1 (string separator) s2))
              strings)))

;;; Check if there is duplicated elements in LIST. KEY functions are
;;; applied to elements previosly. The elements are compared by TEST
;;; function.
(defun duplicatep (list &key (test #'eql) (key #'identity))
  (and (loop for x on list
             for a = (funcall key (car x))
             for b = (cdr x)
             thereis (find a b :key key :test test))
       t))

;;; Return the list of elements which appear in preorder in a tree. If
;;; LIMIT is non-nil, it is a non-negative integer which specify the
;;; deepest level in the tree. Otherwise, it falls until atom
;;; elements.
(defun flatten (tree &optional limit)
  (declare (list tree) (type (or unsigned-byte null) limit))
  (iteration looping ((x tree) (k limit))
    (cond
      ((null x) '())
      ((atom x) (list x))
      ((consp x)
       (if (and limit (zerop k))
           x
           (append (looping (car x) (and limit (1- k)))
                   (looping (cdr x) k)))))))


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

;;; delete-nth is as remove-nth but it could modify the list.
;;; 
;;; NOTE: if you want delete the nth element of the value of a
;;; variable V, you should use '(setf v (delete-nth n v))', indeed of
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

;;;; Streams
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
         ;; Check if CH is not an expected char
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


;;;; Comparators

;;; Like `char=' but it is case-insensitive.
(defun char-ci= (char1 char2)
  (declare (character char1 char2))
  (char= (char-upcase char1)
         (char-upcase char2)))

;;; Like `string=' but it is case-insensitive.
(defun string-ci= (str1 str2)
  (declare (string str1 str2))
  (and (= (length str1) (length str2))
       (every #'char-ci= str1 str2)))

;;; Check if X and Y are not eq.
(definline neq (x y)
  (not (eq x y)))

;;; Anaphoric IF.
(defmacro aif (condition then &optional else)
  `(let ((it ,condition))
     (if it ,then ,else)))


;;;; setf-based

;;; The multiple-setf macro was written by Mario Castelán. It is a
;;; beautiful form to support multiple places in zerof and nilf.
(defmacro multiple-setf (value &rest places)
  (with-gensyms (tmp)
    `(let ((,tmp ,value))
       ,@(loop for place in places
               collect `(setf ,place ,tmp)))))

;;; Set PLACES to 0
(defmacro zerof (&rest places)
  `(multiple-setf 0 ,@places))

;;; Set PLACE to nil.
(defmacro nilf (&rest places)
  `(multiple-setf nil ,@places))

;;; (modf place N) set place to (mod place N)
(define-modify-macro modf (n) mod)


;;;; Others

;;; Like `parse-integer' but it is not allowed to have a sign (+\-).
(defun parse-unsigned-integer (string &rest keyargs &key (start 0) end &allow-other-keys)
  (unless (or (eql start end) (digit-char-p (elt string start)))
    (error "~w is not an unsigned integer." string))
  (apply #'parse-integer string keyargs))

;;; Integer division
(definline idiv (a b)
  (declare (integer a b)
           (optimize speed))
  (values (truncate a b)))

;;; Check if N divides to M.
(definline divisiblep (m n)
  (declare (fixnum m n) (optimize speed))
  (zerop (mod m n)))

;;; Set PLACE to zero.
;;; This function is thought to use this function as default-value in
;;; optional or keyword arguments.
(defun required-arg ()
  (error "A required &KEY or &OPTIONAL argument was not supplied."))

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


(definline mod7 (n)
  (declare (optimize speed))
  (declare (fixnum n))
  (mod n 7))

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

;;; Check if CLASS1 is a superclass of CLASS2.
(defun superclassp (class1 class2)
  (subclassp class2 class1))


;;; utils.lisp ends here
