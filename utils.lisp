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


(defmacro case* (keyform comparator &body cases)
  (with-gensyms (keyform-sym)
    `(let ((,keyform-sym ,keyform))
       (cond
         ,@(loop for i in cases
                 collect (if (eq (car i) t)
                             (cons t (cdr i))
                             `((,comparator ,keyform-sym ,(car i)) ,@(cdr i))))))))


(defmacro define-transitive-relation (name (arg1 arg2) &body body)
  (with-gensyms (argsvar i)
    `(defun ,name (&rest ,argsvar)
       (do ((,i ,argsvar (cdr ,i)))
           ((null (cdr ,i)) t)
         (let ((,arg1 (first  ,i))
               (,arg2 (second ,i)))
           (or (block nil ,@body)
               (return nil)))))))

(defun strip-if (func seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position-if func seq rest)))

(defun strip (x seq &rest rest &key &allow-other-keys)
  (subseq seq 0 (apply #'position x seq rest)))


;;; utils.lisp ends here
