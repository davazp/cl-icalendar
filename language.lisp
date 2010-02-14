;; language.lisp -- See RFC5543 
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

(defun alphanum-char-p (x)
  (and (ascii-p x)
       (or
	(alpha-char-p x)
	(digit-char-p x))))

(defun alphanum-string-p (x &key (start 0) (end (length x)))
  "For characters, chech if they are a ascii letter or a digit, for
string check if it contains only alphanum characters"
  (loop for i from start below end
	do (unless (alphanum-char-p (elt x i))
	     (return nil))
	finally (return t)))

(defun singleton-char-p (char)
  "Return a generalized bool if the char is a sigleton"
  (and
   (ascii-p char)
   (char/= char #\x)
   (char/= char #\X)
   (or
    (digit-char-p char)
    (alpha-char-p char))))

;; Parsing is as follows: We pick one subtag and check if it match the
;; current category, if it does, we assign it and we switch to the
;; next category if that category only accepts one value (like script,
;; but unlike variants).  All subtags should match one category so at
;; end of parsing subtags variable should be nil; if not, the
;; Language-Tag is unvalid and we signal an error.
(defun parse-language (string)
  (let ((subtags (split-sequence:split-sequence #\- string))
	(token)
	(language)
	(extlang)
	(script)
	(region)
	(variants)
	(extension)
	(privateuse))
    (labels
	((check-alpha (str)		; TODO: Implement the checking
	   str)
	 (check-num (str)
	   str)
	 (check-alphanum (str)
	   (if (alphanum-string-p str)
	       str
	       (error "Non alphanum character finded")))
	 (check-length (str)
	   (if (<= 8 (length str))
	       (error
		"Finded a token ~a characters long, maxium is 8"
		(length str)))
	   str)
	 (pop-token ()
	   (setf token (check-length (pop subtags)))))
      (pop-token)
      (cond
	((and (= (length token) 1) 
	      (or (char= (elt token 0) #\X)
		  (char= (elt token 0) #\x)))
	 ;; This Language-Tag begin with a X, so it contain only
	 ;; privateuse subtags, check if they are valid
	 (dolist (i subtags)
	   (check-length i)
	   (check-alphanum i))
	 (values nil subtags))

	(t
	 ;; Parse a langtag
	 (progn 
	   (when (<= 2 (length token) 3)
	     (setf language (check-alpha token)))
	   (pop-token)
	   (when (<= 2 (length token) 3)
	     (setf extlang (check-alpha token))
	     (pop-token))
	   (when (= (length token) 4)
	     (setf script (check-alpha token))
	     (pop-token))
	   (when (<= 2 (length token) 3)
	     (setf region (check-alpha token))
	     (pop-token))
	   (setf variants
		 (with-collecting
		   (while
		       (cond
			 ((and (<= 5 (length token) 8)
			       (alphanum-string-p token))
			  (collect token)
			  (pop-token))
		
			 ((and (= (length token) 4)
			       (digit-char-p (elt token 0))
			       (alphanum-string-p token :start 1))
			  (collect token)
			  (pop-token))

			 (t nil)))))	; Finish the loop

	   ;; TODO: Parse the extension
	   
	   (values (list language extlang script region variants)
		   privateuse)))))))

;; language.lisp ends here
