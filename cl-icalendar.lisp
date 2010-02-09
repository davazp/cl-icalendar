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

;;; ANOTACION: Los streams wrapped hacen simplemente de envoltorios
;;; sobre otro stream, de forma que la lectura y escritura se realiza
;;; directamente sobre ese stream. De por sí esto no es útil, pero si
;;; que es útil heredar de un stream de este tipo y especializar
;;; algunos métodos. Así conseguimos crear STREAMS que transforman el
;;; contenido.

(defconstant +tab-character+    (code-char #x09))
(defconstant +return-character+ (code-char #x0D))

(defconstant +content-line-max-length+ 75)

(defclass wrapped-character-stream (fundamental-character-input-stream
                                    fundamental-character-output-stream)
  (
   ;; FIXME: Actualmente COLUMN marca el número de caracteres escritos
   ;; en el stream desde el último #\Newline. Sin embargo esto no es
   ;; correcto, según el RFC ninguna linea debe superar 75
   ;; _octetos_. Y el conjunto de caracteres por defecto es UTF-8, por
   ;; lo que algunos caracteres pueden tener más de un octeto. Hay que
   ;; arreglar este stream para que cuente el número de octetos de
   ;; cada caracter.
   ;;
   ;; Flexstreams parece soportar muchas codificaciones, además de un
   ;; salto de linea configurable. Puede ser una opción..
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


;;; CRLF/LF Conversion stream

;;; Este es un stream que transforma los CRLF leidos en LF, y los LF
;;; escritos en CRLF.

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

;;; Este stream implementa el algoritmo de folding y unfolding
;;; descrito en el RFC. Y se encadena automáticamente con el stream
;;; CRLF. Esto es, al usar la función `make-folding-stream' sobre un
;;; stream, se crea un CRLF stream y sobre ese un folding-stream.  
;;; 
;;; cada línea es una linea de contenido completa del RFC en este stream.

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


;;; Parsing

;;; Parseamos las lineas de contenido como es descrito en el RFC.

(defstruct content-line
  name
  params
  value)



(defun read-params-value (stream)
  (if (char= (peek-char nil stream) #\")
      (prog2 (read-char stream)
          (read-until stream "#\"" +return-character+)
        (read-char stream))
      (read-until stream ",;:" #\Newline)))

(defun read-params-values (stream)
  (cons (read-params-value stream)
        (with-collecting
          (while (char= (peek-char nil stream) #\,)
            (read-char stream)
            (collect (read-params-value stream))))))

(defun read-params (stream)
  (with-collecting
    (while (char= (read-char stream) #\;)
      (let ((name (read-until stream "=" #(#\Newline #\: #\;))))
        (read-char stream)
        (collect (cons name (read-params-values stream)))))))

(defun read-content-line (stream)
  (make-content-line
   :name (read-until stream ";:" #\Newline)
   :params (read-params stream)
   :value (read-line stream)))

;; (defun parse-date (string &optional (date (make-date)) &key (offset 0))
;;   (flet ((~ (x) (+ offset x)))
;;     (setf 
;;      (date-year date) (parse-integer string :start (~ 0) :end (~ 4))
;;      (date-month date) (parse-integer string :start (~ 4) :end (~ 6))
;;      (date-day date) (parse-integer (subseq string 6 8))))
;;   date)

;; (defun parse-time (string &optional (date (make-date)) &key (offset 0))
;;   (flet ((~ (x) (+ offset x)))
;;     (setf 
;;      (date-hour date) (parse-integer string :start (~ 0) :end (~ 2))
;;      (date-minute date) (parse-integer string :start (~ 2) :end (~ 4))
;;      (date-second date) (parse-integer string :start (~ 4) :end (~ 8))))
;;   date)

;; Generación del árbol sintáctico

(defstruct icalendar-block
  name
  items)

(defun read-item (stream)
  "Read a content line, if it is a block (BEGIN:...) read and return
the full block, else only that content line"
  (let ((first-line (read-content-line stream)))
    (if (string= (content-line-name first-line) "BEGIN")
        (make-icalendar-block
         :name (content-line-value first-line)
         :items (loop for item = (read-item stream)
                      until (and (content-line-p item)
                                 (string= (content-line-name item) "END"))
                      collect item))
        first-line)))

(defun search-content-line (tree name)
  "return the first content line with a given name in a sintactic
tree"
  (loop for i in (icalendar-block-items tree)
        if (string= (content-line-name i) name)
        do (return i)))

(defun search-content-lines (tree name)
  "return all content lines with a given name"
  (with-collecting 
    (dolist (i (icalendar-block-items tree))
      (if (string= (content-line-name i) name)
          (collect i)))))

(defun search-content-line-value (tree name)
  "return the value of the first content line with a given name in a
sintactic tree"
  (aif (search-content-line tree name)
       (content-line-value it)))

;; Composition of the sintactic tree in components

(defclass component ()
  ((props-required)
   (props-optional-multi)
   (props-optional-once)
   (properties :initform (make-hash-table :test #'equal :size 16))
   (branches :initform nil)))

(defmethod prop-category ((self component) prop)
  (declare (type string prop))
  (flet ((test (str sym)
           (string= (symbol-name sym) str)))
    (cond
      ((find prop (slot-value self 'props-required) :test #'test) 'required)
      ((find prop (slot-value self 'props-optional-multi) :test #'test) 'optional-multi)
      ((find prop (slot-value self 'props-optional-once) :test #'test) 'optional-once))))

(defmethod getproperty ((self component) propname)
  (gethash propname (slot-value self 'properties)))

(defmethod parse-content-line ((self component) i)
  (let* ((name (content-line-name i))
         (value (content-line-value i))
         (category (prop-category self name)))
    (case category
      ((required optional-once)
       (if (gethash name (slot-value self 'properties))
           (error "Property ~a, type ~a appears twice" name category)
           (push value (gethash name (slot-value self 'properties)))))
      ((optional-multi)
       (push value (gethash name (slot-value self 'properties))))
      (t
       (parse-strange-content-line self i)))))

(defmethod parse-strange-content-line ((self component) prop)
  "Method meant to be overwritted if there is some special
property expected that &required, &optional-multi or &optional-once
don't cover."
  (error "Strange propertiy: ~a" prop))

(defun build-component (tree)
  (declare (type icalendar-block tree))
  (let* ((component-name (icalendar-block-name tree))
         (component (make-instance (intern component-name))))
    (build component tree)
    component))

(defmethod build ((self component) tree &key (recursive-parsing t))
  (dolist (i (icalendar-block-items tree))
    (case (type-of i)
      (content-line
       (parse-content-line self i))
      (icalendar-block
       (when recursive-parsing
         (push (build-component i) (slot-value self 'branches))))
      (error "Tree item is not a valid type: ~a" (type-of i)))))

(defmacro defcomponent (component props-list &key extra-superclasses)
  (labels ((modifier-p (x)
             (char= (elt (symbol-name x) 0) #\&))
           (select (modifier)
             (strip-if #'modifier-p (cdr (member modifier props-list)))))
    (let* ((required (select '&required))
           ;; may appear any times, including 0
           (optional-multi (select '&optional-multi))
           ;; may appear at most one time
           (optional-once (select '&optional-once)))

      `(defclass ,component (,(cons component extra-superclasses))
         ((props-required :initform ',required)
          (props-optional-multi :initform ',optional-multi)
          (props-optional-once :initform ',optional-once))) )))

(defclass time-bound-component (component)
  ())

(defmethod begin-date ((self time-bound-component))
  (getproperty self "DTSTART"))

;; (defmethod end-date ((self time-bound-component))
;;   "Returns the end date of a time bound if DTEND is defined, or
;;   estimate it from DURATION (Leap seconds not considred)"
;;   (or (getproperty self "DTEND")
;;       (date+ (getproperty self "dtstart")
;;              (getproperty self "duration"))))

;; ;;; Iterators TODO

;; (defmacro do-vcal-body ((vcal
;;                          &key
;;                          type
;;                          begin-after
;;                          begin-before
;;                          end-after
;;                          end-before
;;                          &body
;;                          body))
;;   `(dolist (item (slot-value vcal 'branches))
;;      (when ,(cons 'and
;;                   (with-collecting
;;                     (if type
;;                         (collect `(eq (type-of) item ,type)))
;;                     (if begin-after
;;                         (collect `(date>= ,begin-after (begin-date item))))
;;                     (if begin-before
;;                         (collect `(date< ,begin-before (begin-date item))))
;;                     (if end-after
;;                         (collect `(date>= ,end-after (end-date item))))
;;                     (if end-before
;;                         (collect `(date< ,end-before (end-date item))))))
;;        body)))



;; (defcomponent vcalendar
;;     (&required
;;      prodid
;;      version
     
;;      &optional-once
;;      calscale
;;      method
     
;;      &optional-once
;;      xprop
;;      iana-prop))

;; (defcomponent vevent
;;     (&required
;;      dtstamp
;;      uid
     
;;      &optional-once
;;      dtstart
;;      class
;;      created
;;      description
;;      geo
;;      last-mod
;;      location
;;      organizer
;;      priority
;;      seq
;;      status
;;      summary
;;      transp
;;      url
;;      recurid
     
;;      &optional-multi
;;      rrule ; optional but SHOULD appear only once at most
;;      attach
;;      attendee
;;      categories
;;      comment
;;      contact
;;      exdate
;;      rstatus
;;      related
;;      resources
;;      rdate
;;      x-prop
;;      iana-prop))

;; cl-icalendar.lisp ends here
