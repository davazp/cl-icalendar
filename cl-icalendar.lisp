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

;;; Parsing

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
